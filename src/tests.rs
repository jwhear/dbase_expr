use std::borrow::Cow;

use crate::{
    codebase_functions::CodebaseFunction,
    parser,
    translate::{
        self, Error, Expression, FieldType, SQLTree, TranslationContext,
        postgres::{translate_binary_op, translate_expr as default_translate, translate_fn_call},
    },
};

pub struct TestTranslator;

impl TranslationContext for TestTranslator {
    fn lookup_field(
        &self,
        alias: Option<&str>,
        field: &str,
    ) -> std::result::Result<(Cow<'_, str>, FieldType), String> {
        let field = field.to_string().to_uppercase();
        let field_type = match (alias, field.as_ref()) {
            (_, "A" | "B" | "C") => FieldType::Integer,
            (_, "BINDATAFIELD") => FieldType::MemoBinary,
            (_, "SHIP_DATE") => FieldType::Date,
            (_, "ID") => FieldType::Character(10),
            (_, "L_NAME") => FieldType::Character(20),
            (_, "__DELETED") => FieldType::Logical,
            (Some(alias), _) => panic!("unknown field: {alias}.{field}"),
            (None, _) => panic!("unknown field: {field}"),
        };
        Ok((field.into(), field_type))
    }

    fn translate_expr<'field_lookup, 'parse>(
        &'field_lookup self,
        source: &'parse parser::Expression,
        src_tree: &'parse parser::ParseTree,
        dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    ) -> translate::ExpResult<'field_lookup, 'parse> {
        default_translate(source, src_tree, dst_tree, self)
    }

    fn translate_fn_call<'field_lookup, 'parse>(
        &'field_lookup self,
        name: &'parse CodebaseFunction,
        args: &'parse [parser::ExpressionId],
        src_tree: &'parse parser::ParseTree,
        dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    ) -> translate::ExpResult<'field_lookup, 'parse> {
        let dst_args = translate::postgres::translate_args(args, src_tree, dst_tree, self)?;

        if name == &CodebaseFunction::DTOS {
            let date = dst_args[0].0;
            let fmt = dst_tree.push_expr("YYYYMMDD".into());
            Ok((
                Expression::FunctionCall {
                    name: "cb_date_to_text".into(),
                    args: dst_tree.push_args([date, fmt].into_iter()),
                },
                FieldType::Character(8),
            ))
        } else if let CodebaseFunction::Unknown(name) = name
            && name.eq_ignore_ascii_case("USER")
        {
            Ok((
                translate::Expression::SingleQuoteStringLiteral(Cow::from("my user")),
                FieldType::Memo,
            ))
        } else {
            translate_fn_call(name, args, src_tree, dst_tree, self)
        }
    }

    fn translate_binary_op<'field_lookup, 'parse>(
        &'field_lookup self,
        l: &'parse parser::Expression,
        op: &'parse parser::BinaryOp,
        r: &'parse parser::Expression,
        src_tree: &'parse parser::ParseTree,
        dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    ) -> translate::ExpResult<'field_lookup, 'parse> {
        translate_binary_op(self, l, op, r, src_tree, dst_tree)
    }
}

#[test]
fn field_concat_len_test() {
    let cx = TestTranslator;
    let parse_tree = parser::parse("ID + L_NAME").unwrap();
    let (_tree, field_type) = cx.translate(&parse_tree).unwrap();
    let FieldType::Character(30) = &field_type else {
        panic!(
            "Expected FieldType::Character(30) field type, got {:?}",
            field_type
        )
    };
}

#[test]
fn field_concat_if_else_len_test() {
    let cx = TestTranslator;
    let parse_tree = parser::parse("ID + iif(__DELETED,'..', '.')").unwrap();
    let (_, field_type) = cx.translate(&parse_tree).unwrap();
    let FieldType::Character(12) = &field_type else {
        panic!(
            "Expected FieldType::Character(12) field type (length of ID plus max length of if/else), got {:?}",
            field_type
        )
    };

    let parse_tree = parser::parse("ID + iif(__DELETED,'', '.')").unwrap();
    let (_, field_type) = cx.translate(&parse_tree).unwrap();
    let FieldType::Character(11) = &field_type else {
        panic!(
            "Expected FieldType::Character(11) field type (length of ID plus max length of if/else), got {:?}",
            field_type
        )
    };
}

#[test]
fn field_concat_if_else_alltrim_len_test() {
    let cx = TestTranslator;
    let parse_tree = parser::parse("ID + iif(__DELETED,'', ALLTRIM('.  '))").unwrap();
    let (_, field_type) = cx.translate(&parse_tree).unwrap();
    let FieldType::Character(13) = &field_type else {
        panic!(
            "Expected FieldType::Character(13) field type (length of ID plus max length of if/else), got {:?}",
            field_type
        )
    };
}

#[test]
fn substr_test() {
    let cx = TestTranslator;
    let parse_tree = parser::parse("substr(ID, 0, 3)").unwrap();
    let (tree, field_type) = cx.translate(&parse_tree).unwrap();
    let root = tree.get_root().expect("a root node");
    let Expression::FunctionCall { name, args } = root else {
        panic!("Expected FunctionCall, got {root:?}")
    };
    assert_eq!(name, &"SUBSTR");
    let args = tree.get_args(args);
    assert_eq!(args.len(), 3);
    assert_eq!(field_type, FieldType::Character(3));
    assert_eq!(
        *tree.get_expr(args[0]).expect("first arg"),
        Expression::Field {
            name: "ID".into(),
            field_type: FieldType::Character(10),
        }
    );
    assert_eq!(
        *tree.get_expr(args[1]).expect("second arg"),
        Expression::NumberLiteral(Cow::from("1"))
    );
    assert_eq!(
        *tree.get_expr(args[2]).expect("third arg"),
        Expression::NumberLiteral(Cow::from("3"))
    );
}

#[test]
fn str_test() {
    let cx = TestTranslator;
    for variation in ["STR(A, 10, 0)", "STR(A, 10)", "STR(A)"] {
        let parse_tree = parser::parse(variation).unwrap();
        let (tree, field_type) = cx.translate(&parse_tree).unwrap();
        let root = tree.get_root().expect("a root node");
        assert_eq!(field_type, FieldType::Character(10));

        let Expression::Iif {
            cond,
            when_true,
            when_false,
        } = root
        else {
            panic!("Expected Iif condition, got {root:?}")
        };

        // length check
        let cond = tree.get_expr_unchecked(*cond);
        let Expression::BinaryOperator(l, op, r, _) = cond else {
            panic!("Expected format string, got {:?}", cond)
        };
        let l = tree.get_expr_unchecked(*l);
        let Expression::FunctionCall {
            name: func_name,
            args,
        } = l
        else {
            panic!("Expected FunctionCall, got {:?}", l)
        };
        assert_eq!(func_name, "LENGTH");
        assert_eq!(args.len(), 1);
        assert_eq!(op, &translate::BinaryOp::Le);
        let r = tree.get_expr_unchecked(*r);
        let Expression::NumberLiteral(len) = r else {
            panic!("Expected number literal, got {:?}", r)
        };
        assert_eq!(len, "10");

        // asterisks when the number is too big for the length
        let when_false = tree.get_expr_unchecked(*when_false);
        let Expression::SingleQuoteStringLiteral(asterisks) = when_false else {
            panic!("Expected format string, got {:?}", when_false)
        };
        assert_eq!(asterisks, "**********");

        // padding string to correct length
        let when_true = tree.get_expr_unchecked(*when_true);
        let Expression::FunctionCall {
            name: func_name,
            args,
        } = when_true
        else {
            panic!("Expected FunctionCall, got {:?}", when_true)
        };
        assert_eq!(func_name, "LPAD");
        let args = tree.get_args(args);
        assert_eq!(args.len(), 3);

        let len = tree.get_expr_unchecked(args[1]);
        let Expression::NumberLiteral(len) = len else {
            panic!("Expected number literal, got {:?}", len)
        };
        assert_eq!(len, "10");

        let pad_char = tree.get_expr_unchecked(args[2]);
        let Expression::SingleQuoteStringLiteral(pad_char) = pad_char else {
            panic!("Expected string literal, got {:?}", pad_char)
        };
        assert_eq!(pad_char, " ");

        // converting the number to characters
        let to_char = tree.get_expr_unchecked(args[0]);
        let Expression::FunctionCall {
            name: func_name,
            args,
        } = to_char
        else {
            panic!("Expected FunctionCall, got {:?}", when_true)
        };
        assert_eq!(func_name, "TO_CHAR");
        let args = tree.get_args(args);
        assert_eq!(args.len(), 2);
        let first = tree.get_expr_unchecked(args[0]);
        let Expression::Field { name, field_type } = first else {
            panic!("Expected Field, got {:?}", first)
        };
        assert_eq!(name, "A");
        assert_eq!(field_type, &FieldType::Integer);
        let second = tree.get_expr_unchecked(args[1]);
        let Expression::SingleQuoteStringLiteral(fmt) = second else {
            panic!("Expected format string, got {:?}", second)
        };
        assert_eq!(fmt, "FM9999999990");
    }
}

#[test]
fn empty_string_test() {
    let cx = TestTranslator;
    let parse_tree = parser::parse("EMPTY(ID)").unwrap();
    let (tree, field_type) = cx.translate(&parse_tree).unwrap();
    let root = tree.get_root().expect("a root node");

    // Expect ID = ''
    let Expression::BinaryOperator(l, op, r, _) = root else {
        panic!("Expected BinaryOperator, got {root:?}")
    };
    assert_eq!(field_type, FieldType::Logical);
    assert_eq!(op, &translate::BinaryOp::Eq);

    let r = tree.get_expr(*r).expect("right side");
    let Expression::SingleQuoteStringLiteral(empty_str) = r else {
        panic!("Expected SingleQuoteStringLiteral, got {r:?}")
    };
    assert_eq!(empty_str, "");

    let Expression::FunctionCall {
        name: func_name,
        args,
    } = tree.get_expr_unchecked(*l)
    else {
        panic!("Expected FunctionCall, got {l:?}")
    };
    assert_eq!(func_name, &"TRIM");

    let args = tree.get_args(args);
    assert_eq!(args.len(), 1);

    let inner = tree.get_expr(args[0]).expect("one arg");
    let Expression::Field { name, field_type } = inner else {
        panic!("Expected Field, got {:?}", inner)
    };
    assert_eq!(name, "ID");
    assert_eq!(field_type, &FieldType::Character(10));
}

#[test]
fn empty_date_test() {
    let cx = TestTranslator;
    let parse_tree = parser::parse("EMPTY(SHIP_DATE)").unwrap();
    let (tree, field_type) = cx.translate(&parse_tree).unwrap();
    let root = tree.get_root().expect("a root node");

    let Expression::BinaryOperator(l, op, r, _) = root else {
        panic!("Expected BinaryOperator, got {root:?}")
    };
    assert_eq!(field_type, FieldType::Logical);
    assert_eq!(op, &translate::BinaryOp::Eq);
    let r = tree.get_expr_unchecked(*r);
    let Expression::SingleQuoteStringLiteral(coalesce_str) = r else {
        panic!("Expected SingleQuoteStringLiteral, got {r:?}")
    };
    assert_eq!(coalesce_str, "0001-01-01");

    let inner = tree.get_expr_unchecked(*l);
    let Expression::Field { name, field_type } = inner else {
        panic!("Expected Field, got {inner:?}")
    };
    assert_eq!(name, "SHIP_DATE");
    assert_eq!(field_type, &FieldType::Date);
}

#[test]
fn numeric_cast_test() {
    let cx = TestTranslator;
    let parse_tree = parser::parse("VAL(ID)").unwrap();
    let (tree, field_type) = cx.translate(&parse_tree).unwrap();
    let root = tree.get_root().expect("a root node");
    let Expression::Iif {
        cond,
        when_true,
        when_false,
    } = root
    else {
        panic!("Expected Iif condition, got {root:?}")
    };

    let cond = tree.get_expr_unchecked(*cond);
    let Expression::FunctionCall { name, args } = cond else {
        panic!("Expected FunctionCall, got {cond:?}")
    };
    assert_eq!(name, &"pg_input_is_valid");
    assert_eq!(field_type, FieldType::Numeric { len: 0, dec: 0 });

    let args = tree.get_args(args);
    assert_eq!(args.len(), 2);
    assert_eq!(
        *tree.get_expr_unchecked(args[0]),
        Expression::Field {
            name: "ID".into(),
            field_type: FieldType::Character(10),
        }
    );
    assert_eq!(
        *tree.get_expr_unchecked(args[1]),
        Expression::SingleQuoteStringLiteral(Cow::from("numeric"))
    );

    let when_true = tree.get_expr_unchecked(*when_true);
    let Expression::Cast(field_ref, s) = when_true else {
        panic!("Expected Cast, got {when_true:?}")
    };
    assert_eq!(
        *tree.get_expr_unchecked(*field_ref),
        Expression::Field {
            name: "ID".into(),
            field_type: FieldType::Character(10),
        }
    );
    assert_eq!(s, &"numeric");

    let when_false = tree.get_expr_unchecked(*when_false);
    let Expression::NumberLiteral(s) = when_false else {
        panic!("Expected NumberLiteral, got {when_false:?}")
    };
    assert_eq!(*s, "0");
}

#[test]
fn substr_wrong_params_test() {
    let cx = TestTranslator;
    let parse_tree = parser::parse("substr(ID)").unwrap();
    match cx.translate(&parse_tree) {
        Err(Error::IncorrectArgCount(func, count)) => {
            assert_eq!(func, "SUBSTR");
            assert_eq!(count, 1);
        }
        Err(e) => panic!("Expected IncorrectArgCount error, got a {e:?}"),
        Ok(_) => panic!("Expected IncorrectArgCount error, got a valid parse"),
    }
}

#[test]
fn substr_replace_0_with_1_test() {
    let cx = TestTranslator;
    let parse_tree = parser::parse("substr(ID, 0, 3)").unwrap();
    let (tree, field_type) = cx.translate(&parse_tree).unwrap();
    let root = tree.get_root().expect("a root node");
    let Expression::FunctionCall { name, args } = root else {
        panic!("Expected FunctionCall, got {root:?}")
    };
    assert_eq!(name, &"SUBSTR");

    let args = tree.get_args(args);
    assert_eq!(args.len(), 3);
    assert_eq!(field_type, FieldType::Character(3));
    assert_eq!(
        *tree.get_expr_unchecked(args[1]),
        Expression::NumberLiteral(Cow::from("1"))
    );
    assert_eq!(
        *tree.get_expr_unchecked(args[2]),
        Expression::NumberLiteral(Cow::from("3"))
    );
}

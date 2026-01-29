use crate::{
    codebase_functions::CodebaseFunction,
    parser,
    translate::{
        self, Error, ExprRef, Expression, FieldType, TranslationContext, expr_ref,
        postgres::{translate as default_translate, translate_binary_op, translate_fn_call},
    },
};

pub struct TestTranslator<F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
{
    pub field_lookup: F,
}
impl<F> TranslationContext for TestTranslator<F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
{
    fn lookup_field(
        &self,
        alias: Option<&str>,
        field: &str,
    ) -> std::result::Result<(String, FieldType), String> {
        (self.field_lookup)(alias, field)
    }

    fn translate(
        &self,
        source: &parser::Expression,
        tree: &parser::ParseTree,
    ) -> translate::Result {
        default_translate(source, tree, self)
    }

    fn translate_fn_call(
        &self,
        name: &CodebaseFunction,
        args: &[parser::ExpressionId],
        tree: &parser::ParseTree,
    ) -> std::result::Result<(ExprRef, FieldType), Error> {
        let arg = |index: usize| {
            args.get(index)
                .map(|&a| default_translate(tree.get_expr_unchecked(a), tree, self))
                .ok_or(Error::IncorrectArgCount(format!("{:?}", name), index))
        };

        if name == &CodebaseFunction::DTOS {
            Ok((
                expr_ref(Expression::FunctionCall {
                    name: "CB_DATE_TO_TEXT".into(),
                    args: vec![arg(0)??.0, expr_ref("YYYYMMDD".into())],
                }),
                FieldType::Character(8),
            ))
        } else if let CodebaseFunction::Unknown(name) = name
            && name.eq_ignore_ascii_case("USER")
        {
            Ok((
                expr_ref(translate::Expression::SingleQuoteStringLiteral(
                    "my user".to_string(),
                )),
                FieldType::Memo,
            ))
        } else {
            translate_fn_call(name, args, tree, self)
        }
    }

    fn translate_binary_op(
        &self,
        l: &parser::Expression,
        op: &parser::BinaryOp,
        r: &parser::Expression,
        tree: &parser::ParseTree,
    ) -> translate::Result {
        translate_binary_op(self, l, op, r, tree)
    }
}

#[cfg(test)]
mod tests {
    use super::{parser, *};

    #[test]
    fn field_concat_len_test() {
        let (_, field_type) = parse_expression("ID + L_NAME").unwrap();
        let FieldType::Character(len) = &field_type else {
            panic!("Expected Character field type, got {:?}", field_type)
        };
        assert_eq!(*len, 30);
    }

    #[test]
    fn substr_test() {
        let (expression, field_type) = parse_expression("substr(ID, 0, 3)").unwrap();
        let Expression::FunctionCall { name, args } = &*expression.borrow() else {
            panic!("Expected FunctionCall, got {:?}", expression)
        };
        assert_eq!(name, "SUBSTR");
        assert_eq!(args.len(), 3);
        assert_eq!(field_type, FieldType::Character(3));
        assert_eq!(
            *args[0].borrow(),
            Expression::Field {
                name: "ID".to_string(),
                field_type: FieldType::Character(10),
            }
        );
        assert_eq!(
            *args[1].borrow(),
            Expression::NumberLiteral("1".to_string())
        );
        assert_eq!(
            *args[2].borrow(),
            Expression::NumberLiteral("3".to_string())
        );
    }

    #[test]
    fn substr_wrong_params_test() {
        match parse_expression("substr(ID)") {
            Err(Error::IncorrectArgCount(func, count)) => {
                assert_eq!(func, "SUBSTR");
                assert_eq!(count, 1);
            }
            other => panic!("Expected IncorrectArgCount error, got {:?}", other),
        }
    }

    #[test]
    fn substr_replace_0_with_1_test() {
        let (expression, field_type) = parse_expression("substr(ID, 0, 3)").unwrap();
        let Expression::FunctionCall { name, args } = &*expression.borrow() else {
            panic!("Expected FunctionCall, got {:?}", expression)
        };
        assert_eq!(name, "SUBSTR");
        assert_eq!(args.len(), 3);
        assert_eq!(field_type, FieldType::Character(3));
        assert_eq!(
            *args[1].borrow(),
            Expression::NumberLiteral("1".to_string())
        );
        assert_eq!(
            *args[2].borrow(),
            Expression::NumberLiteral("3".to_string())
        );
    }

    fn parse_expression(expr: &str) -> translate::Result {
        let (tree, root) = parser::parse(expr).unwrap();
        let cx = TestTranslator {
            field_lookup: |alias: Option<&str>,
                           field: &str|
             -> Result<(String, FieldType), String> {
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
                Ok((field, field_type))
            },
        };
        cx.translate(&root, &tree)
    }
}

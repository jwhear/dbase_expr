use chrono::NaiveDate;
use dbase_expr::{
    codebase_functions::CodebaseFunction,
    parser::parse,
    to_sql::PrinterConfig,
    translate::{
        Error, ExprRef, Expression, FieldType, TranslationContext, expr_ref,
        postgres::{
            Translator, translate as default_translate, translate_binary_op, translate_fn_call,
        },
    },
    *,
};
use to_sql::Printer;

fn main() {
    println!("Running expr tests...");
    expr_tests();

    // Plain vanilla translation
    println!("Running to_sql tests...");
    let get_type = |alias: Option<&str>, field: &str| match (alias, field) {
        (_, "__DELETED") => FieldType::Logical,
        (_, "A" | "B" | "C") => FieldType::Integer,
        (_, "BINDATAFIELD") => FieldType::MemoBinary,
        (_, "SHIP_DATE") => FieldType::Date,
        (_, "ID") => FieldType::Character(1),
        (_, "L_NAME") => FieldType::Character(20),
        (Some(alias), _) => panic!("unknown field: {alias}.{field}"),
        (None, _) => panic!("unknown field: {field}"),
    };

    let translation_cx = Translator {
        field_lookup: |alias: Option<&str>, field: &str| -> Result<(String, FieldType), String> {
            let field = field.to_string().to_uppercase();
            let field_type = get_type(alias, &field);
            Ok((field, field_type))
        },
    };
    to_sql_tests(&translation_cx);

    // Overriding the DTOS function
    println!("Running to_sql tests with function overriding...");
    pub struct CustomTranslator<F>
    where
        F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
    {
        field_lookup: F,
    }
    impl<F> TranslationContext for CustomTranslator<F>
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
                    .map(|a| tree.get_expr_unchecked(*a))
                    .map(|a| default_translate(a, tree, self))
                    .ok_or(Error::IncorrectArgCount(format!("{:?}", name), index))
            };

            if let CodebaseFunction::Unknown(unknown) = name
                && unknown.eq_ignore_ascii_case("USER")
            {
                Ok((
                    expr_ref(Expression::SingleQuoteStringLiteral("my user".to_owned())),
                    FieldType::Memo,
                ))
            } else if name == &CodebaseFunction::DTOS {
                Ok((
                    expr_ref(Expression::FunctionCall {
                        name: "CB_DATE_TO_TEXT".into(),
                        args: vec![arg(0)??.0, expr_ref("YYYYMMDD".into())],
                    }),
                    FieldType::Character(8),
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
    let cx = CustomTranslator {
        field_lookup: |alias: Option<&str>, field: &str| -> Result<(String, FieldType), String> {
            let field = field.to_string().to_uppercase();
            let field_type = get_type(alias, &field);
            Ok((field, field_type))
        },
    };
    to_sql_tests(&cx);
}

fn expr_tests() {
    let tests = [
        "-",    //0
        "--3",  //3 because it translates to -(-3)
        "---3", //-3 because it translates to -(-(-3))
        "+-3",  //-3
        ".",    //0
        ".5",   //0.5
        "5.",   //5.0
        "   3    - 44  ",
        "deleted() = .f. .and. substr(id, 1, 3 ) <> \"($)\"",
        ".NOT.deleted()",
        "12",
        "(12)",
        "a + b * c",
        "a * b + c",
        "(a + b) * c",
        "left((a), -b + -c)",
        "a .or. d",
        "a .and. d",
        "B_T .or. B_F",
        ".T..AND..FALSE.",
        r#""double quote""#,
        r#"'single quote'"#,
        r#"VAL('10.123')"#,
        "SUBSTR('hello', 2, 3)",
        "IIF (B_T, ID, L_NAME)",
        "LEFT(ID, 2)",
        "RIGHT(ID, 8)",
        "ID + L_NAME",
        // From Paul
        r#"iif(.t., (ID="A"), (ID="E"))"#,
        "bindatafield <> CHR(0)",
        r#""T"$L_NAME"#,
        r#""mit"$L_NAME"#,
        r#"(DATE() + 1) - STOD("20240731")"#,
        r#"(DATE() + 1) = STOD("20240802")"#,
        r#"(DATE() + 1) = STOD("20250620")"#,
        r#"(SHIP_DATE + 1) = STOD("20240802")"#,
        "iif( ( trim( F_NAME ) <> trim( \" \" ) ), ( trim( F_NAME ) + \" \" + trim( L_NAME ) ), ( trim( F_NAME) + trim( L_NAME ) ) )",
        // From Randy's training video
        "date() + 7-
        ((DATE() - STOD('20000102')) -
        VAL(STR((DATE() - STOD('20000102'))/7 - 0.5,6,0))*7)",
        "date() + 14 -
        ((DATE() - STOD('20000102')) -
        VAL(STR((DATE() - STOD('20000102'))/7 - 0.5,6,0))*7)",
        "SHIP_DATE >= STOD('20240622')",
        "SHIP_DATE >= date() + 7-
        ((DATE() - STOD('20000102')) -
        VAL(STR((DATE() - STOD('20000102'))/7 - 0.5,6,0))*7)
        .and.
         SHIP_DATE < date() + 14 -
         ((DATE() - STOD('20000102')) -
         VAL(STR((DATE() - STOD('20000102'))/7 - 0.5,6,0))*7)",
        // Precision test
        "STR(0.1 + 1/3, 17, 15)",
        // Simplify test
        "a+b+c",
        "'hello' + chr(32) + LEFT('world', 3) + 'ld'",
        "a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c",
        // Parsing of numbers with optional digits before and after decimal
        ".+.=.", // 0.0 + 0.0 = 0.0
        "1. + 2 = 3.00",
        ".1 + 0.2 = 000.3",
        "USER() + \"Hello world\"",
    ];

    let value_lookup = |_alias: Option<&str>, field_name: &str| -> Option<evaluate::Value> {
        match field_name.to_uppercase().as_str() {
            "B_T" => Some(evaluate::Value::Bool(true)),
            "B_F" => Some(evaluate::Value::Bool(false)),
            "A" => Some(evaluate::Value::Number(1.0, false)),
            "B" => Some(evaluate::Value::Number(2.0, false)),
            "C" => Some(evaluate::Value::Number(3.0, false)),
            "D" => Some(evaluate::Value::Number(0.0, false)),
            "BINDATAFIELD" => Some(evaluate::Value::Blob(vec![0x01, 0x02, 0x03])),
            "SHIP_DATE" => Some(evaluate::Value::Date(NaiveDate::from_ymd_opt(2024, 8, 1))),
            "ID" => Some(evaluate::Value::FixedLenStr("DOEJOH".into(), 10)),
            "F_NAME" => Some(evaluate::Value::FixedLenStr("John".into(), 100)),
            "L_NAME" => Some(evaluate::Value::FixedLenStr("Smith".into(), 100)),
            "__DELETED" => Some(evaluate::Value::Bool(false)),
            _ => None,
        }
    };

    let custom_functions = |name: &str| {
        name.eq_ignore_ascii_case("USER")
            .then_some(Ok(evaluate::Value::Str("my user".to_owned())))
    };

    for test in tests.iter() {
        //println!("{test}");
        match parse(test) {
            Ok((tree, root)) => {
                //println!("{t:?}");
                match evaluate::evaluate(&root, &tree, &value_lookup, &custom_functions) {
                    Ok(_tree) => {}
                    //println!("{test} => {tree:?}\n"),
                    Err(e) => {
                        eprintln!("{test}\n{e:?}\n")
                    }
                }
            }
            Err(e) => println!("{test}\n{e}\n"),
        };
    }
}

fn to_sql_tests<T: TranslationContext>(cx: &T) {
    let tests = [
        "deleted() = .f. .and. substr(id, 1, 3 ) <> \"($)\"",
        ".NOT.deleted()",
        "12",
        "(12)",
        "a + b * c",
        "a * b + c",
        "(a + b) * c",
        "left((a), -b + -c)",
        ".T..AND..FALSE.",
        r#""double quote""#,
        r#"'single quote'"#,
        r#"VAL('10.123')"#,
        "SUBSTR('hello', 2, 3)",
        "DTOS(DATE())",
        // From Paul
        r#"iif(.t., (ID="A"), (ID="E"))"#,
        "bindatafield <> CHR(0)",
        r#""T"$L_NAME"#,
        r#"(DATE() + 1) - STOD("20240731")"#,
        r#"(DATE() + 1) = STOD("20240802")"#,
        // From Randy's training video
        "SHIP_DATE >= date() + 7-
         ((DATE() - STOD('20000102')) -
           VAL(STR((DATE() - STOD('20000102'))/7 - 0.5,6,0))*7)
         .and.
         SHIP_DATE < date() + 14 -
         ((DATE() - STOD('20000102')) -
          VAL(STR((DATE() - STOD('20000102'))/7 - 0.5,6,0))*7)",
        // Simplification test
        "a + b + c + a + b",
        // Custom function translation
        "USER() + \"Hello world\"",
    ];

    for test in tests.iter() {
        match parse(test) {
            Ok((tree, root)) => match cx.translate(&root, &tree) {
                Ok(tree) => println!(
                    "{test}\n=>\n{}\n",
                    Printer::new(tree.0, PrinterConfig::default())
                ),
                Err(e) => eprintln!("Error translating tree: {e:?}\n:{test}\n"),
            },

            Err(e) => println!("{:?}", e),
        };
    }
}

use chrono::NaiveDate;
use dbase_expr::{translate::FieldType, *};
use to_sql::Printer;

fn main() {
    println!("Running expr tests...");
    expr_tests();

    println!("Running to_sql tests...");
    to_sql_tests();
}

fn expr_tests() {
    let parser = grammar::ExprParser::new();
    let tests = [
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
        r#""double \" quote""#,
        r#"'single \' quote'"#,
        r#"VAL('10.123')"#,
        "SUBSTR('hello', 2, 3)",
        "IIF (B_T, ID, L_NAME)", //TODO: this should return length of 100 since we should assume the larger of the potential values
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
    ];

    let value_lookup = |field_name: &str| -> Option<evaluate::Value> {
        match field_name.to_uppercase().as_str() {
            "B_T" => Some(evaluate::Value::Bool(true)),
            "B_F" => Some(evaluate::Value::Bool(false)),
            "A" => Some(evaluate::Value::Number(1.0)),
            "B" => Some(evaluate::Value::Number(2.0)),
            "C" => Some(evaluate::Value::Number(3.0)),
            "D" => Some(evaluate::Value::Number(0.0)),
            "BINDATAFIELD" => Some(evaluate::Value::Blob(vec![0x01, 0x02, 0x03])),
            "SHIP_DATE" => Some(evaluate::Value::Date(
                NaiveDate::from_ymd_opt(2024, 8, 1).unwrap(),
            )),
            "ID" => Some(evaluate::Value::Str("DOEJOH".into(), 10)),
            "F_NAME" => Some(evaluate::Value::Str("John".into(), 100)),
            "L_NAME" => Some(evaluate::Value::Str("Smith".into(), 100)),
            "__DELETED" => Some(evaluate::Value::Bool(false)),
            _ => None,
        }
    };

    for test in tests.iter() {
        match parser.parse(test) {
            Ok(t) => match evaluate::evaluate(&t, &value_lookup) {
                Ok(tree) => println!("{test} => {tree:?}\n"),
                Err(e) => eprintln!("{test} => Error translating tree: {e:?}\n"),
            },
            // The parse failed with an unexpected token: show the approximate
            //  position in the source
            Err(lalrpop_util::ParseError::InvalidToken { location }) => {
                let end = test.len().min(location + 10);
                println!("Failed: {}\nError near here: {}", test, unsafe {
                    test.get_unchecked(location..end)
                })
            }
            // Any other kind of error, just print it
            Err(e) => println!("{:?}", e),
        };
    }
}

fn to_sql_tests() {
    let parser = grammar::ExprParser::new();
    let tests = [
        "deleted() = .f. .and. substr(id, 1, 3 ) <> \"($)\"",
        ".NOT.deleted()",
        "12",
        "(12)",
        "a + b * c",
        "a * b + c",
        "(a + b) * c",
        "left((a), -b + -c)",
        "a .or. b",
        ".T..AND..FALSE.",
        r#""double \" quote""#,
        r#"'single \' quote'"#,
        r#"VAL('10.123')"#,
        "SUBSTR('hello', 2, 3)",
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
    ];

    let get_type = |alias: Option<&str>, field: &str| match (alias, field) {
        (_, "A" | "B" | "C") => FieldType::Integer,
        (_, "BINDATAFIELD") => FieldType::MemoBinary,
        (_, "SHIP_DATE") => FieldType::Date,
        (_, "ID") => FieldType::Character(1),
        (_, "L_NAME") => FieldType::Character(20),
        (Some(alias), _) => panic!("unknown field: {alias}.{field}"),
        (None, _) => panic!("unknown field: {field}"),
    };

    let field_lookup = |alias: Option<&str>, field: &str| -> (String, FieldType) {
        let field = field.to_string().to_uppercase();
        let field_type = get_type(alias, &field);
        (field, field_type)
    };

    for test in tests.iter() {
        match parser.parse(test) {
            Ok(t) => match translate::translate(&t, &field_lookup) {
                Ok(tree) => println!("{test}\n=>\n{}\n", Printer::new(tree.0)),
                Err(e) => eprintln!("Error translating tree: {e:?}"),
            },

            // The parse failed with an unexpected token: show the approximate
            //  position in the source
            Err(lalrpop_util::ParseError::InvalidToken { location }) => {
                let end = test.len().min(location + 10);
                println!("Failed: {}\nError near here: {}", test, unsafe {
                    test.get_unchecked(location..end)
                })
            }
            // Any other kind of error, just print it
            Err(e) => println!("{:?}", e),
        };
    }
}

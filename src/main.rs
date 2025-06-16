use dbase_expr::{translate::FieldType, *};
use to_sql::Printer;

fn main() {
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

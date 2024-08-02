use dbase_query::*;
use to_sql::Printer;

fn main() {
    let parser = grammar::ExprParser::new();
    let tests = [
        "12",
        "(12)",
        "a + b * c",
        "a * b + c",
        "(a + b) * c",
        "left((a), -b + -c)",
        ".T..AND..FALSE.",
        r#""double \" quote""#,
        r#"'single \' quote'"#,
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
    for test in tests.iter() {
        match parser.parse(test) {
            Ok(t) => match sql::translate(&t) {
                Ok(tree) => println!("{test}:\n\t{}\n", Printer::new(tree)),
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

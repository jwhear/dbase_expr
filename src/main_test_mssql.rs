use dbase_expr::{
    ast::simplify,
    to_sql::{MssqlPrinterContext, PrinterConfig},
    translate::{FieldType, TranslationContext, mssql::MssqlTranslator},
    *,
};
use to_sql::Printer;

fn main() {
    println!("Running MSSQL translator tests...");

    let get_type = |alias: Option<&str>, field: &str| match (alias, field) {
        (_, "A" | "B" | "C") => FieldType::Integer,
        (_, "BINDATAFIELD") => FieldType::MemoBinary,
        (_, "SHIP_DATE") => FieldType::Date,
        (_, "DATE") => FieldType::Date,
        (_, "ID") => FieldType::Character(1),
        (_, "QUOTE") => FieldType::Character(10),
        (_, "L_NAME") => FieldType::Character(20),
        (_, "DESCR_2") => FieldType::Memo,
        (_, "DESCRIPTION") => FieldType::Memo,
        (_, "PO_EXT") => FieldType::Character(2),
        (_, "PO_NO") => FieldType::Character(15),
        (_, "C_TYPE") => FieldType::Numeric { len: 2, dec: 0 },
        (_, "INACTIVE") => FieldType::Logical,

        (Some(alias), _) => panic!("unknown field: {alias}.{field}"),
        (None, _) => panic!("unknown field: {field}"),
    };

    let mssql_cx = MssqlTranslator {
        field_lookup: |alias: Option<&str>, field: &str| -> Result<(String, FieldType), String> {
            let field = field.to_string().to_uppercase();
            let field_type = get_type(alias, &field);
            Ok((field, field_type))
        },
    };

    let parser = grammar::ExprParser::new();
    let tests = [
        //Concatenate
        "'John'+'Doe'",
        //ConcatenateII
        "'  John'-'Doe  '",
        "'John  '-'Doe'",
        // Test simple date arithmetic
        "DATE() + 1",
        "DATE() - 1",
        "DATE() + 1 + 2",
        // Test date - date
        "DATE() - STOD(\"20240731\")",
        // Test some functions that are different
        "SHIP_DATE - STOD(\"20240630\")",
        // Test the complex expression
        "date() + 7 - ((DATE() - STOD('20000102')) - VAL(STR((DATE() - STOD('20000102'))/7 - 0.5,6,0))*7)",
        "DATE >= date() + 7-
        ((DATE() - STOD('20000102')) -
        VAL(STR((DATE() - STOD('20000102'))/7 - 0.5,6,0))*7)
        .and.
         DATE < date() + 14 -
         ((DATE() - STOD('20000102')) -
         VAL(STR((DATE() - STOD('20000102'))/7 - 0.5,6,0))*7)",
        // Also test some functions that are different
        "CHR(65)",
        "'HELLO' + 12",
        "'Hello from ' + ' - ' + STOD('20000102')",
        "CTOD(\"07/04/24\")",
        "DTOC(SHIP_DATE)",
        "DTOS(SHIP_DATE)",
        "DAY(SHIP_DATE)",
        "MONTH(SHIP_DATE)",
        "YEAR(SHIP_DATE)",
        "PADL('asdddd', 5)",
        "PADL(ALLTRIM(QUOTE),10)",
        "RIGHT(ID, 1)",
        "LEFT('asd',1)",
        "EMPTY(DESCR_2)",
        ".not.EMPTY(DESCR_2)",
        "IIF(EMPTY(DESCR_2), 'Empty', 'Not Empty')",
        "EMPTY(DATE)",
        "IIF(EMPTY(DATE), 'Empty', 'Not Empty')",
        "EMPTY(C_TYPE)",
        ".not.EMPTY(C_TYPE)",
        "EMPTY(BINDATAFIELD)",
        ".not.EMPTY(BINDATAFIELD)",
        "STR(A, 5, 2)",
        "VAL(\"123.45\")",
        // Test contain operation with CHARINDEX
        "'world' $ 'Hello world'",
        "'xyz' $ 'Hello world'",
        "'product' $ DESCRIPTION",
        //Nested IIF
        "IIF(C_TYPE=0,'Service',IIF(C_TYPE=1,'No Count',IIF(C_TYPE=2,'Track Count',IIF(C_TYPE=3,'Serialized',IIF(C_TYPE=4,'Special',IIF(C_TYPE=5,'Rental',IIF(C_TYPE=6,'Percentage Price',IIF(C_TYPE=7,'Non-inventory Serialized',IIF(C_TYPE=8,'Rental',IIF(C_TYPE=9,'Average-Cost Lot',IIF(C_TYPE=10,'Discount',IIF(C_TYPE=11,'Tracked-Cost Lot',IIF(C_TYPE=12,'Gift Card','')))))))))))))",
        "po_no+iif(po_ext='   ','', '.' + ALLTRIM(po_ext))",
        "iif(po_ext=po_no, 'Match', 'No Match')",
        "iif(dtos(DATE) = '2001', 'Y', 'N')",
        "po_no + iif(EMPTY(po_ext),'', '.' + ALLTRIM(po_ext))",

        // Tests for bit-typed expressions in SQL Server
        // These should translate roughly to the commented SQL or something equivalent
        // The main thing is that they should produce a valid expression that can be used as a SELECT column
        ".t.", // 1
        ".f.", // 0
        "INACTIVE", // INACTIVE
        ".not. INACTIVE", // ~INACTIVE
        "INACTIVE = .f.", // IIF(INACTIVE = 0, 1, 0)
        "INACTIVE = .f. = .f.", // Whatever this translates to, it should be equivalent to INACTIVE
        "INACTIVE .or. A < 0", // INACTIVE | IIF(A < 0, 1, 0)
        "DATE = SHIP_DATE", // IIF(DATE = SHIP_DATE, 1, 0)
        "empty(C_TYPE)", // IIF(COALESCE(C_TYPE, 0) = 0, 1, 0)
        "iif(INACTIVE, 'Inactive', 'Active')", // IIF(INACTIVE = 1, 'Inactive', 'Active')
        "iif(INACTIVE = .t., 'Inactive', 'Active')", // IIF(INACTIVE = 1, 'Inactive', 'Active')
        "iif(.not. INACTIVE, 'Active', 'Inactive')", // IIF(NOT INACTIVE = 1, 'Active', 'Inactive')
        "iif(DATE < stod('19690720'), DATE > stod('19620220'), L_NAME = 'Armstrong' .or. L_NAME = 'Aldrin')", // IIF(DATE < '1969-07-20', IIF(DATE > '1962-02-20', 1, 0), IIF(L_NAME = 'Armstrong' OR L_NAME = 'Aldrin', 1, 0))

        // Tests for boolean-typed conditions in SQL Server
        // These should translate roughly to the commented SQL or something equivalent
        // The main thing is that they should produce a valid condition that can be used as a WHERE clause
        ".t.", // 1 = 1
        ".f.", // 1 <> 1
        "INACTIVE", // INACTIVE = 1
        ".not. INACTIVE", // NOT INACTIVE = 1
        "INACTIVE = .f.", // INACTIVE = 0
        "INACTIVE = .f. = .f.", // Whatever this translates to, it should be equivalent to INACTIVE = 1
        "INACTIVE .or. A < 0", // INACTIVE = 1 OR A < 0
        "DATE = SHIP_DATE", // DATE = SHIP_DATE
        "empty(C_TYPE)", // COALESCE(C_TYPE, 0) = 0
        "iif(DATE < stod('19690720'), DATE > stod('19620220'), L_NAME = 'Armstrong' .or. L_NAME = 'Aldrin')", // IIF(DATE < '1969-07-20', IIF(DATE > '1962-02-20', 1, 0), IIF(L_NAME = 'Armstrong' OR L_NAME = 'Aldrin', 1, 0)) = 1
    ];

    for test in tests.iter() {
        match parser.parse(test) {
            Ok(t) => {
                let t = simplify(*t);
                match mssql_cx.translate(&t) {
                    Ok(tree) => {
                        let mssql_config = PrinterConfig {
                            context: Box::new(MssqlPrinterContext),
                        };
                        println!("{test}\n=>\n{}\n", Printer::new(tree.0, mssql_config))
                    }
                    Err(e) => eprintln!("Error translating tree: {e:?}\n:{test}\n"),
                }
            }

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

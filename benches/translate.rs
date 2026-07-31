use std::borrow::Cow;

use criterion::{Criterion, criterion_group, criterion_main};
use dbase_expr::{
    parse,
    parser::ParseTree,
    to_sql::{Printer, PrinterConfig},
    translate::{FieldType, TranslationContext, postgres::Translator},
};

const TESTS: [&str; 9] = [
    r#"("DSGCJPNREJ4FNK00")"#,
    r#"("                        ") + ("          ") + "(none)""#,
    r#"iif(((0)=2.or.(0)=3.or.(0)=4.or.(0)=9.or.(0)=11.or.(0)=7),(0.000000) + (0.000000),0.0)"#,
    r#"ID+WAREHOUSE+IIF(PICK_LOC,'T','F')+LOCATION"#,
    r#"("($)    2                ")+("LOCC      ")+IIF(.T.,'T','F')+("DE
       │ FAULT             ")"#,
    r#""E-MAIL"$UPPER(("               ")) .or. "EMAIL"$UPPER(("
       │        "))"#,
    r#"(0.000000) + (0.000000)"#,
    r#"TRIM(TRIM(TRIM(TRIM(TRIM(TRIM(TRIM(TRIM(TRIM(TRIM(""))))))))))"#,
    "a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c+a+b+c",
];

/*
Keeping Score
--------------
ea6fe21: [115.55 µs 115.90 µs 116.23 µs]  Original version
1cec594: [20.666 µs 20.694 µs 20.726 µs]  Rewrite to ExpressionTree, tests pass
a28f474: [17.737 µs 17.751 µs 17.767 µs]  Make field lookup return a Cow instead of String

*/

// This tests both the translate and the to_sql because they really go together
//  (you're unlikely to call translate without also calling to_sql)
fn translate<'field_lookup, F>(tests: &[ParseTree], cx: &Translator<'field_lookup, F>)
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(Cow<'field_lookup, str>, FieldType), String>,
{
    for tree in tests {
        #[allow(clippy::unit_arg)]
        std::hint::black_box({
            let (exp, _ft) = cx.translate(tree).expect("translated");
            let _as_sql = format!("{}", Printer::new(exp, PrinterConfig::default()));
        })
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    // We don't want to include the parse time in our benchmark, so do before
    //  measurement begins
    let tests: Vec<_> = TESTS.iter().map(|t| parse(t).expect("valid")).collect();
    let cx = Translator {
        field_lookup: |_: Option<&str>, name: &str| match name {
            "ID" => Ok((Cow::from("ID"), FieldType::Character(8))),
            "WAREHOUSE" => Ok(("WAREHOUSE".into(), FieldType::Character(12))),
            "PICK_LOC" => Ok(("PICK_LOC".into(), FieldType::Logical)),
            "LOCATION" => Ok(("LOCATION".into(), FieldType::Character(12))),
            "E-MAIL" => Ok(("E-MAIL".into(), FieldType::Memo)),
            "a" => Ok(("A".into(), FieldType::Numeric { len: 12, dec: 3 })),
            "b" => Ok(("B".into(), FieldType::Numeric { len: 10, dec: 2 })),
            "c" => Ok(("C".into(), FieldType::Numeric { len: 4, dec: 0 })),
            _ => Err("Unknown field".into()),
        },
    };

    // Uncomment to see the translated tests
    /*
    for (tree, root) in tests.iter() {
        let (exp, _ft) = cx.translate(&root, &tree).expect("translated");
        let as_sql = format!("{}", Printer::new(exp, PrinterConfig::default()));
        println!("translated to: {as_sql}",);
    }
    */

    c.bench_function("translate", |b| b.iter(|| translate(&tests, &cx)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

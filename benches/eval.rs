use criterion::{Criterion, criterion_group, criterion_main};

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

fn old_eval() {
    use dbase_expr::{
        evaluate::{Value, evaluate},
        parser::parse,
    };

    let value_lookup = |_: Option<&str>, name: &str| -> Option<Value> {
        match name {
            "ID" => Some(Value::FixedLenStr("A1234567".into(), 8, false)),
            "WAREHOUSE" => Some(Value::FixedLenStr("Main".into(), 12, false)),
            "PICK_LOC" => Some(Value::Bool(true)),
            "LOCATION" => Some(Value::FixedLenStr("   Dunno".into(), 8, false)),
            "E-MAIL" => Some(Value::Str("foo@example.com".into())),
            "a" => Some(Value::Number(1.0, false)),
            "b" => Some(Value::Number(2.0, false)),
            "c" => Some(Value::Number(3.0, false)),
            _ => None,
        }
    };
    let custom_functions = |_: &str| None;
    for test in TESTS.iter() {
        #[allow(clippy::unit_arg)]
        std::hint::black_box({
            let tree = parse(test).unwrap();
            evaluate(&tree, &value_lookup, &custom_functions).expect("evaluated");
        })
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("old eval", |b| b.iter(old_eval));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

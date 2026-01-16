use criterion::{Criterion, criterion_group, criterion_main};

const TESTS: [&str; 8] = [
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
];

fn old_parser() {
    use dbase_expr::grammar::ExprParser;
    let parser = ExprParser::new();
    for test in TESTS.iter() {
        _ = std::hint::black_box(parser.parse(test));
    }
}

fn new_parser() {
    use dbase_expr::parser::parse;
    for test in TESTS.iter() {
        _ = std::hint::black_box(parse(test));
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("old parser", |b| b.iter(|| old_parser()));
    c.bench_function("new parser", |b| b.iter(|| new_parser()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

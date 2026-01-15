use criterion::{Criterion, criterion_group, criterion_main};
use dbase_expr::grammar::ExprParser;

fn parse_some_expressions() {
    let tests = [
        r#"("DSGCJPNREJ4FNK00")"#,
        r#"("                        ") + ("          ") + "(none)""#,
        r#"iif(((0)=2.or.(0)=3.or.(0)=4.or.(0)=9.or.(0)=11.or.(0)=7),(0.000000) + (0.000000),0.0)"#,
        r#"ID+WAREHOUSE+IIF(PICK_LOC,'T','F')+LOCATION"#,
        r#"("($)    2                ")+("LOCC      ")+IIF(.T.,'T','F')+("DE
       │ FAULT             ")"#,
        r#""E-MAIL"$UPPER(("               ")) .or. "EMAIL"$UPPER(("
       │        "))"#,
        r#"(0.000000) + (0.000000)"#,
    ];

    let parser = ExprParser::new();
    for test in tests.iter() {
        _ = std::hint::black_box(parser.parse(test));
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("parse some expressions", |b| {
        b.iter(|| parse_some_expressions())
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

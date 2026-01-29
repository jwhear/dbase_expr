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

/*
fn old_parser() {
    use dbase_expr::{ast::simplify, grammar::ExprParser};
    let parser = ExprParser::new();
    for test in TESTS.iter() {
        _ = std::hint::black_box({
            let p = parser.parse(test).expect("parsed");
            // With the old parser, we have to simplify to get the same results
            //  as with the new one
            let p = simplify(*p);
        });
    }
}
*/

fn new_parser() {
    use dbase_expr::parser::parse;
    for test in TESTS.iter() {
        _ = std::hint::black_box(parse(test));
    }
}
fn new_parser_no_alloc() {
    use dbase_expr::parser::{Depth, ParseTree, parse_into_tree};
    let exprs = Vec::with_capacity(1000);
    let args = Vec::with_capacity(2000);
    let mut tree = ParseTree::new_from_vecs(exprs, args);
    for test in TESTS.iter() {
        _ = std::hint::black_box({
            tree.clear();
            parse_into_tree(test, &mut tree, Depth::default())
        });
    }
}

fn simple_text_parse() {
    use dbase_expr::simple_text_expr::parse_simple_text_expr;
    for test in TESTS.iter() {
        _ = std::hint::black_box(parse_simple_text_expr(test));
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    //c.bench_function("old parser", |b| b.iter(|| old_parser()));
    c.bench_function("new parser", |b| b.iter(new_parser));
    c.bench_function("new parser, no alloc", |b| b.iter(new_parser_no_alloc));
    c.bench_function("simple text parser", |b| b.iter(simple_text_parse));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

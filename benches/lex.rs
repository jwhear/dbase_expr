use criterion::{Criterion, criterion_group, criterion_main};

const TESTS: [&str; 7] = [
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
fn new_lexer() {
    use dbase_expr::lex::Lexer;
    // we'll track the number of tokens seen to ensure that the loop below doesn't
    //  get optimized out
    let mut num_tokens: u64 = 0;
    for test in TESTS {
        let mut lexer = Lexer::new(test.as_bytes());
        loop {
            match lexer.next_token() {
                Ok(Some(_)) => num_tokens += 1,
                Ok(None) => break,
                Err(e) => panic!("Unexpected: {e}"),
            }
        }
    }
    assert_eq!(num_tokens, 125);
}
// Unfortunately lalrpop doesn't let me import and use the generated lexer, so
//  no head to head

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("lex some expressions", |b| b.iter(|| new_lexer()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

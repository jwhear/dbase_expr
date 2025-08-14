#![no_main]
use arbitrary::{Arbitrary, Unstructured};
use dbase_expr::ast::Expression;
use dbase_expr::ast::*;
use libfuzzer_sys::fuzz_target;
use std::str;

#[derive(Debug)]
pub struct AstInput {
    pub expr: Expression,
}

impl<'a> Arbitrary<'a> for AstInput {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(AstInput {
            expr: Expression::arbitrary(u)?,
        })
    }
}

fuzz_target!(|data: &[u8]| {
    if let Ok(input) = arbitrary::Unstructured::new(data).arbitrary::<AstInput>() {
        // Now you can feed the AST directly into your translation logic
        let _ = dbase_expr::fuzz_helper::translate_ast(input.expr);
    }
});

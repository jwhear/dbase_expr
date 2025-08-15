#![no_main]
use libfuzzer_sys::fuzz_target;

use arbitrary::Arbitrary;
use std::str;

#[derive(Debug)]
pub struct ExprInput {
    pub expr: String,
}

impl<'a> Arbitrary<'a> for ExprInput {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let expr = random_expr_string(u)?;
        Ok(ExprInput { expr })
    }
}

const MAX_EXPR_LENGTH: usize = 10000;

fn random_expr_string(u: &mut arbitrary::Unstructured) -> arbitrary::Result<String> {
    let s: String = u.arbitrary()?;
    Ok(s.chars().take(MAX_EXPR_LENGTH).collect())
}

fuzz_target!(|data: &[u8]| {
    if let Ok(input) = arbitrary::Unstructured::new(data).arbitrary::<ExprInput>() {
        dbase_expr::fuzz_helper::translate_expr(&input.expr);
    }
});

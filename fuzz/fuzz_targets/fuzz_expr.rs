#![no_main]
use libfuzzer_sys::fuzz_target;

const MAX_EXPR_LENGTH: usize = 10000;

fuzz_target!(|data: &[u8]| {
    if let Ok(input) = std::str::from_utf8(data) {
        dbase_expr::fuzz_helper::translate_expr(&input);
    }
});

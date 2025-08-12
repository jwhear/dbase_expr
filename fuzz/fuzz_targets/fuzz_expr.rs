#![no_main]
use libfuzzer_sys::fuzz_target;

use arbitrary::Arbitrary;
use dbase_expr::{
    ast, evaluate, grammar,
    translate::{FieldType, TranslationContext, postgres::Translator},
};
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

fn random_expr_string(u: &mut arbitrary::Unstructured) -> arbitrary::Result<String> {
    const ALLOWED_CHARS: &[u8] =
        b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_()+-*/=<>.\"' ";

    let len = u.int_in_range(1..=1000)?;

    let mut s = String::with_capacity(len);
    for _ in 0..len {
        let &ch = u.choose(ALLOWED_CHARS)?;
        s.push(ch as char);
    }
    Ok(s)
}

fuzz_target!(|data: &[u8]| {
    if let Ok(input) = arbitrary::Unstructured::new(data).arbitrary::<ExprInput>() {
        let expr_str = &input.expr;

        // Try parsing
        let parser = grammar::ExprParser::new();
        if let Ok(parsed) = parser.parse(expr_str) {
            let simplified = ast::simplify(*parsed);

            // Example simple value lookup for evaluation
            let value_lookup = |field_name: &str| -> Option<evaluate::Value> {
                match field_name.to_uppercase().as_str() {
                    "B_T" => Some(evaluate::Value::Bool(true)),
                    "B_F" => Some(evaluate::Value::Bool(false)),
                    "A" => Some(evaluate::Value::Number(1.0)),
                    "B" => Some(evaluate::Value::Number(2.0)),
                    "C" => Some(evaluate::Value::Number(3.0)),
                    "D" => Some(evaluate::Value::Number(0.0)),
                    "BINDATAFIELD" => Some(evaluate::Value::Blob(vec![0x01, 0x02, 0x03])),
                    "SHIP_DATE" => Some(evaluate::Value::Date(chrono::NaiveDate::from_ymd_opt(
                        2024, 8, 1,
                    ))),
                    "ID" => Some(evaluate::Value::Str("DOEJOH".into(), 10)),
                    "F_NAME" => Some(evaluate::Value::Str("John".into(), 100)),
                    "L_NAME" => Some(evaluate::Value::Str("Smith".into(), 100)),
                    "__DELETED" => Some(evaluate::Value::Bool(false)),
                    _ => None,
                }
            };

            // Evaluate, ignore errors
            let _ = evaluate::evaluate(&simplified, &value_lookup);

            // Optional: translate
            let translator = Translator {
                field_lookup: |_alias: Option<&str>,
                               field: &str|
                 -> Result<(String, FieldType), String> {
                    let field = field.to_string().to_uppercase();
                    let field_type = match field.as_str() {
                        "A" | "B" | "C" => FieldType::Integer,
                        "BINDATAFIELD" => FieldType::MemoBinary,
                        "SHIP_DATE" => FieldType::Date,
                        "ID" => FieldType::Character(1),
                        "L_NAME" => FieldType::Character(20),
                        _ => FieldType::Character(10),
                    };
                    Ok((field, field_type))
                },
            };
            let _ = translator.translate(&simplified);
        }
    }
});

use crate::{
    ast,
    evaluate::{self, Value},
    grammar::ExprParser,
    translate::{FieldType, TranslationContext, postgres::Translator},
};

// Simple value lookup for evaluation
fn value_lookup<'a>() -> impl Fn(&str) -> Option<Value> {
    let value_lookup = |field_name: &str| -> Option<Value> {
        match field_name.to_uppercase().as_str() {
            "B_T" => Some(Value::Bool(true)),
            "B_F" => Some(Value::Bool(false)),
            "A" => Some(Value::Number(1.0)),
            "B" => Some(Value::Number(2.0)),
            "C" => Some(Value::Number(3.0)),
            "D" => Some(Value::Number(0.0)),
            "BINDATAFIELD" => Some(Value::Blob(vec![0x01, 0x02, 0x03])),
            "SHIP_DATE" => Some(Value::Date(chrono::NaiveDate::from_ymd_opt(2024, 8, 1))),
            "ID" => Some(Value::Str("DOEJOH".into(), 10)),
            "F_NAME" => Some(Value::Str("John".into(), 100)),
            "L_NAME" => Some(Value::Str("Smith".into(), 100)),
            "__DELETED" => Some(Value::Bool(false)),
            _ => None,
        }
    };
    value_lookup
}

pub fn translate_expr(expr: &str) {
    // Try parsing
    let parser = ExprParser::new();
    if let Ok(parsed) = parser.parse(expr) {
        let simplified = crate::ast::simplify(*parsed);
        // Evaluate, ignore errors
        let _ = evaluate::evaluate(&simplified, &value_lookup());

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
        _ = translator.translate(&simplified);
    }
}

pub fn translate_ast(expr: ast::Expression) {
    let simplified = crate::ast::simplify(expr);
    // Evaluate, ignore errors
    let _ = evaluate::evaluate(&simplified, &value_lookup());

    let translator = Translator {
        field_lookup: |_alias: Option<&str>, field: &str| -> Result<(String, FieldType), String> {
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
    _ = translator.translate(&simplified);
}

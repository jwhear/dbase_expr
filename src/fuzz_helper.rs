use crate::{
    ast,
    evaluate::{self, Value},
    grammar::ExprParser,
    translate::{FieldType, TranslationContext, postgres::Translator},
};

// Simple value lookup for evaluation
fn value_lookup() -> impl Fn(&str) -> Option<Value> {
    |field_name: &str| -> Option<Value> {
        match field_name.to_uppercase().as_str() {
            "B_T" => Some(Value::Bool(true)),
            "B_F" => Some(Value::Bool(false)),
            "A" => Some(Value::Number(1.0, false)),
            "B" => Some(Value::Number(2.0, false)),
            "C" => Some(Value::Number(3.0, false)),
            "D" => Some(Value::Number(0.0, false)),
            "BINDATAFIELD" => Some(Value::Blob(vec![0x01, 0x02, 0x03])),
            "SHIP_DATE" => Some(Value::Date(chrono::NaiveDate::from_ymd_opt(2024, 8, 1))),
            "ID" => Some(Value::FixedLenStr("DOEJOH".into(), 10)),
            "F_NAME" => Some(Value::FixedLenStr("John".into(), 100)),
            "L_NAME" => Some(Value::FixedLenStr("Smith".into(), 100)),
            "__DELETED" => Some(Value::Bool(false)),
            _ => None,
        }
    }
}

fn custom_functions() -> fn(&str) -> Option<ast::Expression> {
    custom_functions_impl
}

fn custom_functions_impl(func: &str) -> Option<ast::Expression> {
    match func.to_uppercase().as_str() {
        "USER" => Some(ast::Expression::StringLiteral("my user".to_string())),
        _ => None,
    }
}

fn field_lookup() -> impl Fn(Option<&str>, &str) -> Result<(String, FieldType), String> {
    |_alias: Option<&str>, field: &str| -> Result<(String, FieldType), String> {
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
    }
}

pub fn translate_expr(expr: &str) {
    // Try parsing
    let parser = ExprParser::new();
    if let Ok(parsed) = parser.parse(expr) {
        let simplified = crate::ast::simplify(*parsed);
        // Evaluate, ignore errors
        let _ = evaluate::evaluate(&simplified, &value_lookup(), &custom_functions());

        let translator = Translator {
            field_lookup: field_lookup(),
            custom_function: custom_functions(),
        };
        _ = translator.translate(&simplified);
    }
}

pub fn translate_ast(expr: ast::Expression) {
    let simplified = crate::ast::simplify(expr);
    let func = custom_functions();
    // Evaluate, ignore errors
    {
        let _ = evaluate::evaluate(&simplified, &value_lookup(), &func);
    }

    let translator = Translator {
        field_lookup: field_lookup(),
        custom_function: func,
    };
    _ = translator.translate(&simplified);
}

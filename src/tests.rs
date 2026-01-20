use crate::{
    ast,
    codebase_functions::CodebaseFunction,
    translate::{
        self, Error, ExprRef, Expression, FieldType, TranslationContext, expr_ref,
        postgres::{translate as default_translate, translate_binary_op, translate_fn_call},
    },
};

pub struct TestTranslator<F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
{
    pub field_lookup: F,
    pub custom_functions: fn(&str) -> Option<ast::Expression>,
}
impl<F> TranslationContext for TestTranslator<F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
{
    fn lookup_field(
        &self,
        alias: Option<&str>,
        field: &str,
    ) -> std::result::Result<(String, FieldType), String> {
        (self.field_lookup)(alias, field)
    }

    fn custom_function(&self, func: &str) -> Option<ast::Expression> {
        (self.custom_functions)(func)
    }

    fn translate(&self, source: &ast::Expression) -> translate::Result {
        default_translate(source, self)
    }

    fn translate_fn_call(
        &self,
        name: &CodebaseFunction,
        args: &[Box<ast::Expression>],
    ) -> std::result::Result<(ExprRef, FieldType), Error> {
        let arg = |index: usize| {
            args.get(index)
                .map(|a| default_translate(a, self))
                .ok_or(Error::IncorrectArgCount(format!("{:?}", name), index))
        };

        if name == &CodebaseFunction::DTOS {
            Ok((
                expr_ref(Expression::FunctionCall {
                    name: "CB_DATE_TO_TEXT".into(),
                    args: vec![arg(0)??.0, expr_ref("YYYYMMDD".into())],
                }),
                FieldType::Character(8),
            ))
        } else {
            translate_fn_call(name, args, self)
        }
    }

    fn translate_binary_op(
        &self,
        l: &ast::Expression,
        op: &ast::BinaryOp,
        r: &ast::Expression,
    ) -> translate::Result {
        translate_binary_op(self, l, op, r)
    }
}

pub fn custom_functions() -> fn(&str) -> Option<ast::Expression> {
    custom_functions_impl
}

pub fn custom_functions_impl(func: &str) -> Option<ast::Expression> {
    match func.to_uppercase().as_str() {
        "USER" => Some(ast::Expression::StringLiteral("my user".to_string())),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::{ast, *};

    #[test]
    fn substr_test() {
        let (expression, field_type) = parse_expression("substr(ID, 1, 3)").unwrap();
        match (&*expression.borrow(), field_type) {
            (Expression::FunctionCall { name, args }, FieldType::Character(3)) => {
                assert_eq!(name, "SUBSTR");
                assert_eq!(args.len(), 3);
                assert_eq!(
                    *args[0].borrow(),
                    Expression::Field {
                        name: "ID".to_string(),
                        field_type: FieldType::Character(10),
                    }
                );
                assert_eq!(
                    *args[1].borrow(),
                    Expression::NumberLiteral("1".to_string())
                );
                assert_eq!(
                    *args[2].borrow(),
                    Expression::NumberLiteral("3".to_string())
                );
            }
            _ => panic!("Expected FunctionCall, got {:?}", expression),
        }
    }

    #[test]
    fn substr_replace_0_with_1_test() {
        let (expression, field_type) = parse_expression("substr(ID, 0, 3)").unwrap();
        match (&*expression.borrow(), field_type) {
            (Expression::FunctionCall { name, args }, FieldType::Character(3)) => {
                assert_eq!(name, "SUBSTR");
                assert_eq!(args.len(), 3);
                assert_eq!(
                    *args[1].borrow(),
                    Expression::NumberLiteral("1".to_string())
                );
                assert_eq!(
                    *args[2].borrow(),
                    Expression::NumberLiteral("3".to_string())
                );
            }
            _ => panic!("Expected FunctionCall, got {:?}", expression),
        }
    }

    fn parse_expression(expr: &str) -> translate::Result {
        let parser = crate::grammar::ExprParser::new();
        let expression = ast::simplify(*parser.parse(expr).unwrap());
        let cx = TestTranslator {
            field_lookup: |alias: Option<&str>,
                           field: &str|
             -> Result<(String, FieldType), String> {
                let field = field.to_string().to_uppercase();
                let field_type = match (alias, field.as_ref()) {
                    (_, "A" | "B" | "C") => FieldType::Integer,
                    (_, "BINDATAFIELD") => FieldType::MemoBinary,
                    (_, "SHIP_DATE") => FieldType::Date,
                    (_, "ID") => FieldType::Character(10),
                    (_, "L_NAME") => FieldType::Character(20),
                    (_, "__DELETED") => FieldType::Logical,
                    (Some(alias), _) => panic!("unknown field: {alias}.{field}"),
                    (None, _) => panic!("unknown field: {field}"),
                };
                Ok((field, field_type))
            },
            custom_functions: custom_functions(),
        };
        cx.translate(&expression)
    }
}

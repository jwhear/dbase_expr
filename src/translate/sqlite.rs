use crate::{
    ast,
    codebase_functions::CodebaseFunction as F,
    translate::{
        BinaryOp, Error, Expression, FieldType, Result, TranslationContext, UnaryOp, ok,
        postgres::{
            self, get_all_args, get_arg, translate as default_translate, translate_binary_op,
            wrong_type,
        },
    },
};

pub struct SqliteTranslator<F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
{
    pub field_lookup: F,
}

impl<F> TranslationContext for SqliteTranslator<F>
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

    fn translate(&self, source: &ast::Expression) -> Result {
        default_translate(source, self)
    }

    fn translate_fn_call(
        &self,
        name: &crate::codebase_functions::CodebaseFunction,
        args: &[Box<ast::Expression>],
    ) -> std::result::Result<(Box<Expression>, FieldType), Error> {
        translate_fn_call(name, args, self)
    }

    fn translate_binary_op(
        &self,
        l: &Box<ast::Expression>,
        op: &ast::BinaryOp,
        r: &Box<ast::Expression>,
    ) -> Result {
        let binop_translated = |l, op, r, ty| ok(Expression::BinaryOperator(l, op, r), ty);
        let (translated_l, ty) = self.translate(l)?;
        match (op, ty) {
            (ast::BinaryOp::Eq, FieldType::Character(_) | FieldType::Memo) => {
                let translated_r = self.translate(r)?.0;
                let escaped_r = make_escaped_like_pattern(&translated_r, "%");
                binop_translated(translated_l, BinaryOp::Like, escaped_r, FieldType::Logical)
            }
            (ast::BinaryOp::Ne, FieldType::Character(_) | FieldType::Memo) => {
                let translated_r = self.translate(r)?.0;
                let escaped_r = make_escaped_like_pattern(&translated_r, "%");
                let starts_with =
                    Expression::BinaryOperator(translated_l, BinaryOp::Like, escaped_r);
                ok(
                    Expression::UnaryOperator(UnaryOp::Not, Box::new(starts_with)),
                    FieldType::Logical,
                )
            }
            _ => translate_binary_op(self, l, op, r),
        }
    }
}

fn make_escaped_like_pattern(expression: &Box<Expression>, suffix: &str) -> Box<Expression> {
    match expression.as_ref() {
        Expression::SingleQuoteStringLiteral(s) => {
            let escaped = s
                .replace('\\', "\\\\")
                .replace('%', "\\%")
                .replace('_', "\\_");
            Box::new(Expression::SingleQuoteStringLiteral(format!(
                "{}{}",
                escaped, suffix
            )))
        }
        _ => expression.clone(),
    }
}

pub fn translate_fn_call(
    name: &F,
    args: &[Box<ast::Expression>],
    cx: &impl TranslationContext,
) -> std::result::Result<(Box<Expression>, FieldType), Error> {
    let arg = |index: usize| get_arg(index, args, cx, name);
    let all_args = || get_all_args(args, cx);
    let wrong_type = |index| wrong_type(index, name, args);

    //these are only the ones that are different from Postgres, everything else falls through to postgres
    match name {
        F::CHR => ok(
            Expression::FunctionCall {
                name: "CHAR".to_string(), // SQLite equivalent
                args: all_args()?,
            },
            FieldType::Character(1),
        ),

        F::CTOD => ok(
            Expression::FunctionCall {
                name: "DATE".to_string(),
                args: vec![arg(0)??.0], // assumes date in ISO 8601 or needs pre-processing
            },
            FieldType::Date,
        ),

        F::DAY => ok(
            Expression::FunctionCall {
                name: "CAST".to_string(),
                args: vec![Box::new(Expression::FunctionCall {
                    name: "STRFTIME".to_string(),
                    args: vec!["'%d'".into(), arg(0)??.0],
                })],
            },
            FieldType::Double,
        ),

        F::DTOC => {
            if args.len() == 2 {
                // Equivalent to DTOS
                ok(
                    Expression::FunctionCall {
                        name: "STRFTIME".into(),
                        args: vec!["'%Y%m%d'".into(), arg(0)??.0],
                    },
                    FieldType::Character(8),
                )
            } else {
                ok(
                    Expression::FunctionCall {
                        name: "STRFTIME".into(),
                        args: vec!["'%m/%d/%y'".into(), arg(0)??.0],
                    },
                    FieldType::Character(8),
                )
            }
        }

        F::DTOS => ok(
            Expression::FunctionCall {
                name: "STRFTIME".into(),
                args: vec!["'%Y%m%d'".into(), arg(0)??.0],
            },
            FieldType::Character(8),
        ),

        F::MONTH => ok(
            Expression::FunctionCall {
                name: "CAST".to_string(),
                args: vec![Box::new(Expression::FunctionCall {
                    name: "STRFTIME".into(),
                    args: vec!["'%m'".into(), arg(0)??.0],
                })],
            },
            FieldType::Double,
        ),

        F::RIGHT => {
            let (x, ty) = arg(0)??;
            let n = match arg(1)??.0.as_ref() {
                Expression::NumberLiteral(v) => {
                    u32::from_str_radix(&v, 10).map_err(|_| wrong_type(1))
                }
                _ => Err(wrong_type(1)),
            }?;
            let out_ty = match ty {
                FieldType::Character(len) => FieldType::Character(len - n),
                _ => FieldType::Memo,
            };
            ok(
                Expression::FunctionCall {
                    name: "SUBSTR".into(),
                    args: vec![x, (-i64::try_from(n).unwrap()).into()],
                },
                out_ty,
            )
        }

        F::STOD => {
            // Convert 'YYYYMMDD' -> 'YYYY-MM-DD' using SUBSTR
            ok(
                Expression::FunctionCall {
                    name: "DATE".into(),
                    args: vec![Box::new(Expression::FunctionCall {
                        name: "printf".into(),
                        args: vec![
                            "'%s-%s-%s'".into(),
                            Box::new(Expression::FunctionCall {
                                name: "SUBSTR".into(),
                                args: vec![arg(0)??.0.clone(), 1.into(), 4.into()],
                            }),
                            Box::new(Expression::FunctionCall {
                                name: "SUBSTR".into(),
                                args: vec![arg(0)??.0.clone(), 5.into(), 2.into()],
                            }),
                            Box::new(Expression::FunctionCall {
                                name: "SUBSTR".into(),
                                args: vec![arg(0)??.0, 7.into(), 2.into()],
                            }),
                        ],
                    })],
                },
                FieldType::Date,
            )
        }

        F::STR => {
            let len = match arg(1)??.0.as_ref() {
                Expression::NumberLiteral(v) => {
                    i64::from_str_radix(&v, 10).map_err(|_| wrong_type(1))
                }
                _ => Err(wrong_type(1)),
            }?;
            let dec = match arg(2)??.0.as_ref() {
                Expression::NumberLiteral(v) => {
                    i64::from_str_radix(&v, 10).map_err(|_| wrong_type(2))
                }
                _ => Err(wrong_type(2)),
            }?;
            let fmt = format!("%{}.{}f", len, dec); // e.g. "%.2f"
            ok(
                Expression::FunctionCall {
                    name: "PRINTF".to_string(),
                    args: vec![fmt.into(), arg(0)??.0],
                },
                FieldType::Character(len as u32),
            )
        }

        F::VAL => ok(
            Expression::Cast(arg(0)??.0, "REAL"),
            FieldType::Numeric { len: 0, dec: 0 },
        ),

        F::YEAR => ok(
            Expression::FunctionCall {
                name: "CAST".to_string(),
                args: vec![Box::new(Expression::FunctionCall {
                    name: "STRFTIME".into(),
                    args: vec!["'%Y'".into(), arg(0)??.0],
                })],
            },
            FieldType::Double,
        ),

        other => postgres::translate_fn_call(other, args, cx),
    }
}

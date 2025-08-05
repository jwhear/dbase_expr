use crate::{
    ast,
    translate::{
        Error, Expression, FieldType, Result, TranslationContext, ok,
        postgres::translate as default_translate,
    },
};

/// This type provides default function translation for SQLite. You can
///  "inherit" while allowing overriding by implementing the TranslationContext
///  trait and dispatching to `default_translate_fn_call` any function calls
///  you're not interested in overriding.
pub struct DefaultSqliteTranslator<F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
{
    pub field_lookup: F,
}

impl<F> TranslationContext for DefaultSqliteTranslator<F>
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
        name: &str,
        args: &[Box<ast::Expression>],
    ) -> std::result::Result<(Box<Expression>, FieldType), Error> {
        translate_fn_call(name, args, self)
    }
}

// This function does the kind of gross work of converting dBase function calls
//  to the SQL equivalent.  Some are super straightforward: `CHR(97)` -> `CHR(97)`
//  but others have no exact equivalent and have to resolve to a nested bundle.
pub fn translate_fn_call(
    name: &str,
    args: &[Box<ast::Expression>],
    cx: &impl TranslationContext,
) -> std::result::Result<(Box<Expression>, FieldType), Error> {
    let name = name.to_uppercase();
    // Helper to get the specified argument or return the appropriate error
    let arg = |index: usize| {
        args.get(index)
            .map(|a| default_translate(a, cx))
            .ok_or(Error::IncorrectArgCount(name.clone(), index))
    };

    // Translates all arguments and puts them into a Result<Vec>
    let all_args = || {
        let res: std::result::Result<Vec<_>, Error> = args
            .iter()
            .map(|a| default_translate(a, cx).map(|r| r.0))
            .collect();
        res
    };

    // Helper for constructing an error if the CodeBase function was called with
    //  an incorrect argument type (usually a function that requires an integer
    //  or string literal)
    let wrong_type = |index| Error::ArgWrongType {
        func: ast::Expression::FunctionCall {
            name: name.clone(),
            args: args.into(),
        },
        wrong_arg_index: index,
    };

    match name.as_str() {
        "ALLTRIM" => ok(
            Expression::FunctionCall {
                name: "TRIM".to_string(),
                args: vec![arg(0)??.0],
            },
            FieldType::Memo,
        ),

        "CHR" => ok(
            Expression::FunctionCall {
                name: "CHAR".to_string(), // SQLite equivalent
                args: all_args()?,
            },
            FieldType::Character(1),
        ),

        "CTOD" => ok(
            Expression::FunctionCall {
                name: "DATE".to_string(),
                args: vec![arg(0)??.0], // assumes date in ISO 8601 or needs pre-processing
            },
            FieldType::Date,
        ),

        "DATE" => ok(
            Expression::BareFunctionCall("CURRENT_DATE".to_string()),
            FieldType::Date,
        ),

        "DAY" => ok(
            Expression::FunctionCall {
                name: "CAST".to_string(),
                args: vec![Box::new(Expression::FunctionCall {
                    name: "STRFTIME".to_string(),
                    args: vec!["'%d'".into(), arg(0)??.0],
                })],
            },
            FieldType::Double,
        ),

        "DELETED" => ok(
            Expression::Field {
                alias: None,
                name: "__deleted".into(),
                field_type: FieldType::Logical,
            },
            FieldType::Logical,
        ),

        "DTOC" => {
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

        "DTOS" => ok(
            Expression::FunctionCall {
                name: "STRFTIME".into(),
                args: vec!["'%Y%m%d'".into(), arg(0)??.0],
            },
            FieldType::Character(8),
        ),

        "IIF" => {
            let (when_true, ty) = arg(1)??;
            ok(
                Expression::Iif {
                    cond: arg(0)??.0,
                    when_true,
                    when_false: arg(2)??.0,
                },
                ty,
            )
        }

        "LEFT" => ok(
            Expression::FunctionCall {
                name: "SUBSTR".to_string(),
                args: vec![arg(0)??.0, 1.into(), arg(1)??.0],
            },
            FieldType::Memo,
        ),

        "LTRIM" => ok(
            Expression::FunctionCall {
                name: "LTRIM".into(),
                args: vec![arg(0)??.0],
            },
            FieldType::Memo,
        ),

        "MONTH" => ok(
            Expression::FunctionCall {
                name: "CAST".to_string(),
                args: vec![Box::new(Expression::FunctionCall {
                    name: "STRFTIME".into(),
                    args: vec!["'%m'".into(), arg(0)??.0],
                })],
            },
            FieldType::Double,
        ),

        "RECNO" => ok(
            Expression::Field {
                alias: None,
                name: "RECNO5".into(),
                field_type: FieldType::Integer,
            },
            FieldType::Integer,
        ),

        "RIGHT" => {
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

        "STOD" => {
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

        "STR" => {
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

        "SUBSTR" => {
            let len = match arg(2)??.0.as_ref() {
                Expression::NumberLiteral(v) => {
                    u32::from_str_radix(&v, 10).map_err(|_| wrong_type(2))
                }
                _ => Err(wrong_type(2)),
            }?;
            ok(
                Expression::FunctionCall {
                    name: "SUBSTR".into(),
                    args: all_args()?,
                },
                FieldType::Character(len),
            )
        }

        "TRIM" => ok(
            Expression::FunctionCall {
                name: "RTRIM".into(),
                args: vec![arg(0)??.0],
            },
            FieldType::Memo,
        ),

        "UPPER" => {
            let (first, ty) = arg(0)??;
            ok(
                Expression::FunctionCall {
                    name: "UPPER".into(),
                    args: vec![first],
                },
                ty,
            )
        }

        "VAL" => ok(
            Expression::Cast(arg(0)??.0, "REAL"),
            FieldType::Numeric { len: 0, dec: 0 },
        ),

        "YEAR" => ok(
            Expression::FunctionCall {
                name: "CAST".to_string(),
                args: vec![Box::new(Expression::FunctionCall {
                    name: "STRFTIME".into(),
                    args: vec!["'%Y'".into(), arg(0)??.0],
                })],
            },
            FieldType::Double,
        ),

        _ => Err(Error::UnsupportedFunction(name)),
    }
}

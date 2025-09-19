use crate::{
    ast,
    codebase_functions::CodebaseFunction as F,
    translate::{
        BinaryOp, COALESCE_DATE, Error, ExprRef, Expression, FieldType, Parenthesize, Result,
        TranslationContext, expr_ref, ok,
        postgres::{
            self, get_all_args, get_arg, translate as default_translate, translate_binary_op,
            wrong_type,
        },
    },
};

pub struct SqliteTranslator<F, C>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
    C: Fn(&str) -> Option<ast::Expression>,
{
    pub field_lookup: F,
    pub custom_function: C,
}

impl<F, C> TranslationContext for SqliteTranslator<F, C>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
    C: Fn(&str) -> Option<ast::Expression>,
{
    fn lookup_field(
        &self,
        alias: Option<&str>,
        field: &str,
    ) -> std::result::Result<(String, FieldType), String> {
        (self.field_lookup)(alias, field)
    }

    fn custom_function(&self, func: &str) -> Option<ast::Expression> {
        (self.custom_function)(func)
    }

    fn translate(&self, source: &ast::Expression) -> Result {
        default_translate(source, self)
    }

    fn translate_fn_call(
        &self,
        name: &crate::codebase_functions::CodebaseFunction,
        args: &[Box<ast::Expression>],
    ) -> std::result::Result<(ExprRef, FieldType), Error> {
        translate_fn_call(name, args, self)
    }

    fn translate_binary_op(
        &self,
        l: &ast::Expression,
        op: &ast::BinaryOp,
        r: &ast::Expression,
    ) -> Result {
        let (translated_l, ty) = self.translate(l)?;
        match (op, ty) {
            (
                op @ (ast::BinaryOp::Eq | ast::BinaryOp::Ne),
                FieldType::Character(_) | FieldType::Memo,
            ) => {
                let translated_r = self.translate(r)?.0;
                let modified_r = expr_between_right_side(translated_r);
                let binop = match op {
                    ast::BinaryOp::Eq => BinaryOp::Between,
                    ast::BinaryOp::Ne => BinaryOp::NotBetween,
                    _ => unreachable!(),
                };
                ok(
                    Expression::BinaryOperator(translated_l, binop, modified_r, Parenthesize::Yes),
                    FieldType::Logical,
                )
            }
            _ => translate_binary_op(self, l, op, r),
        }
    }
}

fn expr_between_right_side(expression: ExprRef) -> ExprRef {
    let char = Expression::BareFunctionCall("char(0xFFFF)".to_string());
    let appended = Expression::BinaryOperator(
        expression.clone(),
        BinaryOp::Concat,
        expr_ref(char),
        Parenthesize::No,
    );
    let combined = Expression::BinaryOperator(
        expression,
        BinaryOp::And,
        expr_ref(appended),
        Parenthesize::No,
    );
    expr_ref(combined)
}

pub fn translate_fn_call(
    name: &F,
    args: &[Box<ast::Expression>],
    cx: &impl TranslationContext,
) -> std::result::Result<(ExprRef, FieldType), Error> {
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

        F::CTOD => {
            //COALESCE(DATE(NULLIF(TRIM(x),''),'0001-01-01')
            let trim = Expression::FunctionCall {
                name: "TRIM".into(),
                args: vec![arg(0)??.0],
            };
            let null_if = Expression::FunctionCall {
                name: "NULLIF".into(),
                args: vec![expr_ref(trim), expr_ref("".into())],
            };
            // Convert format -> 'YYYY-MM-DD' using SUBSTR
            let date = Expression::FunctionCall {
                name: "DATE".into(),
                args: vec![expr_ref(Expression::FunctionCall {
                    name: "printf".into(),
                    args: vec![expr_ref(null_if)], // assumes date in ISO 8601 or needs pre-processing
                })],
            };
            let coalesce = Expression::FunctionCall {
                name: "COALESCE".into(),
                args: vec![expr_ref(date), expr_ref(COALESCE_DATE.into())],
            };
            ok(coalesce, FieldType::Date)
        }

        F::DAY => ok(
            Expression::FunctionCall {
                name: "CAST".to_string(),
                args: vec![expr_ref(Expression::FunctionCall {
                    name: "STRFTIME".to_string(),
                    args: vec![expr_ref("'%d'".into()), arg(0)??.0],
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
                        args: vec![expr_ref("%Y%m%d".into()), arg(0)??.0],
                    },
                    FieldType::Character(8),
                )
            } else {
                ok(
                    Expression::FunctionCall {
                        name: "STRFTIME".into(),
                        args: vec![expr_ref("'%m/%d/%y'".into()), arg(0)??.0],
                    },
                    FieldType::Character(8),
                )
            }
        }

        F::DTOS => ok(
            Expression::FunctionCall {
                name: "STRFTIME".into(),
                args: vec![expr_ref("%Y%m%d".into()), arg(0)??.0],
            },
            FieldType::Character(8),
        ),

        F::MONTH => ok(
            Expression::FunctionCall {
                name: "CAST".to_string(),
                args: vec![expr_ref(Expression::FunctionCall {
                    name: "STRFTIME".into(),
                    args: vec![expr_ref("'%m'".into()), arg(0)??.0],
                })],
            },
            FieldType::Double,
        ),

        F::RIGHT => {
            let (x, ty) = arg(0)??;
            let n: u32 = match &*arg(1)??.0.borrow() {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(1)),
                _ => Err(wrong_type(1)),
            }?;
            let out_ty = match ty {
                FieldType::Character(len) => FieldType::Character(len - n),
                _ => FieldType::Memo,
            };
            ok(
                Expression::FunctionCall {
                    name: "SUBSTR".into(),
                    args: vec![x, expr_ref((-i64::from(n)).into())],
                },
                out_ty,
            )
        }
        F::STOD => {
            //COALESCE(DATE(SUBSTR(TRIM(x),1,4),SUBSTR(TRIM(x),5,2),SUBSTR(TRIM(x),7,2)),'0001-01-01')
            let trim = expr_ref(Expression::FunctionCall {
                name: "TRIM".into(),
                args: vec![arg(0)??.0],
            });
            // Convert format -> 'YYYY-MM-DD' using SUBSTR
            let date = Expression::FunctionCall {
                name: "DATE".into(),
                args: vec![
                    expr_ref(Expression::FunctionCall {
                        name: "SUBSTR".into(),
                        args: vec![trim.clone(), expr_ref(1.into()), expr_ref(4.into())],
                    }),
                    expr_ref(Expression::FunctionCall {
                        name: "SUBSTR".into(),
                        args: vec![trim.clone(), expr_ref(5.into()), expr_ref(2.into())],
                    }),
                    expr_ref(Expression::FunctionCall {
                        name: "SUBSTR".into(),
                        args: vec![trim, expr_ref(7.into()), expr_ref(2.into())],
                    }),
                ],
            };
            let coalesce = Expression::FunctionCall {
                name: "COALESCE".into(),
                args: vec![expr_ref(date), expr_ref(COALESCE_DATE.into())],
            };
            ok(coalesce, FieldType::Date)
        }
        F::STR => {
            let len: i64 = match &*arg(1)??.0.borrow() {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(1)),
                _ => Err(wrong_type(1)),
            }?;
            let dec: i64 = match &*arg(2)??.0.borrow() {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(2)),
                _ => Err(wrong_type(2)),
            }?;
            let fmt = format!("%{}.{}f", len, dec); // e.g. "%.2f"
            ok(
                Expression::FunctionCall {
                    name: "PRINTF".to_string(),
                    args: vec![expr_ref(fmt.into()), arg(0)??.0],
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
                args: vec![expr_ref(Expression::FunctionCall {
                    name: "STRFTIME".into(),
                    args: vec![expr_ref("'%Y'".into()), arg(0)??.0],
                })],
            },
            FieldType::Double,
        ),

        other => postgres::translate_fn_call(other, args, cx),
    }
}

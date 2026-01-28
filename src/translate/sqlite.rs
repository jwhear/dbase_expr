use crate::{
    codebase_functions::CodebaseFunction as F,
    parser::{self, ParseTree},
    translate::{
        BinaryOp as TranslateBinaryOp, COALESCE_DATE, Error, ExprRef,
        Expression as TranslateExpression, FieldType, Parenthesize, Result, TranslationContext,
        expr_ref, ok,
        postgres::{
            self, get_all_args, get_arg, translate as default_translate, translate_binary_op_right,
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

    fn translate(&self, source: &parser::Expression, tree: &ParseTree) -> Result {
        default_translate(source, tree, self)
    }

    fn translate_fn_call(
        &self,
        name: &crate::codebase_functions::CodebaseFunction,
        args: &[parser::ExpressionId],
        tree: &ParseTree,
    ) -> std::result::Result<(ExprRef, FieldType), Error> {
        translate_fn_call(name, args, tree, self)
    }

    fn translate_binary_op(
        &self,
        l: &parser::Expression,
        op: &parser::BinaryOp,
        r: &parser::Expression,
        tree: &ParseTree,
    ) -> Result {
        let (translated_l, ty) = self.translate(l, tree)?;
        match (op, ty) {
            (
                op @ (parser::BinaryOp::Eq | parser::BinaryOp::Ne),
                FieldType::Character(_) | FieldType::Memo,
            ) => {
                let translated_r = self.translate(r, tree)?.0;
                let modified_r = expr_between_right_side(translated_r);
                let binop = match op {
                    parser::BinaryOp::Eq => TranslateBinaryOp::Between,
                    parser::BinaryOp::Ne => TranslateBinaryOp::NotBetween,
                    _ => unreachable!(),
                };
                ok(
                    TranslateExpression::BinaryOperator(
                        translated_l,
                        binop,
                        modified_r,
                        Parenthesize::Yes,
                    ),
                    FieldType::Logical,
                )
            }
            _ => translate_binary_op_right(self, l, translated_l, ty, op, r, tree),
        }
    }
}

fn expr_between_right_side(expression: ExprRef) -> ExprRef {
    let char = TranslateExpression::BareFunctionCall("char(0xFFFF)".to_string());
    let appended = TranslateExpression::BinaryOperator(
        expression.clone(),
        TranslateBinaryOp::Concat,
        expr_ref(char),
        Parenthesize::No,
    );
    let combined = TranslateExpression::BinaryOperator(
        expression,
        TranslateBinaryOp::And,
        expr_ref(appended),
        Parenthesize::No,
    );
    expr_ref(combined)
}

pub fn translate_fn_call<'a>(
    name: &'a F,
    args: &'a [parser::ExpressionId],
    tree: &'a ParseTree<'a>,
    cx: &'a impl TranslationContext,
) -> std::result::Result<(ExprRef, FieldType), Error> {
    let arg = |index: usize| get_arg(index, args, tree, cx, name);
    let all_args = || get_all_args(args, tree, cx);
    let wrong_type = |index| wrong_type(index, name, args);

    //these are only the ones that are different from Postgres, everything else falls through to postgres
    match name {
        F::CHR => ok(
            TranslateExpression::FunctionCall {
                name: "CHAR".to_string(), // SQLite equivalent
                args: all_args()?,
            },
            FieldType::Character(1),
        ),

        F::CTOD => {
            //COALESCE(DATE(NULLIF(TRIM(x),''),'0001-01-01')
            let trim = TranslateExpression::FunctionCall {
                name: "TRIM".into(),
                args: vec![arg(0)??.0],
            };
            let null_if = TranslateExpression::FunctionCall {
                name: "NULLIF".into(),
                args: vec![expr_ref(trim), expr_ref("".into())],
            };
            // Convert format -> 'YYYY-MM-DD' using SUBSTR
            let date = TranslateExpression::FunctionCall {
                name: "DATE".into(),
                args: vec![expr_ref(TranslateExpression::FunctionCall {
                    name: "printf".into(),
                    args: vec![expr_ref(null_if)], // assumes date in ISO 8601 or needs pre-processing
                })],
            };
            let coalesce = TranslateExpression::FunctionCall {
                name: "COALESCE".into(),
                args: vec![expr_ref(date), expr_ref(COALESCE_DATE.into())],
            };
            ok(coalesce, FieldType::Date)
        }

        F::DAY => ok(
            TranslateExpression::FunctionCall {
                name: "CAST".to_string(),
                args: vec![expr_ref(TranslateExpression::FunctionCall {
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
                    TranslateExpression::FunctionCall {
                        name: "STRFTIME".into(),
                        args: vec![expr_ref("%Y%m%d".into()), arg(0)??.0],
                    },
                    FieldType::Character(8),
                )
            } else {
                ok(
                    TranslateExpression::FunctionCall {
                        name: "STRFTIME".into(),
                        args: vec![expr_ref("'%m/%d/%y'".into()), arg(0)??.0],
                    },
                    FieldType::Character(8),
                )
            }
        }

        F::DTOS => ok(
            TranslateExpression::FunctionCall {
                name: "STRFTIME".into(),
                args: vec![expr_ref("%Y%m%d".into()), arg(0)??.0],
            },
            FieldType::Character(8),
        ),

        F::MONTH => ok(
            TranslateExpression::FunctionCall {
                name: "CAST".to_string(),
                args: vec![expr_ref(TranslateExpression::FunctionCall {
                    name: "STRFTIME".into(),
                    args: vec![expr_ref("'%m'".into()), arg(0)??.0],
                })],
            },
            FieldType::Double,
        ),

        F::RIGHT => {
            let (x, ty) = arg(0)??;
            let n: u32 = match &*arg(1)??.0.borrow() {
                TranslateExpression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(1)),
                _ => Err(wrong_type(1)),
            }?;
            let out_ty = match ty {
                FieldType::Character(len) => FieldType::Character(len - n),
                _ => FieldType::Memo,
            };
            ok(
                TranslateExpression::FunctionCall {
                    name: "SUBSTR".into(),
                    args: vec![x, expr_ref((-i64::from(n)).into())],
                },
                out_ty,
            )
        }
        F::STOD => {
            //COALESCE(DATE(SUBSTR(TRIM(x),1,4),SUBSTR(TRIM(x),5,2),SUBSTR(TRIM(x),7,2)),'0001-01-01')
            let trim = expr_ref(TranslateExpression::FunctionCall {
                name: "TRIM".into(),
                args: vec![arg(0)??.0],
            });
            // Convert format -> 'YYYY-MM-DD' using SUBSTR
            let date = TranslateExpression::FunctionCall {
                name: "DATE".into(),
                args: vec![
                    expr_ref(TranslateExpression::FunctionCall {
                        name: "SUBSTR".into(),
                        args: vec![trim.clone(), expr_ref(1.into()), expr_ref(4.into())],
                    }),
                    expr_ref(TranslateExpression::FunctionCall {
                        name: "SUBSTR".into(),
                        args: vec![trim.clone(), expr_ref(5.into()), expr_ref(2.into())],
                    }),
                    expr_ref(TranslateExpression::FunctionCall {
                        name: "SUBSTR".into(),
                        args: vec![trim, expr_ref(7.into()), expr_ref(2.into())],
                    }),
                ],
            };
            let coalesce = TranslateExpression::FunctionCall {
                name: "COALESCE".into(),
                args: vec![expr_ref(date), expr_ref(COALESCE_DATE.into())],
            };
            ok(coalesce, FieldType::Date)
        }
        F::STR => {
            let len: i64 = match &*arg(1)??.0.borrow() {
                TranslateExpression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(1)),
                _ => Err(wrong_type(1)),
            }?;
            let dec: i64 = match &*arg(2)??.0.borrow() {
                TranslateExpression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(2)),
                _ => Err(wrong_type(2)),
            }?;
            let fmt = format!("%{}.{}f", len, dec); // e.g. "%.2f"
            ok(
                TranslateExpression::FunctionCall {
                    name: "PRINTF".to_string(),
                    args: vec![expr_ref(fmt.into()), arg(0)??.0],
                },
                FieldType::Character(len as u32),
            )
        }

        F::VAL => ok(
            TranslateExpression::Cast(arg(0)??.0, "REAL"),
            FieldType::Numeric { len: 0, dec: 0 },
        ),

        F::YEAR => ok(
            TranslateExpression::FunctionCall {
                name: "CAST".to_string(),
                args: vec![expr_ref(TranslateExpression::FunctionCall {
                    name: "STRFTIME".into(),
                    args: vec![expr_ref("'%Y'".into()), arg(0)??.0],
                })],
            },
            FieldType::Double,
        ),

        other => postgres::translate_fn_call(other, args, tree, cx),
    }
}

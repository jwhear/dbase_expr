use crate::{
    ast,
    codebase_functions::CodebaseFunction as F,
    translate::{
        COALESCE_DATE, Error, ExprRef, Expression, FieldType, Result, TranslationContext, expr_ref,
        ok,
        postgres::{self, get_all_args, get_arg, translate as default_translate, wrong_type},
    },
};

pub struct MssqlTranslator<F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
{
    pub field_lookup: F,
}

impl<F> TranslationContext for MssqlTranslator<F>
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

    fn custom_function(&self, _func: &str) -> Option<ast::Expression> {
        None //not used here
    }

    fn translate(&self, source: &ast::Expression) -> Result {
        match source {
            ast::Expression::Sequence(operands, concat_op) => {
                let (first_expr, first_ty) = self.translate(&operands[0])?;
                match first_ty {
                    FieldType::Date => self.translate_date_sequence(
                        first_expr,
                        first_ty,
                        &operands[1..],
                        concat_op,
                    ),
                    FieldType::Character(_) | FieldType::Memo | FieldType::MemoBinary => self
                        .translate_string_sequence(first_expr, first_ty, &operands[1..], concat_op),
                    _ => default_translate(source, self),
                }
            }
            _ => default_translate(source, self),
        }
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
        translate_binary_op(self, l, op, r)
    }

    fn string_comp_left(&self, l: ExprRef, r: ExprRef) -> ExprRef {
        let right_side_len_expression = expr_ref(Expression::FunctionCall {
            name: "LEN".into(),
            args: vec![r],
        });
        expr_ref(Expression::FunctionCall {
            name: "SUBSTRING".into(),
            args: vec![
                l,
                expr_ref(Expression::NumberLiteral("1".into())),
                right_side_len_expression,
            ],
        })
    }

    fn string_comp_right(&self, r: ExprRef, len: u32) -> ExprRef {
        expr_ref(Expression::FunctionCall {
            name: "SUBSTRING".into(),
            args: vec![
                r,
                expr_ref(Expression::NumberLiteral("1".to_string())),
                expr_ref(Expression::NumberLiteral(len.to_string())),
            ],
        })
    }
}

impl<F> MssqlTranslator<F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
{
    fn translate_date_sequence(
        &self,
        first_expr: ExprRef,
        first_ty: FieldType,
        remaining_operands: &[Box<ast::Expression>],
        concat_op: &ast::ConcatOp,
    ) -> Result {
        let mut result = (first_expr, first_ty);
        for operand in remaining_operands {
            let (right_expr, right_ty) = self.translate(operand)?;
            match (concat_op, &right_ty) {
                (ast::ConcatOp::Add, ty) if is_numeric_type(ty) => {
                    result = (
                        expr_ref(dateadd_expr("day", right_expr, result.0)),
                        FieldType::Date,
                    );
                }
                (ast::ConcatOp::Sub, ty) if is_numeric_type(ty) => {
                    let negated_amount = expr_ref(Expression::UnaryOperator(
                        crate::translate::UnaryOp::Neg,
                        right_expr,
                    ));
                    result = (
                        expr_ref(dateadd_expr("day", negated_amount, result.0)),
                        FieldType::Date,
                    );
                }
                (ast::ConcatOp::Sub, FieldType::Date) => {
                    result = (
                        expr_ref(datediff_expr("day", right_expr, result.0)),
                        FieldType::Numeric { len: 99, dec: 0 },
                    );
                }
                _ => {
                    // Not a supported date operation, reconstruct the sequence and fall back to default
                    let mut all_operands =
                        vec![Box::new(ast::Expression::NumberLiteral("0".to_string()))];
                    all_operands.extend_from_slice(remaining_operands);
                    return default_translate(
                        &ast::Expression::Sequence(all_operands, *concat_op),
                        self,
                    );
                }
            }
        }
        Ok(result)
    }

    fn translate_string_sequence(
        &self,
        first_expr: ExprRef,
        first_ty: FieldType,
        remaining_operands: &[Box<ast::Expression>],
        concat_op: &ast::ConcatOp,
    ) -> Result {
        let mut all_exprs = vec![first_expr];
        let mut result_type = first_ty;

        for operand in remaining_operands {
            let (right_expr, _right_ty) = self.translate(operand)?;
            match concat_op {
                ast::ConcatOp::Add => {
                    all_exprs.push(right_expr);
                }
                ast::ConcatOp::Sub => {
                    if all_exprs.len() == 1 {
                        // First subtraction, use the helper function
                        let left_expr = all_exprs.pop().unwrap();
                        return Ok((
                            expr_ref(string_subtract_expr(left_expr, right_expr)),
                            FieldType::Memo,
                        ));
                    } else {
                        // Additional operations after subtraction - just concatenate
                        all_exprs.push(right_expr);
                    }
                }
            }
        }

        if all_exprs.len() > 1 {
            result_type = FieldType::Memo;
            Ok((expr_ref(concat_expr(all_exprs)), result_type))
        } else {
            Ok((all_exprs.into_iter().next().unwrap(), result_type))
        }
    }
}

fn dateadd_expr(interval: &str, amount: ExprRef, date: ExprRef) -> Expression {
    Expression::FunctionCall {
        name: "DATEADD".to_string(),
        args: vec![
            expr_ref(Expression::BareFunctionCall(interval.to_string())),
            amount,
            date,
        ],
    }
}

fn datediff_expr(interval: &str, start_date: ExprRef, end_date: ExprRef) -> Expression {
    Expression::FunctionCall {
        name: "DATEDIFF".to_string(),
        args: vec![
            expr_ref(Expression::BareFunctionCall(interval.to_string())),
            start_date,
            end_date,
        ],
    }
}

fn concat_expr(args: Vec<ExprRef>) -> Expression {
    Expression::FunctionCall {
        name: "CONCAT".to_string(),
        args,
    }
}

fn string_subtract_expr(left: ExprRef, right: ExprRef) -> Expression {
    let without_spaces = expr_ref(Expression::FunctionCall {
        name: "RTRIM".to_string(),
        args: vec![left.clone()],
    });
    let length_without_spaces = expr_ref(Expression::FunctionCall {
        name: "LEN".to_string(),
        args: vec![without_spaces.clone()],
    });
    let length_with_spaces = expr_ref(Expression::FunctionCall {
        name: "DATALENGTH".to_string(),
        args: vec![left],
    });
    let num_spaces = expr_ref(Expression::BinaryOperator(
        length_with_spaces,
        crate::translate::BinaryOp::Sub,
        length_without_spaces,
        crate::translate::Parenthesize::No,
    ));
    let repeated_spaces = expr_ref(Expression::FunctionCall {
        name: "REPLICATE".to_string(),
        args: vec![
            expr_ref(Expression::SingleQuoteStringLiteral(" ".to_string())),
            num_spaces,
        ],
    });
    concat_expr(vec![without_spaces, right, repeated_spaces])
}

fn is_numeric_type(ty: &FieldType) -> bool {
    matches!(
        ty,
        FieldType::Integer | FieldType::Double | FieldType::Float | FieldType::Numeric { .. }
    )
}

fn is_string_type(ty: &FieldType) -> bool {
    matches!(ty, FieldType::Character(_) | FieldType::Memo)
}

pub fn translate_binary_op<T: TranslationContext>(
    cx: &T,
    l: &ast::Expression,
    op: &ast::BinaryOp,
    r: &ast::Expression,
) -> Result {
    let (translated_l, ty_l) = cx.translate(l)?;

    match (op, &ty_l) {
        // Date arithmetic
        (ast::BinaryOp::Add, FieldType::Date) => {
            let (translated_r, ty_r) = cx.translate(r)?;
            if is_numeric_type(&ty_r) {
                ok(
                    dateadd_expr("day", translated_r, translated_l),
                    FieldType::Date,
                )
            } else {
                postgres::translate_binary_op_right(cx, l, translated_l, ty_l, op, r)
            }
        }
        (ast::BinaryOp::Sub, FieldType::Date) => {
            let (translated_r, ty_r) = cx.translate(r)?;
            if is_numeric_type(&ty_r) {
                let amount = expr_ref(Expression::UnaryOperator(
                    crate::translate::UnaryOp::Neg,
                    translated_r,
                ));
                ok(dateadd_expr("day", amount, translated_l), FieldType::Date)
            } else if matches!(ty_r, FieldType::Date) {
                ok(
                    datediff_expr("day", translated_r, translated_l),
                    FieldType::Numeric { len: 99, dec: 0 },
                )
            } else {
                postgres::translate_binary_op_right(cx, l, translated_l, ty_l, op, r)
            }
        }

        // String operations
        (ast::BinaryOp::Add, ty_l) if is_string_type(ty_l) => {
            let (translated_r, _) = cx.translate(r)?;
            ok(
                concat_expr(vec![translated_l, translated_r]),
                FieldType::Memo,
            )
        }
        (ast::BinaryOp::Sub, ty_l) if is_string_type(ty_l) => {
            let (translated_r, _) = cx.translate(r)?;
            ok(
                string_subtract_expr(translated_l, translated_r),
                FieldType::Memo,
            )
        }

        // Contains operation using CHARINDEX (MSSQL equivalent of STRPOS)
        // Note: In CodeBase the haystack is the right operand, needle is left
        (ast::BinaryOp::Contain, ty_l) if is_string_type(ty_l) => {
            let (translated_r, _) = cx.translate(r)?;
            let charindex = expr_ref(Expression::FunctionCall {
                name: "CHARINDEX".to_string(),
                args: vec![translated_l, translated_r], // needle, haystack
            });
            // Use CASE WHEN for maximum SQL Server compatibility
            ok(
                Expression::Case {
                    branches: vec![crate::translate::When {
                        cond: expr_ref(Expression::BinaryOperator(
                            charindex,
                            crate::translate::BinaryOp::Gt,
                            expr_ref(Expression::NumberLiteral("0".to_string())),
                            crate::translate::Parenthesize::No,
                        )),
                        then: expr_ref(Expression::NumberLiteral("1".to_string())),
                    }],
                    r#else: expr_ref(Expression::NumberLiteral("0".to_string())),
                },
                FieldType::Logical,
            )
        }

        // For all other operations, delegate to postgres implementation
        _ => postgres::translate_binary_op_right(cx, l, translated_l, ty_l, op, r),
    }
}

pub fn translate_fn_call(
    name: &F,
    args: &[Box<ast::Expression>],
    cx: &impl TranslationContext,
) -> std::result::Result<(ExprRef, FieldType), Error> {
    let arg = |index: usize| get_arg(index, args, cx, name);
    let all_args = || get_all_args(args, cx);
    let wrong_type = |index| wrong_type(index, name, args);

    // These are only the ones that are different from Postgres, everything else falls through to postgres
    match name {
        // In SQL Server, CHR is called CHAR
        F::CHR => ok(
            Expression::FunctionCall {
                name: "CHAR".to_string(),
                args: all_args()?,
            },
            FieldType::Character(1),
        ),

        // CTOD(x) => CONVERT(date, x, 101) -- format 101 is MM/DD/YY
        F::CTOD => ok(
            Expression::FunctionCall {
                name: "CONVERT".into(),
                args: vec![
                    expr_ref(Expression::BareFunctionCall("date".to_string())),
                    arg(0)??.0,
                    expr_ref(Expression::NumberLiteral("101".to_string())), // MM/DD/YY format
                ],
            },
            FieldType::Date,
        ),

        // DATE() => CAST(GETDATE() AS date)
        F::DATE => ok(
            Expression::Cast(
                expr_ref(Expression::BareFunctionCall("GETDATE()".to_string())),
                "date",
            ),
            FieldType::Date,
        ),

        // DAY(x) => DAY(x) -- SQL Server has this function
        F::DAY => ok(
            Expression::FunctionCall {
                name: "DAY".into(),
                args: vec![arg(0)??.0],
            },
            FieldType::Double,
        ),

        // DTOC(x) => FORMAT(x, 'MM/dd/yy') or FORMAT(x, 'yyyyMMdd')
        F::DTOC => {
            if args.len() == 2 {
                // Equivalent to DTOS
                ok(
                    Expression::FunctionCall {
                        name: "FORMAT".into(),
                        args: vec![arg(0)??.0, expr_ref("yyyyMMdd".into())],
                    },
                    FieldType::Character(8),
                )
            } else {
                ok(
                    Expression::FunctionCall {
                        name: "FORMAT".into(),
                        args: vec![arg(0)??.0, expr_ref("MM/dd/yy".into())],
                    },
                    FieldType::Character(8),
                )
            }
        }

        F::DTOS => ok(
            Expression::FunctionCall {
                name: "FORMAT".into(),
                args: vec![arg(0)??.0, expr_ref("yyyyMMdd".into())],
            },
            FieldType::Character(8),
        ),

        // MONTH(x) => MONTH(x)
        F::MONTH => ok(
            Expression::FunctionCall {
                name: "MONTH".into(),
                args: vec![arg(0)??.0],
            },
            FieldType::Double,
        ),

        F::EMPTY => {
            let (x, ty) = arg(0)??;

            match &ty {
                // EMPTY(X) => COALESCE(TRIM(CAST(x AS nvarchar(max))), '') = ''
                FieldType::Character(_) | FieldType::Memo => {
                    let cast = expr_ref(Expression::Cast(x.clone(), "nvarchar(max)"));
                    let trim = expr_ref(Expression::FunctionCall {
                        name: "TRIM".into(),
                        args: vec![cast],
                    });
                    let coalesce = expr_ref(Expression::FunctionCall {
                        name: "COALESCE".into(),
                        args: vec![trim, expr_ref("".into())],
                    });
                    ok(
                        Expression::BinaryOperator(
                            coalesce,
                            crate::translate::BinaryOp::Eq,
                            expr_ref("".into()),
                            crate::translate::Parenthesize::No,
                        ),
                        FieldType::Logical,
                    )
                }

                // EMPTY(X) => COALESCE(x, COALESCE_DATE) = COALESCE_DATE
                FieldType::Date | FieldType::DateTime => ok(
                    Expression::BinaryOperator(
                        x.clone(),
                        crate::translate::BinaryOp::Eq,
                        expr_ref(COALESCE_DATE.into()),
                        crate::translate::Parenthesize::No,
                    ),
                    FieldType::Logical,
                ),

                // EMPTY(X) => COALESCE(x, 0) = 0 (or false for logical)
                FieldType::Logical
                | FieldType::Integer
                | FieldType::Double
                | FieldType::Float
                | FieldType::Currency
                | FieldType::Numeric { .. } => {
                    let coalesce = expr_ref(Expression::FunctionCall {
                        name: "COALESCE".into(),
                        args: vec![
                            x.clone(),
                            expr_ref(Expression::NumberLiteral("0".to_string())),
                        ],
                    });
                    ok(
                        Expression::BinaryOperator(
                            coalesce,
                            crate::translate::BinaryOp::Eq,
                            expr_ref(Expression::NumberLiteral("0".to_string())),
                            crate::translate::Parenthesize::No,
                        ),
                        FieldType::Logical,
                    )
                }

                // EMPTY(X => COALESCE(LEN(x), 0) = 0
                FieldType::CharacterBinary(..) | FieldType::MemoBinary | FieldType::General => {
                    let len = expr_ref(Expression::FunctionCall {
                        name: "LEN".into(),
                        args: vec![x.clone()],
                    });
                    let coalesce = expr_ref(Expression::FunctionCall {
                        name: "COALESCE".into(),
                        args: vec![len, expr_ref(Expression::NumberLiteral("0".into()))],
                    });
                    ok(
                        Expression::BinaryOperator(
                            coalesce,
                            crate::translate::BinaryOp::Eq,
                            expr_ref(Expression::NumberLiteral("0".to_string())),
                            crate::translate::Parenthesize::No,
                        ),
                        FieldType::Logical,
                    )
                }
            }
        }

        F::LEFT => {
            let (x, ty) = arg(0)??;
            let n = match &*arg(1)??.0.borrow() {
                Expression::NumberLiteral(v) => v.parse::<u32>().map_err(|_| wrong_type(1)),
                _ => Err(wrong_type(1)),
            }?;
            let out_ty = match ty {
                FieldType::Character(len) => FieldType::Character(len - n),
                _ => FieldType::Memo,
            };
            ok(
                Expression::FunctionCall {
                    name: "LEFT".into(),
                    args: vec![x, arg(1)??.0],
                },
                out_ty,
            )
        }

        // STOD(x) => CONVERT(date, x, 112) -- format 112 is YYYYMMDD
        F::STOD => ok(
            Expression::FunctionCall {
                name: "CONVERT".to_string(),
                args: vec![
                    expr_ref(Expression::BareFunctionCall("date".to_string())),
                    arg(0)??.0,
                    expr_ref(Expression::NumberLiteral("112".to_string())), // YYYYMMDD format
                ],
            },
            FieldType::Date,
        ),

        // STR(num, len, dec) => STR(num, len, dec) with padding
        F::STR => {
            let len: i64 = match &*arg(1)??.0.borrow() {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(1)),
                _ => Err(wrong_type(1)),
            }?;
            let dec: i64 = match &*arg(2)??.0.borrow() {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(2)),
                _ => Err(wrong_type(2)),
            }?;

            // SQL Server STR function: STR(float_expression, length, decimal)
            ok(
                Expression::FunctionCall {
                    name: "STR".to_string(),
                    args: vec![
                        arg(0)??.0,
                        expr_ref(Expression::NumberLiteral(len.to_string())),
                        expr_ref(Expression::NumberLiteral(dec.to_string())),
                    ],
                },
                FieldType::Character(len as u32),
            )
        }

        // VAL(x) => CAST(x as float)
        F::VAL => ok(
            Expression::Cast(arg(0)??.0, "float"),
            FieldType::Numeric { len: 0, dec: 0 },
        ),

        // YEAR(x) => YEAR(x) -- SQL Server has this function
        F::YEAR => ok(
            Expression::FunctionCall {
                name: "YEAR".into(),
                args: vec![arg(0)??.0],
            },
            FieldType::Double,
        ),

        F::SUBSTR => {
            let len: u32 = match &*arg(2)??.0.borrow() {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(2)),
                _ => Err(wrong_type(2)),
            }?;
            ok(
                Expression::FunctionCall {
                    name: "SUBSTRING".into(),
                    args: all_args()?,
                },
                FieldType::Character(len),
            )
        }

        // For all other functions, delegate to Postgres implementation
        other => postgres::translate_fn_call(other, args, cx),
    }
}

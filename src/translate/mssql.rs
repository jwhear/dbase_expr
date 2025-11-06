//! Important implementation note!
//! MS SQL has booleans (the result of `x = y`) but they're not values so you can't
//!  return them or even use them in most expressions (e.g. CAST). They're basically
//!  only valid as part of a boolean operation (AND/OR/NOT), within IIF/CASE, or
//!  as a WHERE clause (or similar).
//!
//! As a result, this translator exposes two translate methods: [translate_for_select]
//!  and [translate_for_where]. Conditionals are forced to values using:
//!    `CASE WHEN (conditional) THEN 1 ELSE 0 END`
//!  while values are forced to conditionals using `value = 1`.

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

            // Unary NOT needs to coerce its argument to a conditional
            ast::Expression::UnaryOperator(ast::UnaryOp::Not, op) => {
                let (op, _) = self.translate_for_where(op)?;
                Ok((
                    expr_ref(Expression::UnaryOperator(super::UnaryOp::Not, op)),
                    FieldType::Logical,
                ))
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
                expr_ref(Expression::NumberLiteral("1".into())),
                expr_ref(Expression::NumberLiteral(len.to_string())),
            ],
        })
    }
}

impl<F> MssqlTranslator<F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
{
    pub fn translate_for_select(&self, source: &ast::Expression) -> Result {
        self.translate(source).map(coerce_to_value)
    }

    pub fn translate_for_where(&self, source: &ast::Expression) -> Result {
        self.translate(source).map(coerce_to_condition)
    }

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
                        vec![Box::new(ast::Expression::NumberLiteral("0".into()))];
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

/// Will MS SQL see the translated node as a conditional?
fn is_conditional(expr: &Expression) -> bool {
    use super::{BinaryOp::*, UnaryOp::Not};

    matches!(
        expr,
        Expression::BinaryOperator(
            _,
            Eq | Ne | Lt | Le | Gt | Ge | NotBetween | Between | StartsWith | And | Or,
            _,
            _
        ) | Expression::BinaryOperatorSequence(
            Eq | Ne | Lt | Le | Gt | Ge | NotBetween | Between | StartsWith | And | Or,
            _
        ) | Expression::UnaryOperator(Not, _),
    )
}

/// If the expression is a conditional, coerce it to a value (true = 1, false = 0).
/// Coercion is only injected if `is_conditional(expr) == true` so this is safe to call
///  preemptively wherever you need it.
fn coerce_to_value((expr, ty): (ExprRef, FieldType)) -> (ExprRef, FieldType) {
    if !is_conditional(&expr.borrow()) {
        return (expr, ty);
    }

    // Wrap with `IIF(expr, 1, 0)`
    (
        expr_ref(Expression::Iif {
            cond: expr,
            when_true: expr_ref(Expression::NumberLiteral("1".into())),
            when_false: expr_ref(Expression::NumberLiteral("0".into())),
        }),
        FieldType::Numeric { len: 1, dec: 0 },
    )
}

/// If the expression is a value (non-condition), coerce to a condition by adding `$ = 1`.
/// Coercion is only injected if `is_conditional(expr) == false` so this is safe
///  to call wherever you need it.
fn coerce_to_condition((expr, ty): (ExprRef, FieldType)) -> (ExprRef, FieldType) {
    if is_conditional(&expr.borrow()) {
        return (expr, ty);
    }

    (
        expr_ref(Expression::BinaryOperator(
            expr,
            super::BinaryOp::Eq,
            expr_ref(Expression::NumberLiteral("1".into())),
            super::Parenthesize::No,
        )),
        FieldType::Logical,
    )
}

fn dateadd_expr(interval: &str, amount: ExprRef, date: ExprRef) -> Expression {
    Expression::FunctionCall {
        name: "DATEADD".into(),
        args: vec![
            expr_ref(Expression::BareFunctionCall(interval.to_string())),
            amount,
            date,
        ],
    }
}

fn datediff_expr(interval: &str, start_date: ExprRef, end_date: ExprRef) -> Expression {
    Expression::FunctionCall {
        name: "DATEDIFF".into(),
        args: vec![
            expr_ref(Expression::BareFunctionCall(interval.to_string())),
            start_date,
            end_date,
        ],
    }
}

fn concat_expr(args: Vec<ExprRef>) -> Expression {
    Expression::FunctionCall {
        name: "CONCAT".into(),
        args,
    }
}

fn string_subtract_expr(left: ExprRef, right: ExprRef) -> Expression {
    let without_spaces = expr_ref(Expression::FunctionCall {
        name: "RTRIM".into(),
        args: vec![left.clone()],
    });
    let length_without_spaces = expr_ref(Expression::FunctionCall {
        name: "LEN".into(),
        args: vec![without_spaces.clone()],
    });
    let length_with_spaces = expr_ref(Expression::FunctionCall {
        name: "DATALENGTH".into(),
        args: vec![left],
    });
    let num_spaces = expr_ref(Expression::BinaryOperator(
        length_with_spaces,
        crate::translate::BinaryOp::Sub,
        length_without_spaces,
        crate::translate::Parenthesize::No,
    ));
    let repeated_spaces = expr_ref(Expression::FunctionCall {
        name: "REPLICATE".into(),
        args: vec![
            expr_ref(Expression::SingleQuoteStringLiteral(" ".into())),
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
        // And needs to coerce its operands to conditionals
        (ast::BinaryOp::And, _) => ok(
            Expression::BinaryOperator(
                coerce_to_condition((translated_l, ty_l)).0,
                super::BinaryOp::And,
                coerce_to_condition(cx.translate(r)?).0,
                super::Parenthesize::No,
            ),
            FieldType::Logical,
        ),
        // Or needs to coerce its operands to conditionals
        (ast::BinaryOp::Or, _) => ok(
            Expression::BinaryOperator(
                coerce_to_condition((translated_l, ty_l)).0,
                super::BinaryOp::Or,
                coerce_to_condition(cx.translate(r)?).0,
                super::Parenthesize::No,
            ),
            FieldType::Logical,
        ),

        // Eq needs to coerce its arguments to values
        (ast::BinaryOp::Eq, _) => {
            match r {
                // We can save on awkward conditional->value conversions with a
                //  few optimizations:
                // `$ = .t.` -> `$`
                // `$ = .f.` -> `NOT $`
                /*
                NOTE: these optimizations are currently disabled; MS SQL doesn't
                 actually allow a field name where a boolean is expected or
                 `NOT fieldname`
                // $ = .t. -> $
                ast::Expression::BoolLiteral(true) => Ok((translated_l, ty_l)),
                // $ = .f. -> NOT $
                ast::Expression::BoolLiteral(false) => ok(
                    Expression::UnaryOperator(super::UnaryOp::Not, translated_l),
                    FieldType::Logical,
                ),
                */
                // All others: ensure both operands are values
                _ => ok(
                    Expression::BinaryOperator(
                        coerce_to_value((translated_l, ty_l)).0,
                        super::BinaryOp::Eq,
                        coerce_to_value(cx.translate(r)?).0,
                        super::Parenthesize::No,
                    ),
                    FieldType::Logical,
                ),
            }
        }

        // Ne needs to coerce its arguments to values
        (ast::BinaryOp::Ne, _) => ok(
            Expression::BinaryOperator(
                coerce_to_value((translated_l, ty_l)).0,
                super::BinaryOp::Ne,
                coerce_to_value(cx.translate(r)?).0,
                super::Parenthesize::No,
            ),
            FieldType::Logical,
        ),

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
                name: "CHARINDEX".into(),
                args: vec![translated_l, translated_r], // needle, haystack
            });
            // Use CASE WHEN for maximum SQL Server compatibility
            ok(
                Expression::Case {
                    branches: vec![crate::translate::When {
                        cond: expr_ref(Expression::BinaryOperator(
                            charindex,
                            crate::translate::BinaryOp::Gt,
                            expr_ref(Expression::NumberLiteral("0".into())),
                            crate::translate::Parenthesize::No,
                        )),
                        then: expr_ref(Expression::NumberLiteral("1".into())),
                    }],
                    r#else: expr_ref(Expression::NumberLiteral("0".into())),
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
                name: "CHAR".into(),
                args: all_args()?,
            },
            FieldType::Character(1),
        ),

        // CTOD(x) => CONVERT(date, x, 101) -- format 101 is MM/DD/YY
        F::CTOD => ok(
            Expression::FunctionCall {
                name: "CONVERT".into(),
                args: vec![
                    expr_ref(Expression::BareFunctionCall("date".into())),
                    arg(0)??.0,
                    expr_ref(Expression::NumberLiteral("101".into())), // MM/DD/YY format
                ],
            },
            FieldType::Date,
        ),

        // DATE() => CAST(GETDATE() AS date)
        F::DATE => ok(
            Expression::Cast(
                expr_ref(Expression::BareFunctionCall("GETDATE()".into())),
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
                        args: vec![x.clone(), expr_ref(Expression::NumberLiteral("0".into()))],
                    });
                    ok(
                        Expression::BinaryOperator(
                            coalesce,
                            crate::translate::BinaryOp::Eq,
                            expr_ref(Expression::NumberLiteral("0".into())),
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
                            expr_ref(Expression::NumberLiteral("0".into())),
                            crate::translate::Parenthesize::No,
                        ),
                        FieldType::Logical,
                    )
                }
            }
        }

        // IIF can be translated via the postgres implementation after coercing
        //  the first argument to a conditional and the others to values
        F::IIF => {
            if let [cond, when_true, when_false] = args {
                // We need to coerce the ast::Expression, not the result of
                //  translation so that we can call the postgres translator
                let cond = ast::coerce_to_condition(cond.clone());
                let when_true = ast::coerce_to_value(when_true.clone());
                let when_false = ast::coerce_to_value(when_false.clone());

                postgres::translate_fn_call(&F::IIF, &[cond, when_true, when_false], cx)
            } else {
                Err(Error::IncorrectArgCount("IIF".into(), args.len()))
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
                name: "CONVERT".into(),
                args: vec![
                    expr_ref(Expression::BareFunctionCall("date".into())),
                    arg(0)??.0,
                    expr_ref(Expression::NumberLiteral("112".into())), // YYYYMMDD format
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
                    name: "STR".into(),
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

        // PADL(string, length) => RIGHT(REPLICATE(' ', length) + string, length)
        F::PADL => {
            let len: u32 = match &*arg(1)??.0.borrow() {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(1)),
                _ => Err(wrong_type(1)),
            }?;

            let replicate_spaces = expr_ref(Expression::FunctionCall {
                name: "REPLICATE".into(),
                args: vec![
                    expr_ref(Expression::SingleQuoteStringLiteral(" ".into())),
                    arg(1)??.0,
                ],
            });

            let padded_string = expr_ref(Expression::FunctionCall {
                name: "CONCAT".into(),
                args: vec![replicate_spaces, arg(0)??.0],
            });

            ok(
                Expression::FunctionCall {
                    name: "RIGHT".into(),
                    args: vec![padded_string, arg(1)??.0],
                },
                FieldType::Character(len),
            )
        }

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

#[cfg(test)]
mod tests {
    use super::*;

    fn field_lookup(
        alias: Option<&str>,
        field: &str,
    ) -> std::result::Result<(String, FieldType), String> {
        let ty = match (alias, field) {
            (_, "A" | "B" | "C") => FieldType::Integer,
            (_, "BINDATAFIELD") => FieldType::MemoBinary,
            (_, "SHIP_DATE") => FieldType::Date,
            (_, "DATE") => FieldType::Date,
            (_, "ID") => FieldType::Character(1),
            (_, "QUOTE") => FieldType::Character(10),
            (_, "L_NAME") => FieldType::Character(20),
            (_, "DESCR_2") => FieldType::Memo,
            (_, "DESCRIPTION") => FieldType::Memo,
            (_, "PO_EXT") => FieldType::Character(2),
            (_, "PO_NO") => FieldType::Character(15),
            (_, "C_TYPE") => FieldType::Numeric { len: 2, dec: 0 },
            (_, "INACTIVE") => FieldType::Logical,

            (Some(alias), _) => panic!("unknown field: {alias}.{field}"),
            (None, _) => panic!("unknown field: {field}"),
        };
        Ok((field.to_uppercase(), ty))
    }

    macro_rules! assert_tr_eq {
        ($translate:ident, $src:expr, $witness:expr) => {
            let translator = MssqlTranslator { field_lookup };
            let mssql_config = crate::to_sql::PrinterConfig {
                context: Box::new(crate::to_sql::MssqlPrinterContext),
            };
            let parser = crate::grammar::ExprParser::new();
            let ast = parser.parse($src).map_err(|e| format!("{e}"))?;

            let ast = crate::ast::simplify(*ast);
            let (sql_ast, _ty) = translator.$translate(&ast).map_err(|e| format!("{e}"))?;
            let sql = format!(
                "{}",
                crate::to_sql::Printer::new(sql_ast.clone(), mssql_config.clone())
            );
            //assert_eq!($witness, &sql, "AST: {sql_ast:?}");
            assert!(
                $witness == &sql,
                " expected: \"{}\"\n      got: \"{sql}\"\nAST: {sql_ast:?}",
                $witness
            );
        };
    }

    macro_rules! assert_select_eq {
        ($src:expr, $witness:expr) => {
            assert_tr_eq!(translate_for_select, $src, $witness)
        };
    }
    macro_rules! assert_where_eq {
        ($src:expr, $witness:expr) => {
            assert_tr_eq!(translate_for_where, $src, $witness)
        };
    }

    // Tests for bit-typed expressions in SQL Server
    // The main thing is that they should produce a valid expression that can be used as a SELECT column
    #[test]
    fn test_bool_as_bit_values() -> std::result::Result<(), String> {
        assert_select_eq!(".t.", "1");
        assert_select_eq!(".f.", "0");
        assert_select_eq!("INACTIVE", "INACTIVE");
        assert_select_eq!(
            ".not. INACTIVE",
            "(CASE WHEN (NOT INACTIVE=1) THEN 1 ELSE 0 END)"
        );
        assert_select_eq!("INACTIVE=.f.", "(CASE WHEN INACTIVE=0 THEN 1 ELSE 0 END)");
        assert_select_eq!(
            "INACTIVE = .f. = .f.",
            "(CASE WHEN (CASE WHEN INACTIVE=0 THEN 1 ELSE 0 END)=0 THEN 1 ELSE 0 END)"
        );
        assert_select_eq!(
            "INACTIVE .or. A < 0",
            "(CASE WHEN INACTIVE=1 OR (A<0) THEN 1 ELSE 0 END)"
        );
        assert_select_eq!(
            "DATE = SHIP_DATE",
            "(CASE WHEN COALESCE(DATE, '0001-01-01')=COALESCE(SHIP_DATE, '0001-01-01') THEN 1 ELSE 0 END)"
        );
        assert_select_eq!(
            "empty(C_TYPE)",
            "(CASE WHEN COALESCE(C_TYPE,0)=0 THEN 1 ELSE 0 END)"
        );
        assert_select_eq!(
            "empty(C_TYPE) = .f.",
            "(CASE WHEN (CASE WHEN COALESCE(C_TYPE,0)=0 THEN 1 ELSE 0 END)=0 THEN 1 ELSE 0 END)"
        );
        assert_select_eq!(
            "iif(INACTIVE, 'Inactive', 'Active')",
            "(CASE WHEN INACTIVE=1 THEN 'Inactive' ELSE 'Active' END) "
        );
        assert_select_eq!(
            "iif(INACTIVE = .t., 'Inactive', 'Active')",
            "(CASE WHEN INACTIVE=1 THEN 'Inactive' ELSE 'Active' END) "
        );
        assert_select_eq!(
            "iif(.not. INACTIVE, 'Active', 'Inactive')",
            "(CASE WHEN (NOT INACTIVE=1) THEN 'Active' ELSE 'Inactive' END) "
        );
        assert_select_eq!(
            "iif(DATE < stod('19690720'), DATE > stod('19620220'), L_NAME = 'Armstrong' .or. L_NAME = 'Aldrin')",
            "(CASE WHEN (COALESCE(DATE, '0001-01-01')<CONVERT( date ,'19690720',112)) THEN (CASE WHEN (COALESCE(DATE, '0001-01-01')>CONVERT( date ,'19620220',112)) THEN 1 ELSE 0 END)  WHEN LEFT(COALESCE(L_NAME, '') + REPLICATE(' ', 20), 20)='Armstrong' OR LEFT(COALESCE(L_NAME, '') + REPLICATE(' ', 20), 20)='Aldrin' THEN 1 ELSE 0 END) "
        );
        Ok(())
    }

    // Tests for boolean-typed conditions in SQL Server
    // The main thing is that they should produce a valid condition that can be used as a WHERE clause
    #[test]
    fn test_bool_as_conditionals() -> std::result::Result<(), String> {
        assert_where_eq!(".t.", "1=1");
        assert_where_eq!(".f.", "0=1");
        assert_where_eq!("INACTIVE", "INACTIVE=1");
        assert_where_eq!(".not. INACTIVE", "(NOT INACTIVE=1)");
        assert_where_eq!("INACTIVE=.f.", "INACTIVE=0");
        assert_where_eq!(
            "INACTIVE = .f. = .f.",
            "(CASE WHEN INACTIVE=0 THEN 1 ELSE 0 END)=0"
        );
        assert_where_eq!("INACTIVE .or. A < 0", "INACTIVE=1 OR (A<0)");
        assert_where_eq!(
            "DATE = SHIP_DATE",
            "COALESCE(DATE, '0001-01-01')=COALESCE(SHIP_DATE, '0001-01-01')"
        );
        assert_where_eq!("empty(C_TYPE)", "COALESCE(C_TYPE,0)=0");
        assert_where_eq!(
            "iif(DATE < stod('19690720'), DATE > stod('19620220'), L_NAME = 'Armstrong' .or. L_NAME = 'Aldrin')",
            "(CASE WHEN (COALESCE(DATE, '0001-01-01')<CONVERT( date ,'19690720',112)) THEN (CASE WHEN (COALESCE(DATE, '0001-01-01')>CONVERT( date ,'19620220',112)) THEN 1 ELSE 0 END)  WHEN LEFT(COALESCE(L_NAME, '') + REPLICATE(' ', 20), 20)='Armstrong' OR LEFT(COALESCE(L_NAME, '') + REPLICATE(' ', 20), 20)='Aldrin' THEN 1 ELSE 0 END) =1"
        );
        Ok(())
    }
}

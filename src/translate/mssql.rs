use crate::{
    codebase_functions::CodebaseFunction as F,
    parser::{self, ExpressionId, ParseTree},
    translate::{
        COALESCE_DATE, Error, ExprRef, Expression, FieldType, Result, TranslationContext, expr_ref,
        ok,
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

    fn custom_function(&self, _func: &str) -> Option<parser::Expression> {
        None //not used here
    }

    fn translate(&self, source: &parser::Expression, tree: &ParseTree) -> Result {
        self.translate_impl(source, tree)
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
        translate_binary_op(self, l, op, r, tree)
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
    fn translate_impl(&self, source: &parser::Expression, tree: &ParseTree) -> Result {
        match source {
            parser::Expression::Sequence(args, op) => {
                let args_ids = tree.get_args(args);
                if args_ids.is_empty() {
                    return Err(Error::UnsupportedFunction("empty sequence".into()));
                }

                let first_expr_id = args_ids[0];
                let first_expr = tree.get_expr_unchecked(first_expr_id);
                let (first_result, first_ty) = self.translate_impl(first_expr, tree)?;

                match first_ty {
                    FieldType::Date => self.translate_date_sequence(
                        first_result,
                        first_ty,
                        &args_ids[1..],
                        op,
                        tree,
                    ),
                    FieldType::Character(_) | FieldType::Memo | FieldType::MemoBinary => self
                        .translate_string_sequence(
                            first_result,
                            first_ty,
                            &args_ids[1..],
                            op,
                            tree,
                        ),
                    _ => self.translate_default(source, tree),
                }
            }
            _ => self.translate_default(source, tree),
        }
    }

    fn translate_default(&self, source: &parser::Expression, tree: &ParseTree) -> Result {
        match source {
            parser::Expression::BoolLiteral(v) => {
                ok(Expression::BoolLiteral(*v), FieldType::Logical)
            }
            parser::Expression::NumberLiteral(v) => {
                if let Ok(v_str) = std::str::from_utf8(v) {
                    let dec = v_str
                        .chars()
                        .position(|c| c == '.')
                        .map(|i| v_str.len() - i)
                        .unwrap_or(0) as u32;
                    ok(
                        Expression::NumberLiteral(v_str.to_string()),
                        FieldType::Numeric {
                            len: v_str.len() as u32,
                            dec,
                        },
                    )
                } else {
                    Err(Error::UnsupportedFunction("invalid number literal".into()))
                }
            }
            parser::Expression::StringLiteral(v) => {
                if let Ok(v_str) = std::str::from_utf8(v) {
                    let v_str = escape_single_quotes(v_str);
                    let len = v_str.len();
                    ok(
                        Expression::SingleQuoteStringLiteral(v_str),
                        FieldType::Character(len as u32),
                    )
                } else {
                    Err(Error::UnsupportedFunction("invalid string literal".into()))
                }
            }
            parser::Expression::Field { alias, name } => {
                let alias_str = alias.and_then(|a| std::str::from_utf8(a).ok());
                let name_str = std::str::from_utf8(name)
                    .map_err(|_| Error::UnsupportedFunction("invalid field name".into()))?;
                let (name, field_type) = self
                    .lookup_field(alias_str, name_str)
                    .map_err(|e| Error::Other(e))?;
                ok(Expression::Field { name, field_type }, field_type)
            }
            parser::Expression::FunctionCall { name, args } => {
                self.translate_fn_call(name, tree.get_args(args), tree)
            }
            parser::Expression::BinaryOperator(l, op, r) => self.translate_binary_op(
                tree.get_expr_unchecked(*l),
                op,
                tree.get_expr_unchecked(*r),
                tree,
            ),
            parser::Expression::UnaryOperator(op, expr) => {
                let (translated_expr, ty) =
                    self.translate_impl(tree.get_expr_unchecked(*expr), tree)?;
                match op {
                    parser::UnaryOp::Not => ok(
                        Expression::UnaryOperator(crate::translate::UnaryOp::Not, translated_expr),
                        FieldType::Logical,
                    ),
                    parser::UnaryOp::Neg => ok(
                        Expression::UnaryOperator(crate::translate::UnaryOp::Neg, translated_expr),
                        ty,
                    ),
                }
            }
            parser::Expression::Sequence(_, _) => {
                // Should be handled above
                Err(Error::UnsupportedFunction("unexpected sequence".into()))
            }
        }
    }

    fn translate_date_sequence(
        &self,
        first_expr: ExprRef,
        first_ty: FieldType,
        remaining_operands: &[ExpressionId],
        concat_op: &parser::BinaryOp,
        tree: &ParseTree,
    ) -> Result {
        let mut result = (first_expr, first_ty);
        for operand_id in remaining_operands {
            let operand = tree.get_expr_unchecked(*operand_id);
            let (right_expr, right_ty) = self.translate_impl(operand, tree)?;
            match (concat_op, &right_ty) {
                (parser::BinaryOp::Add, ty) if is_numeric_type(ty) => {
                    result = (
                        expr_ref(dateadd_expr("day", right_expr, result.0)),
                        FieldType::Date,
                    );
                }
                (parser::BinaryOp::Sub, ty) if is_numeric_type(ty) => {
                    let negated_amount = expr_ref(Expression::UnaryOperator(
                        crate::translate::UnaryOp::Neg,
                        right_expr,
                    ));
                    result = (
                        expr_ref(dateadd_expr("day", negated_amount, result.0)),
                        FieldType::Date,
                    );
                }
                (parser::BinaryOp::Sub, FieldType::Date) => {
                    result = (
                        expr_ref(datediff_expr("day", right_expr, result.0)),
                        FieldType::Numeric { len: 99, dec: 0 },
                    );
                }
                _ => {
                    // Not a supported date operation, reconstruct and fall back to default
                    let first_id = remaining_operands.first().copied().unwrap();
                    let mut all_ids = vec![first_id];
                    all_ids.extend_from_slice(remaining_operands);
                    let arg_list = tree.push_args(all_ids.into_iter());
                    return self.translate_default(
                        &parser::Expression::Sequence(arg_list, *concat_op),
                        tree,
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
        remaining_operands: &[ExpressionId],
        concat_op: &parser::BinaryOp,
        tree: &ParseTree,
    ) -> Result {
        let mut all_exprs = vec![first_expr];
        let mut result_type = first_ty;

        for operand_id in remaining_operands {
            let operand = tree.get_expr_unchecked(*operand_id);
            let (right_expr, _right_ty) = self.translate_impl(operand, tree)?;
            match concat_op {
                parser::BinaryOp::Add => {
                    all_exprs.push(right_expr);
                }
                parser::BinaryOp::Sub => {
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
                _ => {
                    // Other operations not supported for string sequences
                    let first_id = remaining_operands.first().copied().unwrap();
                    let mut all_ids = vec![first_id];
                    all_ids.extend_from_slice(remaining_operands);
                    let arg_list = tree.push_args(all_ids.into_iter());
                    return self.translate_default(
                        &parser::Expression::Sequence(arg_list, *concat_op),
                        tree,
                    );
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

fn escape_single_quotes(s: &str) -> String {
    s.replace('\'', "''")
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
    l: &parser::Expression,
    op: &parser::BinaryOp,
    r: &parser::Expression,
    tree: &ParseTree,
) -> Result {
    let (translated_l, ty_l) = cx.translate(l, tree)?;

    match (op, &ty_l) {
        // Date arithmetic
        (parser::BinaryOp::Add, FieldType::Date) => {
            let (translated_r, ty_r) = cx.translate(r, tree)?;
            if is_numeric_type(&ty_r) {
                ok(
                    dateadd_expr("day", translated_r, translated_l),
                    FieldType::Date,
                )
            } else {
                // Fall back to default translation for unsupported operations
                cx.translate_binary_op(l, op, r, tree)
            }
        }
        (parser::BinaryOp::Sub, FieldType::Date) => {
            let (translated_r, ty_r) = cx.translate(r, tree)?;
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
                cx.translate_binary_op(l, op, r, tree)
            }
        }

        // String operations
        (parser::BinaryOp::Add, ty_l) if is_string_type(ty_l) => {
            let (translated_r, _) = cx.translate(r, tree)?;
            ok(
                concat_expr(vec![translated_l, translated_r]),
                FieldType::Memo,
            )
        }
        (parser::BinaryOp::Sub, ty_l) if is_string_type(ty_l) => {
            let (translated_r, _) = cx.translate(r, tree)?;
            ok(
                string_subtract_expr(translated_l, translated_r),
                FieldType::Memo,
            )
        }

        // For all other operations, use a simple binary operator
        _ => {
            let (translated_r, _) = cx.translate(r, tree)?;
            let translate_op = match op {
                parser::BinaryOp::Add => crate::translate::BinaryOp::Add,
                parser::BinaryOp::Sub => crate::translate::BinaryOp::Sub,
                parser::BinaryOp::Mul => crate::translate::BinaryOp::Mul,
                parser::BinaryOp::Div => crate::translate::BinaryOp::Div,
                parser::BinaryOp::Eq => crate::translate::BinaryOp::Eq,
                parser::BinaryOp::Ne => crate::translate::BinaryOp::Ne,
                parser::BinaryOp::Lt => crate::translate::BinaryOp::Lt,
                parser::BinaryOp::Le => crate::translate::BinaryOp::Le,
                parser::BinaryOp::Gt => crate::translate::BinaryOp::Gt,
                parser::BinaryOp::Ge => crate::translate::BinaryOp::Ge,
                parser::BinaryOp::And => crate::translate::BinaryOp::And,
                parser::BinaryOp::Or => crate::translate::BinaryOp::Or,
                _ => crate::translate::BinaryOp::Concat, // fallback for unknown ops
            };
            ok(
                Expression::BinaryOperator(
                    translated_l,
                    translate_op,
                    translated_r,
                    crate::translate::Parenthesize::No,
                ),
                ty_l,
            )
        }
    }
}

pub fn translate_fn_call(
    name: &F,
    args: &[parser::ExpressionId],
    tree: &ParseTree,
    cx: &impl TranslationContext,
) -> std::result::Result<(ExprRef, FieldType), Error> {
    let translate_arg =
        |expr: &parser::ExpressionId| cx.translate(tree.get_expr_unchecked(*expr), tree);

    // These are only the ones that are different from Postgres, everything else falls through
    match name {
        // In SQL Server, CHR is called CHAR
        F::CHR => {
            if args.is_empty() {
                return Err(Error::IncorrectArgCount(format!("{name:?}"), 0));
            }
            let (arg_expr, _) = translate_arg(&args[0])?;
            ok(
                Expression::FunctionCall {
                    name: "CHAR".into(),
                    args: vec![arg_expr],
                },
                FieldType::Character(1),
            )
        }

        // CTOD(x) => CONVERT(date, x, 101) -- format 101 is MM/DD/YY
        F::CTOD => {
            if args.is_empty() {
                return Err(Error::IncorrectArgCount(format!("{name:?}"), 0));
            }
            let (arg_expr, _) = translate_arg(&args[0])?;
            ok(
                Expression::FunctionCall {
                    name: "CONVERT".into(),
                    args: vec![
                        expr_ref(Expression::BareFunctionCall("date".into())),
                        arg_expr,
                        expr_ref(Expression::NumberLiteral("101".into())), // MM/DD/YY format
                    ],
                },
                FieldType::Date,
            )
        }

        // DATE() => CAST(GETDATE() AS date)
        F::DATE => ok(
            Expression::Cast(
                expr_ref(Expression::BareFunctionCall("GETDATE()".into())),
                "date",
            ),
            FieldType::Date,
        ),

        // DAY(x) => DAY(x) -- SQL Server has this function
        F::DAY => {
            if args.is_empty() {
                return Err(Error::IncorrectArgCount(format!("{name:?}"), 0));
            }
            let (arg_expr, _) = translate_arg(&args[0])?;
            ok(
                Expression::FunctionCall {
                    name: "DAY".into(),
                    args: vec![arg_expr],
                },
                FieldType::Double,
            )
        }

        // DTOC(x) => FORMAT(x, 'MM/dd/yy') or FORMAT(x, 'yyyyMMdd')
        F::DTOC => {
            if args.is_empty() {
                return Err(Error::IncorrectArgCount(format!("{name:?}"), 0));
            }
            let (arg_expr, _) = translate_arg(&args[0])?;
            if args.len() == 2 {
                // Equivalent to DTOS
                ok(
                    Expression::FunctionCall {
                        name: "FORMAT".into(),
                        args: vec![arg_expr, expr_ref("yyyyMMdd".into())],
                    },
                    FieldType::Character(8),
                )
            } else {
                ok(
                    Expression::FunctionCall {
                        name: "FORMAT".into(),
                        args: vec![arg_expr, expr_ref("MM/dd/yy".into())],
                    },
                    FieldType::Character(8),
                )
            }
        }

        F::DTOS => {
            if args.is_empty() {
                return Err(Error::IncorrectArgCount(format!("{name:?}"), 0));
            }
            let (arg_expr, _) = translate_arg(&args[0])?;
            ok(
                Expression::FunctionCall {
                    name: "FORMAT".into(),
                    args: vec![arg_expr, expr_ref("yyyyMMdd".into())],
                },
                FieldType::Character(8),
            )
        }

        // MONTH(x) => MONTH(x)
        F::MONTH => {
            if args.is_empty() {
                return Err(Error::IncorrectArgCount(format!("{name:?}"), 0));
            }
            let (arg_expr, _) = translate_arg(&args[0])?;
            ok(
                Expression::FunctionCall {
                    name: "MONTH".into(),
                    args: vec![arg_expr],
                },
                FieldType::Double,
            )
        }

        F::EMPTY => {
            if args.is_empty() {
                return Err(Error::IncorrectArgCount(format!("{name:?}"), 0));
            }
            let (x, ty) = translate_arg(&args[0])?;

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

        F::LEFT => {
            if args.len() < 2 {
                return Err(Error::IncorrectArgCount(format!("{name:?}"), args.len()));
            }
            let (x, ty) = translate_arg(&args[0])?;
            let (n_expr, _) = translate_arg(&args[1])?;
            let n = match &*n_expr.borrow() {
                Expression::NumberLiteral(v) => v
                    .parse::<u32>()
                    .map_err(|_| Error::IncorrectArgCount(format!("{name:?}"), 1)),
                _ => Err(Error::IncorrectArgCount(format!("{name:?}"), 1)),
            }?;
            let out_ty = match ty {
                FieldType::Character(len) => FieldType::Character(len.saturating_sub(n)),
                _ => FieldType::Memo,
            };
            ok(
                Expression::FunctionCall {
                    name: "LEFT".into(),
                    args: vec![x, n_expr],
                },
                out_ty,
            )
        }

        // STOD(x) => CONVERT(date, x, 112) -- format 112 is YYYYMMDD
        F::STOD => {
            if args.is_empty() {
                return Err(Error::IncorrectArgCount(format!("{name:?}"), 0));
            }
            let (arg_expr, _) = translate_arg(&args[0])?;
            ok(
                Expression::FunctionCall {
                    name: "CONVERT".into(),
                    args: vec![
                        expr_ref(Expression::BareFunctionCall("date".into())),
                        arg_expr,
                        expr_ref(Expression::NumberLiteral("112".into())), // YYYYMMDD format
                    ],
                },
                FieldType::Date,
            )
        }

        // STR(num, len, dec) => STR(num, len, dec) with padding
        F::STR => {
            if args.len() < 3 {
                return Err(Error::IncorrectArgCount(format!("{name:?}"), args.len()));
            }
            let (num_expr, _) = translate_arg(&args[0])?;
            let (len_expr, _) = translate_arg(&args[1])?;
            let (dec_expr, _) = translate_arg(&args[2])?;

            let len: i64 = match &*len_expr.borrow() {
                Expression::NumberLiteral(v) => v
                    .parse()
                    .map_err(|_| Error::IncorrectArgCount(format!("{name:?}"), 1)),
                _ => Err(Error::IncorrectArgCount(format!("{name:?}"), 1)),
            }?;
            let dec: i64 = match &*dec_expr.borrow() {
                Expression::NumberLiteral(v) => v
                    .parse()
                    .map_err(|_| Error::IncorrectArgCount(format!("{name:?}"), 2)),
                _ => Err(Error::IncorrectArgCount(format!("{name:?}"), 2)),
            }?;

            // SQL Server STR function: STR(float_expression, length, decimal)
            ok(
                Expression::FunctionCall {
                    name: "STR".into(),
                    args: vec![num_expr, len_expr, dec_expr],
                },
                FieldType::Character(len as u32),
            )
        }

        // VAL(x) => CAST(x as float)
        F::VAL => {
            if args.is_empty() {
                return Err(Error::IncorrectArgCount(format!("{name:?}"), 0));
            }
            let (arg_expr, _) = translate_arg(&args[0])?;
            ok(
                Expression::Cast(arg_expr, "float"),
                FieldType::Numeric { len: 0, dec: 0 },
            )
        }

        // YEAR(x) => YEAR(x) -- SQL Server has this function
        F::YEAR => {
            if args.is_empty() {
                return Err(Error::IncorrectArgCount(format!("{name:?}"), 0));
            }
            let (arg_expr, _) = translate_arg(&args[0])?;
            ok(
                Expression::FunctionCall {
                    name: "YEAR".into(),
                    args: vec![arg_expr],
                },
                FieldType::Double,
            )
        }

        // PADL(string, length) => RIGHT(REPLICATE(' ', length) + string, length)
        F::PADL => {
            if args.len() < 2 {
                return Err(Error::IncorrectArgCount(format!("{name:?}"), args.len()));
            }
            let (str_expr, _) = translate_arg(&args[0])?;
            let (len_expr, _) = translate_arg(&args[1])?;

            let len: u32 = match &*len_expr.borrow() {
                Expression::NumberLiteral(v) => v
                    .parse()
                    .map_err(|_| Error::IncorrectArgCount(format!("{name:?}"), 1)),
                _ => Err(Error::IncorrectArgCount(format!("{name:?}"), 1)),
            }?;

            let replicate_spaces = expr_ref(Expression::FunctionCall {
                name: "REPLICATE".into(),
                args: vec![
                    expr_ref(Expression::SingleQuoteStringLiteral(" ".into())),
                    len_expr.clone(),
                ],
            });

            let padded_string = expr_ref(Expression::FunctionCall {
                name: "CONCAT".into(),
                args: vec![replicate_spaces, str_expr],
            });

            ok(
                Expression::FunctionCall {
                    name: "RIGHT".into(),
                    args: vec![padded_string, len_expr],
                },
                FieldType::Character(len),
            )
        }

        F::SUBSTR => {
            if args.len() < 3 {
                return Err(Error::IncorrectArgCount(format!("{name:?}"), args.len()));
            }
            let mut translated_args = Vec::new();
            for arg in args.iter().take(3) {
                let (arg_expr, _) = translate_arg(arg)?;
                translated_args.push(arg_expr);
            }
            let len: u32 = match &*translated_args[2].borrow() {
                Expression::NumberLiteral(v) => v
                    .parse()
                    .map_err(|_| Error::IncorrectArgCount(format!("{name:?}"), 2)),
                _ => Err(Error::IncorrectArgCount(format!("{name:?}"), 2)),
            }?;
            ok(
                Expression::FunctionCall {
                    name: "SUBSTRING".into(),
                    args: translated_args,
                },
                FieldType::Character(len),
            )
        }

        // For all other functions, use default translation
        _ => {
            // Use default behavior - translate all args and create function call
            let mut translated_args = Vec::new();
            for arg in args.iter() {
                let (arg_expr, _) = translate_arg(arg)?;
                translated_args.push(arg_expr);
            }

            // Determine return type based on function
            let return_type = match name {
                F::IIF => FieldType::Memo, // Default for conditional expressions
                F::UPPER | F::LTRIM | F::RTRIM | F::TRIM => FieldType::Memo,
                _ => FieldType::Memo, // Default fallback
            };

            ok(
                Expression::FunctionCall {
                    name: format!("{:?}", name),
                    args: translated_args,
                },
                return_type,
            )
        }
    }
}

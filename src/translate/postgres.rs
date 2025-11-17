use super::{
    BinaryOp, Error, Expression, FieldType, Result, TranslationContext, UnaryOp,
    escape_single_quotes, ok,
};
use crate::{
    ast::{self, Expression as E},
    codebase_functions::CodebaseFunction as F,
    translate::{COALESCE_DATE, ExprRef, Parenthesize, expr_ref},
};

/// This type provides default function translation for Postgres. You can
///  "inherit" while allowing overriding by implementing the TranslationContext
///  trait and dispatching to `translate_fn_call` any function calls you're not
///  interested in overriding.
pub struct Translator<F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
{
    pub field_lookup: F,
    pub custom_function: fn(&str) -> Option<ast::Expression>,
}

impl<F> TranslationContext for Translator<F>
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
        (self.custom_function)(func)
    }

    fn translate(&self, source: &ast::Expression) -> Result {
        translate(source, self)
    }

    fn translate_binary_op(
        &self,
        l: &ast::Expression,
        op: &ast::BinaryOp,
        r: &ast::Expression,
    ) -> Result {
        translate_binary_op(self, l, op, r)
    }

    fn translate_fn_call(
        &self,
        name: &crate::codebase_functions::CodebaseFunction,
        args: &[Box<ast::Expression>],
    ) -> std::result::Result<(ExprRef, FieldType), Error> {
        translate_fn_call(name, args, self)
    }
}

/// Translates dBase expression to a SQL expression.
pub fn translate<C: TranslationContext>(source: &E, cx: &C) -> Result {
    // helper for creating binary operators
    match source {
        E::BoolLiteral(v) => ok(Expression::BoolLiteral(*v), FieldType::Logical),
        E::NumberLiteral(v) => {
            let dec = v
                .chars()
                .position(|c| c == '.')
                .map(|i| v.len() - i)
                .unwrap_or(0) as u32;
            ok(
                Expression::NumberLiteral(v.clone()),
                FieldType::Numeric {
                    len: v.len() as u32,
                    dec,
                },
            )
        }
        E::StringLiteral(v) => {
            let v = escape_single_quotes(v);
            let len = v.len();
            ok(
                Expression::SingleQuoteStringLiteral(v),
                FieldType::Character(len as u32),
            )
        }
        E::Field { alias, name } => {
            let (name, field_type) = cx
                .lookup_field(alias.as_deref(), name)
                .map_err(Error::Other)?;
            ok(Expression::Field { name, field_type }, field_type)
        }
        E::UnaryOperator(op, r) => match op {
            ast::UnaryOp::Not => ok(
                Expression::UnaryOperator(UnaryOp::Not, translate(r, cx)?.0),
                FieldType::Logical,
            ),
            ast::UnaryOp::Neg => {
                let r = translate(r, cx)?;
                ok(Expression::UnaryOperator(UnaryOp::Neg, r.0), r.1)
            }
        },
        E::BinaryOperator(l, op, r) => {
            // Add, Sub are ambiguous: could be numeric, concat, or days (for dates)
            // We translate the first operand and use its type to determine how
            //  to translate.
            cx.translate_binary_op(l, op, r)
        }
        E::FunctionCall { name, args } => cx.translate_fn_call(name, args),
        E::Sequence(operands, op) => {
            // We'll inspect the type of the first operand and use that to
            //  either emit a '+' or a '||'
            let (first_expr, ty) = cx.translate(&operands[0])?;
            let mut exprs = Vec::with_capacity(operands.len());
            exprs.push(first_expr);
            for operand in &operands[1..] {
                let expr = cx.translate(operand)?.0;
                exprs.push(expr);
            }

            let operator = match (op, ty) {
                (
                    &ast::ConcatOp::Add,
                    FieldType::Character(_) | FieldType::Memo | FieldType::MemoBinary,
                ) => BinaryOp::Concat,
                (&ast::ConcatOp::Add, _) => BinaryOp::Add,
                (&ast::ConcatOp::Sub, _) => BinaryOp::Sub,
            };
            ok(Expression::BinaryOperatorSequence(operator, exprs), ty)
        }
    }
}

// This function does the kind of gross work of converting dBase function calls
//  to the SQL equivalent.  Some are super straightforward: `CHR(97)` -> `CHR(97)`
//  but others have no exact equivalent and have to resolve to a nested bundle.
pub fn translate_fn_call(
    name: &F,
    args: &[Box<ast::Expression>],
    cx: &impl TranslationContext,
) -> std::result::Result<(ExprRef, FieldType), Error> {
    let arg = |index: usize| get_arg(index, args, cx, name);
    let all_args = || get_all_args(args, cx);
    let wrong_type = |index| wrong_type(index, name, args);
    let date = |format: &str| {
        //this translates blank strings into the coalesce date so that it can be properly compared
        let trim = Expression::FunctionCall {
            name: "TRIM".into(),
            args: vec![arg(0)??.0],
        };
        let null_if = Expression::FunctionCall {
            name: "NULLIF".into(),
            args: vec![expr_ref(trim), expr_ref("".into())],
        };
        let to_date = Expression::FunctionCall {
            name: "TO_DATE".into(),
            args: vec![expr_ref(null_if), expr_ref(format.into())],
        };
        let coalesce = Expression::FunctionCall {
            name: "COALESCE".into(),
            args: vec![expr_ref(to_date), expr_ref(COALESCE_DATE.into())],
        };
        ok(coalesce, FieldType::Date)
    };

    match name {
        // ALLTRIM(x) => TRIM(x)
        F::ALLTRIM => ok(
            Expression::FunctionCall {
                name: "TRIM".to_string(),
                args: vec![arg(0)??.0],
            },
            FieldType::Memo,
        ),
        // CHR(x) => CHR(x)
        F::CHR => ok(
            Expression::FunctionCall {
                name: "CHR".to_string(),
                args: all_args()?,
            },
            FieldType::Character(1),
        ),
        // CTOD(x) => COALESCE(TO_DATE(NULLIF(TRIM(x),''),'MM/DD/YY'),'0001-01-01')
        F::CTOD => date("MM/DD/YY"), //TODO KOB-104
        // DATE() => CURRENT_DATE
        F::DATE => ok(
            Expression::BareFunctionCall("CURRENT_DATE".to_string()),
            FieldType::Date,
        ),
        // DAY(x) => DATE_PART('DAY' FROM x)
        F::DAY => ok(
            Expression::FunctionCall {
                name: "DATE_PART".into(),
                args: vec![expr_ref("DAY".into()), arg(0)??.0],
            },
            FieldType::Double,
        ),
        // DELETED() => __deleted
        F::DELETED => {
            let (name, field_type) = cx.lookup_field(None, "__deleted").map_err(Error::Other)?;
            ok(Expression::Field { name, field_type }, field_type)
        }

        // DTOC(x) => TO_CHAR(x, 'MM/DD/YY')
        F::DTOC => {
            if args.len() == 2 {
                // Equivalent to DTOS
                ok(
                    Expression::FunctionCall {
                        name: "TO_CHAR".into(),
                        args: vec![arg(0)??.0, expr_ref("YYYYMMDD".into())],
                    },
                    FieldType::Character(8),
                )
            } else {
                ok(
                    Expression::FunctionCall {
                        name: "TO_CHAR".into(),
                        args: vec![
                            arg(0)??.0,
                            //TODO KOB-104
                            expr_ref("MM/DD/YY".into()),
                        ],
                    },
                    FieldType::Character(8),
                )
            }
        }
        F::DTOS => ok(
            Expression::FunctionCall {
                name: "TO_CHAR".into(),
                args: vec![arg(0)??.0, expr_ref("YYYYMMDD".into())],
            },
            FieldType::Character(8),
        ),

        //the result of EMPTY depends on the type
        F::EMPTY => {
            let (arg, ty) = arg(0)??;
            let expression = match ty {
                FieldType::Logical => {
                    // COALESCE(x, false) = false
                    let coalesce = expr_ref(Expression::FunctionCall {
                        name: "COALESCE".into(),
                        args: vec![arg.clone(), expr_ref(Expression::BoolLiteral(false))],
                    });
                    Expression::BinaryOperator(
                        coalesce,
                        BinaryOp::Eq,
                        expr_ref(Expression::BoolLiteral(false)),
                        Parenthesize::No,
                    )
                }
                FieldType::Integer
                | FieldType::Currency
                | FieldType::Double
                | FieldType::Float
                | FieldType::Numeric { .. } => {
                    // COALESCE(x, 0) = 0
                    let coalesce = expr_ref(Expression::FunctionCall {
                        name: "COALESCE".into(),
                        args: vec![arg.clone(), expr_ref(Expression::NumberLiteral("0".into()))],
                    });
                    Expression::BinaryOperator(
                        coalesce,
                        BinaryOp::Eq,
                        expr_ref(Expression::NumberLiteral("0".into())),
                        Parenthesize::No,
                    )
                }
                FieldType::Character(_)
                | FieldType::Memo
                | FieldType::Date
                | FieldType::DateTime => {
                    // COALESCE(TRIM(CAST(x AS TEXT)), '') = ''
                    let trim = expr_ref(Expression::Cast(arg, "text"));
                    let trim = expr_ref(Expression::FunctionCall {
                        name: "TRIM".into(),
                        args: vec![trim],
                    });
                    let coalesce = expr_ref(Expression::FunctionCall {
                        name: "COALESCE".into(),
                        args: vec![trim, expr_ref("".into())],
                    });
                    Expression::BinaryOperator(
                        coalesce,
                        BinaryOp::Eq,
                        expr_ref("".into()),
                        Parenthesize::No,
                    )
                }
                FieldType::MemoBinary | FieldType::CharacterBinary(_) | FieldType::General => {
                    // COALESCE(LENGTH(x), 0) = 0
                    let length_call = expr_ref(Expression::FunctionCall {
                        name: "length".into(),
                        args: vec![arg],
                    });
                    let coalesce_call = expr_ref(Expression::FunctionCall {
                        name: "COALESCE".into(),
                        args: vec![length_call, expr_ref(Expression::NumberLiteral("0".into()))],
                    });
                    Expression::BinaryOperator(
                        coalesce_call,
                        BinaryOp::Eq,
                        expr_ref(Expression::NumberLiteral("0".into())),
                        Parenthesize::No,
                    )
                }
            };
            ok(expression, FieldType::Logical)
        }
        // Translate nested IIFs to a flat CASE WHEN. This optimization is
        //  important because some databases (looking at you, SQL Server) have
        //  a limit how deeply nested control flow like CASE and IIF can go.
        //
        // Note that this:
        //   IIF(cond_a, v1, IIF(cond_b, v2, v3))
        // is always structurally equivalent to:
        //   CASE WHEN cond_a THEN v1 WHEN cond_b THEN v2 ELSE v3 END
        F::IIF => {
            // The result type will be the type of the when_true expression
            let (_, ty) = arg(1)??;

            let mut branches = Vec::new();
            // We have to take ownership of args for the loop to work
            //OPT: come back and try to rework this
            let mut inner_args = Vec::from(args);

            // Add this IIF as a When branch. If when_false is an IIF, traverse
            //  into it and repeat. We'll eventually encounter a when_false that
            //  is not an IIF: that will become our ELSE value.
            let r#else = loop {
                match inner_args.as_slice() {
                    [cond, when_true, when_false] => {
                        // Convert this IIF to a WHEN
                        branches.push(super::When {
                            cond: cx.translate(cond)?.0,
                            then: cx.translate(when_true)?.0,
                        });

                        //OPT there's probably a way to work around this clone
                        let expr: ast::Expression = (**when_false).clone();
                        if let ast::Expression::FunctionCall { name: F::IIF, args } = expr {
                            inner_args = args.clone();
                        } else {
                            break when_false;
                        }
                    }
                    _ => panic!("IIF should always have three arguments"),
                }
            };
            let (r#else, _) = cx.translate(r#else)?;

            ok(Expression::Case { branches, r#else }, ty)
        }
        // LEFT(x, n) => SUBSTR(x, 1, n)
        F::LEFT => ok(
            Expression::FunctionCall {
                name: "SUBSTR".to_string(),
                args: vec![arg(0)??.0, expr_ref(1.into()), arg(1)??.0],
            },
            FieldType::Memo,
        ),
        F::LTRIM => ok(
            Expression::FunctionCall {
                name: "LTRIM".into(),
                args: vec![arg(0)??.0],
            },
            FieldType::Memo,
        ),

        // MONTH(x) => DATE_PART('MONTH', x)
        F::MONTH => ok(
            Expression::FunctionCall {
                name: "DATE_PART".into(),
                args: vec![expr_ref("MONTH".into()), arg(0)??.0],
            },
            FieldType::Double,
        ),

        F::PADL => ok(
            Expression::FunctionCall {
                name: "LPAD".into(),
                args: vec![arg(0)??.0, arg(1)??.0, expr_ref(" ".into())],
            },
            FieldType::Memo,
        ),

        // RECNO() => RECNO5
        F::RECNO => {
            let (name, field_type) = cx.lookup_field(None, "RECNO5").map_err(Error::Other)?;
            ok(Expression::Field { name, field_type }, field_type)
        }

        // RIGHT(x, n) => RIGHT(x, n)
        F::RIGHT => {
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
                    name: "RIGHT".into(),
                    args: vec![x, arg(1)??.0],
                },
                out_ty,
            )
        }

        F::RTRIM => ok(
            Expression::FunctionCall {
                name: "RTRIM".into(),
                args: vec![arg(0)??.0],
            },
            FieldType::Memo,
        ),

        // STOD(x) => COALESCE(TO_DATE(NULLIF(TRIM(x),''),'YYYYMMDD'),'0001-01-01')
        F::STOD => date("YYYYMMDD"),
        // STR(num, len, dec) => PRINTF("%{len}.{dec}f", num)
        F::STR => {
            match get_str_fn_args(args, cx)? {
                StrArgs::WithArgs(val_arg, fmt, len) => {
                    let expression = expr_ref(Expression::FunctionCall {
                        name: "TO_CHAR".to_string(),
                        args: vec![
                            val_arg, // value to be formatted
                            expr_ref(fmt.into()),
                        ],
                    });
                    //if the length of the evaluated expression is greater than the specified len, fill the len with asterisks instead of showing any value at all
                    let len_expr: std::rc::Rc<std::cell::RefCell<Expression>> =
                        expr_ref(Expression::FunctionCall {
                            name: "LENGTH".into(),
                            args: vec![expression.clone()],
                        });
                    let cond = expr_ref(Expression::BinaryOperator(
                        len_expr,
                        BinaryOp::Le,
                        expr_ref((len as i64).into()),
                        Parenthesize::No,
                    ));
                    let asterisks = "*".repeat(len);
                    let iif = Expression::Iif {
                        cond,
                        when_true: expression,
                        when_false: expr_ref(asterisks.into()),
                    };
                    ok(iif, FieldType::Character(len as u32))
                }
                StrArgs::WithoutArgs(val_arg) => {
                    ok(Expression::Cast(val_arg, "text"), FieldType::Memo)
                }
            }
        }
        F::SUBSTR => {
            let len: u32 = match &*arg(2)??.0.borrow() {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(2)),
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
        F::TRIM => ok(
            Expression::FunctionCall {
                name: "RTRIM".into(),
                args: vec![arg(0)??.0],
            },
            FieldType::Memo,
        ),
        F::UPPER => {
            let (first, ty) = arg(0)??;
            ok(
                Expression::FunctionCall {
                    name: "UPPER".into(),
                    args: vec![first],
                },
                ty,
            )
        }
        // VAL(x) => CAST (x as numeric)
        F::VAL => ok(
            Expression::Cast(arg(0)??.0, "numeric"),
            FieldType::Numeric { len: 0, dec: 0 },
        ),

        // YEAR(x) => DATE_PART('YEAR', x)
        F::YEAR => ok(
            Expression::FunctionCall {
                name: "DATE_PART".into(),
                args: vec![expr_ref("YEAR".into()), arg(0)??.0],
            },
            FieldType::Double,
        ),

        F::Unknown(unknown) => match cx.custom_function(unknown) {
            Some(v) => cx.translate(&v),
            None => Err(Error::UnsupportedFunction(unknown.clone())),
        },
    }
}

pub fn translate_binary_op<T: TranslationContext>(
    cx: &T,
    ast_l: &ast::Expression,
    op: &ast::BinaryOp,
    r: &ast::Expression,
) -> Result {
    let (l, ty) = translate(ast_l, cx)?;
    translate_binary_op_right(cx, ast_l, l, ty, op, r)
}

/// The same as translate_binary_op but useful if you've already translated l and don't want to do it again
pub fn translate_binary_op_right<T: TranslationContext>(
    cx: &T,
    ast_l: &ast::Expression,
    l: ExprRef,
    ty: FieldType,
    op: &ast::BinaryOp,
    r: &ast::Expression,
) -> Result {
    let tr_binop = |l, op, r, ty| ok(Expression::BinaryOperator(l, op, r, Parenthesize::Yes), ty);
    let binop = |l, op, r, ty| {
        //OPT: order of operations is preserved by parenthesizing everything.
        // It'd be nice to analyze precedence to only do so when necessary.
        let r = translate(r, cx)?.0;
        tr_binop(l, op, r, ty)
    };
    match (op, ty) {
        // For these types, simple addition is fine
        (
            ast::BinaryOp::Add,
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Numeric { .. }
            | FieldType::Date,
        ) => binop(l, BinaryOp::Add, r, ty),
        (
            ast::BinaryOp::Sub,
            FieldType::Double | FieldType::Float | FieldType::Integer | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Sub, r, ty),

        // Subtracting from a date will "just work" but we need to change
        //  the returned type to numeric (number of days)
        (ast::BinaryOp::Sub, FieldType::Date) => {
            binop(l, BinaryOp::Sub, r, FieldType::Numeric { len: 99, dec: 0 })
        }

        // Add on a character type maps to CONCAT
        (ast::BinaryOp::Add, FieldType::Character(_) | FieldType::Memo) => {
            binop(l, BinaryOp::Concat, r, FieldType::Memo)
        }
        // Sub on a character type also maps to CONCAT but with the
        //  trailing spaces of the first argument "moved" to the end
        //  of the result. We can map this as:
        //
        // CONCAT(
        //   RTRIM(l),
        //   r,
        //   REPEAT(' ', LENGTH(l) - LENGTH( RTRIM(l) ))
        // )
        //
        (ast::BinaryOp::Sub, FieldType::Character(_) | FieldType::Memo) => {
            let without_spaces = expr_ref(Expression::FunctionCall {
                name: "RTRIM".into(),
                args: vec![l.clone()],
            });
            let length_without_spaces = expr_ref(Expression::FunctionCall {
                name: "LENGTH".into(),
                args: vec![without_spaces.clone()],
            });
            let length_with_spaces = expr_ref(Expression::FunctionCall {
                name: "LENGTH".into(),
                args: vec![l],
            });
            let num_spaces = expr_ref(Expression::BinaryOperator(
                length_with_spaces,
                BinaryOp::Sub,
                length_without_spaces,
                Parenthesize::No,
            ));
            let repeated_spaces = expr_ref(Expression::FunctionCall {
                name: "REPEAT".into(),
                args: vec![expr_ref("' '".into()), num_spaces],
            });
            ok(
                Expression::FunctionCall {
                    name: "CONCAT".into(),
                    args: vec![without_spaces, translate(r, cx)?.0, repeated_spaces],
                },
                FieldType::Memo,
            )
        }

        // Mul and Div are numeric only
        (
            ast::BinaryOp::Mul,
            FieldType::Double | FieldType::Float | FieldType::Integer | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Mul, r, ty),
        (
            ast::BinaryOp::Div,
            FieldType::Double | FieldType::Float | FieldType::Integer | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Div, r, ty),
        // Numbers, bools, and single characters get actual equality
        (
            ast::BinaryOp::Eq,
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Logical
            | FieldType::Date
            | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Eq, r, FieldType::Logical),
        (
            ast::BinaryOp::Ne,
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Logical
            | FieldType::Date
            | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Ne, r, FieldType::Logical),
        (
            ast::BinaryOp::Lt,
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Logical
            | FieldType::Date
            | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Lt, r, FieldType::Logical),
        (
            ast::BinaryOp::Le,
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Logical
            | FieldType::Date
            | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Le, r, FieldType::Logical),
        (
            ast::BinaryOp::Gt,
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Logical
            | FieldType::Date
            | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Gt, r, FieldType::Logical),
        (
            ast::BinaryOp::Ge,
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Logical
            | FieldType::Date
            | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Ge, r, FieldType::Logical),

        // AND and OR are only for Logical
        (ast::BinaryOp::And, FieldType::Logical) => binop(l, BinaryOp::And, r, FieldType::Logical),
        (ast::BinaryOp::Or, FieldType::Logical) => binop(l, BinaryOp::Or, r, FieldType::Logical),
        (ast::BinaryOp::Lt, FieldType::Character(len)) => {
            let r_tr = translate(r, cx)?.0;
            let left = cx.string_comp_left(l, r_tr.clone());
            let right = cx.string_comp_right(r_tr, len);
            tr_binop(left, BinaryOp::Lt, right, FieldType::Logical)
        }
        (ast::BinaryOp::Le, FieldType::Character(len)) => {
            let r_tr = translate(r, cx)?.0;
            let left = cx.string_comp_left(l, r_tr.clone());
            let right = cx.string_comp_right(r_tr, len);
            tr_binop(left, BinaryOp::Le, right, FieldType::Logical)
        }
        (ast::BinaryOp::Gt, FieldType::Character(len)) => {
            let r_tr = translate(r, cx)?.0;
            let left = cx.string_comp_left(l, r_tr.clone());
            let right = cx.string_comp_right(r_tr, len);
            tr_binop(left, BinaryOp::Gt, right, FieldType::Logical)
        }
        (ast::BinaryOp::Ge, FieldType::Character(len)) => {
            let r_tr = translate(r, cx)?.0;
            let left = cx.string_comp_left(l, r_tr.clone());
            let right = cx.string_comp_right(r_tr, len);
            tr_binop(left, BinaryOp::Ge, right, FieldType::Logical)
        }
        (ast::BinaryOp::Lt, FieldType::Memo) => {
            let left = cx.string_comp_left(l, translate(r, cx)?.0);
            binop(left, BinaryOp::Lt, r, FieldType::Logical)
        }
        (ast::BinaryOp::Le, FieldType::Memo) => {
            let left = cx.string_comp_left(l, translate(r, cx)?.0);
            binop(left, BinaryOp::Le, r, FieldType::Logical)
        }
        (ast::BinaryOp::Gt, FieldType::Memo) => {
            let left = cx.string_comp_left(l, translate(r, cx)?.0);
            binop(left, BinaryOp::Gt, r, FieldType::Logical)
        }
        (ast::BinaryOp::Ge, FieldType::Memo) => {
            let left = cx.string_comp_left(l, translate(r, cx)?.0);
            binop(left, BinaryOp::Ge, r, FieldType::Logical)
        }
        (ast::BinaryOp::Eq, FieldType::Memo | FieldType::Character(_)) if is_trim(ast_l) => {
            binop(l, BinaryOp::Eq, r, FieldType::Logical)
        }
        (ast::BinaryOp::Ne, FieldType::Memo | FieldType::Character(_)) if is_trim(ast_l) => {
            binop(l, BinaryOp::Ne, r, FieldType::Logical)
        }
        (ast::BinaryOp::Eq, FieldType::Memo) => {
            binop(l, BinaryOp::StartsWith, r, FieldType::Logical)
        }
        (ast::BinaryOp::Eq, FieldType::Character(len)) => {
            let trimmed_r = cx.string_comp_right(translate(r, cx)?.0, len);
            tr_binop(l, BinaryOp::StartsWith, trimmed_r, FieldType::Logical)
        }
        (ast::BinaryOp::Ne, FieldType::Memo) => {
            let starts_with = binop(l, BinaryOp::StartsWith, r, FieldType::Logical);
            let expr = Expression::UnaryOperator(UnaryOp::Not, starts_with?.0);
            ok(expr, FieldType::Logical)
        }
        (ast::BinaryOp::Ne, FieldType::Character(len)) => {
            let trimmed_r = cx.string_comp_right(translate(r, cx)?.0, len);
            let starts_with = tr_binop(l, BinaryOp::StartsWith, trimmed_r, FieldType::Logical);
            let expr = Expression::UnaryOperator(UnaryOp::Not, starts_with?.0);
            ok(expr, FieldType::Logical)
        }
        (
            ast::BinaryOp::Eq,
            FieldType::CharacterBinary(_) | FieldType::General | FieldType::MemoBinary,
        ) => binop(l, BinaryOp::Eq, r, FieldType::Logical),
        (
            ast::BinaryOp::Ne,
            FieldType::CharacterBinary(_) | FieldType::General | FieldType::MemoBinary,
        ) => binop(l, BinaryOp::Ne, r, FieldType::Logical),

        // SQL doesn't have an exponentation operator, use the POW function
        (ast::BinaryOp::Exp, FieldType::Integer) => ok(
            Expression::FunctionCall {
                name: "POW".to_string(),
                args: vec![l, translate(r, cx)?.0],
            },
            ty,
        ),

        // SQL doesn't have a contain operator, use the STRPOS function
        //NOTE(justin): not using LIKE here because the needle might contain
        // LIKE wildcards (% and _).
        (ast::BinaryOp::Contain, FieldType::Character(_)) => {
            let strpos = expr_ref(Expression::FunctionCall {
                name: "STRPOS".to_string(),
                // Note that in CodeBase the haystack is the right arg
                args: vec![translate(r, cx)?.0, l],
            });
            ok(Expression::Cast(strpos, "bool"), FieldType::Logical)
        }

        (op, ty) => Err(Error::Other(format!(
            "Unsupported operator/type combination: {op:?} and {ty:?}"
        ))),
    }
}

pub enum StrArgs {
    WithArgs(ExprRef, String, usize),
    WithoutArgs(ExprRef),
}

pub fn get_str_fn_args(
    args: &[Box<ast::Expression>],
    cx: &impl TranslationContext,
) -> std::result::Result<StrArgs, Error> {
    if args.len() == 1 {
        let (val_arg, _) = get_arg(0, args, cx, &F::STR)??;
        return Ok(StrArgs::WithoutArgs(val_arg));
    }

    let arg = |index: usize| get_arg(index, args, cx, &F::STR);
    let wrong_type = |index| wrong_type(index, &F::STR, args);

    let val_arg = arg(0)??.0;
    let len_arg = arg(1)??.0;
    let dec_arg = arg(2)??.0;

    // `len` and dec` must be constants according to CB docs, so we can get them and convert to integers
    let len: i64 = match &*len_arg.borrow() {
        Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(1)),
        _ => Err(wrong_type(1)),
    }?;
    let len: usize = len
        .try_into()
        .map_err(|_| Error::Other("STR length must be a positive integer".into()))?;
    let dec: i64 = match &*dec_arg.borrow() {
        Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(2)),
        _ => Err(wrong_type(2)),
    }?;
    let dec: usize = dec
        .try_into()
        .map_err(|_| Error::Other("STR dec must be a positive integer".into()))?;

    //clamp dec to 15 (codebase max)
    let mut dec: usize = dec.min(15);

    if len <= dec + 1 {
        dec = (len.saturating_sub(2)).max(0); //to allow space for the '.', something like 2,1 doesn't make sense since there would be no space for the leading 0 so codebase just removes the dec
    }

    let fmt = if dec > 0 {
        let x = len - dec - 1;
        let y = dec;
        format!("FM{:9<x$}0.{:0<y$}", "", "")
    } else {
        let x = len;
        format!("FM{:9<x$}0", "")
    };

    Ok(StrArgs::WithArgs(val_arg, fmt, len))
}

pub fn get_arg(
    index: usize,
    args: &[Box<ast::Expression>],
    cx: &impl TranslationContext,
    name: &F,
) -> std::result::Result<std::result::Result<(ExprRef, FieldType), Error>, Error> {
    args.get(index)
        .map(|a| translate(a, cx))
        .ok_or(Error::IncorrectArgCount(format!("{name:?}"), index))
}

pub fn get_all_args(
    args: &[Box<ast::Expression>],
    cx: &impl TranslationContext,
) -> std::result::Result<Vec<ExprRef>, Error> {
    args.iter().map(|a| translate(a, cx).map(|r| r.0)).collect()
}

pub fn wrong_type(index: usize, name: &F, args: &[Box<ast::Expression>]) -> Error {
    Error::ArgWrongType {
        func: ast::Expression::FunctionCall {
            name: name.clone(),
            args: args.into(),
        },
        wrong_arg_index: index,
    }
}

fn is_trim(ast_l: &ast::Expression) -> bool {
    matches!(
        ast_l,
        ast::Expression::FunctionCall { name, .. }
            if *name == crate::codebase_functions::CodebaseFunction::TRIM
    )
}

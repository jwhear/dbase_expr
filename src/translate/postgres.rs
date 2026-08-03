use std::borrow::Cow;

use super::{
    BinaryOp, Error, ExpResult, Expression, FieldType, Parenthesize, SQLTree, TranslationContext,
    UnaryOp, escape_single_quotes, exps, ok,
};
use crate::{
    codebase_functions::CodebaseFunction as F,
    parser::{self, Expression as E, ExpressionId, ParseTree},
};

/// This type provides default function translation for Postgres. You can
///  "inherit" while allowing overriding by implementing the TranslationContext
///  trait and dispatching to `translate_fn_call` any function calls you're not
///  interested in overriding.
pub struct Translator<'fl, F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(Cow<'fl, str>, FieldType), String>,
{
    pub field_lookup: F,
}

impl<'fl, F> TranslationContext for Translator<'fl, F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(Cow<'fl, str>, FieldType), String>,
{
    fn lookup_field<'field_lookup>(
        &'field_lookup self,
        alias: Option<&str>,
        field: &str,
    ) -> std::result::Result<(Cow<'field_lookup, str>, FieldType), String> {
        (self.field_lookup)(alias, field)
    }

    fn translate_expr<'field_lookup, 'parse>(
        &'field_lookup self,
        source: &'parse parser::Expression,
        src_tree: &'parse crate::parser::ParseTree,
        dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    ) -> ExpResult<'field_lookup, 'parse> {
        translate_expr(source, src_tree, dst_tree, self)
    }

    fn translate_binary_op<'field_lookup, 'parse>(
        &'field_lookup self,
        l: &'parse parser::Expression,
        op: &'parse parser::BinaryOp,
        r: &'parse parser::Expression,
        src_tree: &'parse crate::parser::ParseTree,
        dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    ) -> ExpResult<'field_lookup, 'parse> {
        translate_binary_op(self, l, op, r, src_tree, dst_tree)
    }

    fn translate_fn_call<'field_lookup, 'parse>(
        &'field_lookup self,
        name: &'parse crate::codebase_functions::CodebaseFunction,
        args: &'parse [parser::ExpressionId],
        src_tree: &'parse crate::parser::ParseTree,
        dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    ) -> ExpResult<'field_lookup, 'parse> {
        translate_fn_call(name, args, src_tree, dst_tree, self)
    }
}

/// Translates a parsed dBase expression to a SQL expression.
pub fn translate<'field_lookup, 'parse, C: TranslationContext>(
    tree: &'parse crate::parser::ParseTree<'parse>,
    cx: &'field_lookup C,
) -> ExpResult<'field_lookup, 'parse> {
    let root = tree.get_root().ok_or(Error::EmptyTree)?;
    let mut dst_tree = SQLTree::new();
    translate_expr(root, tree, &mut dst_tree, cx)
}

/// Translates a particular dBase expression to a SQL expression.
pub fn translate_expr<'field_lookup, 'parse, C: TranslationContext>(
    source: &'parse E<'parse>,
    src_tree: &'parse crate::parser::ParseTree<'parse>,
    dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    cx: &'field_lookup C,
) -> ExpResult<'field_lookup, 'parse> {
    // helper for creating binary operators
    match source {
        E::BoolLiteral(v) => ok(Expression::BoolLiteral(*v), FieldType::Logical),
        E::NumberLiteral(v) => {
            let dec = v
                .iter()
                .position(|&c| c == b'.')
                .map(|i| v.len() - i)
                .unwrap_or(0) as u32;
            let v = unsafe { std::str::from_utf8_unchecked(v) };
            ok(
                Expression::NumberLiteral(Cow::from(v)),
                FieldType::Numeric {
                    len: v.len() as u32,
                    dec,
                },
            )
        }
        E::StringLiteral(v) => {
            let v = unsafe { std::str::from_utf8_unchecked(v) };
            let v = escape_single_quotes(v);
            let len = v.len();
            ok(
                Expression::SingleQuoteStringLiteral(v),
                FieldType::Character(len as u32),
            )
        }
        E::Field { alias, name } => {
            let alias = alias.map(|v| unsafe { std::str::from_utf8_unchecked(v) });
            let name = unsafe { std::str::from_utf8_unchecked(name) };
            let (name, field_type) = cx
                .lookup_field(alias, name)
                .map_err(|m| Error::InvalidField(name.into(), m))?;
            ok(Expression::Field { name, field_type }, field_type)
        }
        E::UnaryOperator(op, r) => {
            let r = src_tree.get_expr_unchecked(*r);
            let (r, t) = translate_expr(r, src_tree, dst_tree, cx)?;
            let r = dst_tree.push_expr(r);
            match op {
                parser::UnaryOp::Not => ok(
                    Expression::UnaryOperator(UnaryOp::Not, r),
                    FieldType::Logical,
                ),
                parser::UnaryOp::Neg => ok(Expression::UnaryOperator(UnaryOp::Neg, r), t),
            }
        }
        E::BinaryOperator(l, op, r) => {
            // Add, Sub are ambiguous: could be numeric, concat, or days (for dates)
            // We translate the first operand and use its type to determine how
            //  to translate.
            cx.translate_binary_op(
                src_tree.get_expr_unchecked(*l),
                op,
                src_tree.get_expr_unchecked(*r),
                src_tree,
                dst_tree,
            )
        }
        E::FunctionCall { name, args } => {
            cx.translate_fn_call(name, src_tree.get_args(args), src_tree, dst_tree)
        }
        E::Sequence(operands, op) => {
            // We'll inspect the type of the first operand and use that to
            //  either emit a '+' or a '||'
            assert!(
                operands.len() >= 2,
                "Sequence operation should only be generated for at least two operands"
            );
            let operands = src_tree.get_args(operands);
            //TODO use scratch buffer here
            let mut exprs = Vec::with_capacity(operands.len());
            let mut first_ty = None;
            for (i, operand) in operands.iter().enumerate() {
                let (expr, ty) =
                    cx.translate_expr(src_tree.get_expr_unchecked(*operand), src_tree, dst_tree)?;
                if i == 0 {
                    first_ty = Some(ty);
                }
                exprs.push((dst_tree.push_expr(expr), ty));
            }
            let first_ty = first_ty.unwrap();
            let (operator, ty) = match (op, first_ty) {
                (&parser::BinaryOp::Add, FieldType::Character(_)) => {
                    let ty = exprs
                        .iter()
                        .try_fold(0u32, |acc, expr| match expr.1 {
                            FieldType::Character(len) => Some(acc + len),
                            _ => None,
                        })
                        .map_or(FieldType::Memo, FieldType::Character);
                    (BinaryOp::Concat, ty)
                }
                (&parser::BinaryOp::Add, FieldType::Memo | FieldType::MemoBinary) => {
                    (BinaryOp::Concat, first_ty)
                }
                (&parser::BinaryOp::Add, _) => (BinaryOp::Add, first_ty),
                (&parser::BinaryOp::Sub, _) => (BinaryOp::Sub, first_ty),
                _ => panic!("Unsupported binary operator for Sequence: {op:?}"),
            };

            let exprs = dst_tree.push_args(exprs.into_iter().map(|(e, _)| e));
            ok(Expression::BinaryOperatorSequence(operator, exprs), ty)
        }
    }
}

// This function does the kind of gross work of converting dBase function calls
//  to the SQL equivalent.  Some are super straightforward: `CHR(97)` -> `CHR(97)`
//  but others have no exact equivalent and have to resolve to a nested bundle.
pub fn translate_fn_call<'parse, 'field_lookup>(
    name: &'parse F,
    args: &'parse [parser::ExpressionId],
    src_tree: &'parse crate::parser::ParseTree<'parse>,
    dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    cx: &'field_lookup impl TranslationContext,
) -> std::result::Result<(Expression<'field_lookup, 'parse>, FieldType), Error> {
    // This recursively translates all arguments and packs them into dst_tree,
    //  returning their ExpressionIds and FieldTypes
    let dst_args = translate_args(args, src_tree, dst_tree, cx)?;

    // Gets the ExpressionId for the argument at `index`
    let argid = |index| {
        dst_args
            .get(index)
            .map(|&(a, _)| a)
            .ok_or(Error::IncorrectArgCount(format!("{name:?}"), index))
    };

    // Gets the FieldType for the argument at `index`
    let argtype = |index| {
        dst_args
            .get(index)
            .map(|&(_, a)| a)
            .ok_or(Error::IncorrectArgCount(format!("{name:?}"), index))
    };

    // Gets the translated Expression for the argument at `index`
    let argexpr = |index| {
        dst_args
            .get(index)
            .map(|&(a, _)| a)
            .and_then(|a| dst_tree.get_expr(a))
            .ok_or(Error::IncorrectArgCount(format!("{name:?}"), index))
    };

    let wrong_type = |index| wrong_type(index, name, args);

    fn date<'field_lookup, 'parse>(
        format: &'parse str,
        value: ExpressionId,
        dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    ) -> Result<(Expression<'field_lookup, 'parse>, FieldType), Error> {
        //this translates blank strings into the coalesce date so that it can be properly compared
        let format = dst_tree.push_expr(format.into());
        let trim = dst_tree.push_fn_call("TRIM", &[value]);
        let null_if = dst_tree.push_fn_call("NULLIF", &[trim, exps::EMPTY_STR]);
        let to_date = dst_tree.push_fn_call("TO_DATE", &[null_if, format]);
        let coalesce = Expression::FunctionCall {
            name: "COALESCE",
            args: dst_tree.push_args([to_date, exps::COALESCE_DATE].into_iter()),
        };
        Ok((coalesce, FieldType::Date))
    }

    match name {
        // ALLTRIM(x) => TRIM(x)
        F::ALLTRIM | F::LTRIM | F::RTRIM | F::TRIM => {
            let ty = match argtype(0)? {
                FieldType::Character(l) => FieldType::Character(l),
                _ => FieldType::Memo,
            };
            let name = match name {
                F::ALLTRIM | F::TRIM => "TRIM",
                F::LTRIM => "LTRIM",
                F::RTRIM => "RTRIM",
                _ => unreachable!(),
            };
            let expr = Expression::FunctionCall {
                name,
                args: dst_tree.push_args([argid(0)?].into_iter()),
            };
            ok(expr, ty)
        }
        // CHR(x) => CHR(x)
        F::CHR => ok(
            Expression::FunctionCall {
                name: "CHR",
                args: dst_tree.push_args([argid(0)?].into_iter()),
            },
            FieldType::Character(1),
        ),
        // CTOD(x) => COALESCE(TO_DATE(NULLIF(TRIM(x),''),'MM/DD/YY'),'0001-01-01')
        F::CTOD => date("MM/DD/YY", argid(0)?, dst_tree),
        // DATE() => CURRENT_DATE
        F::DATE => ok(
            Expression::BareFunctionCall("CURRENT_DATE"),
            FieldType::Date,
        ),
        // DAY(x) => DATE_PART('DAY', x)
        F::DAY => ok(
            Expression::FunctionCall {
                name: "DATE_PART",
                args: dst_tree.push_args([exps::LIT_DAY, argid(0)?].into_iter()),
            },
            FieldType::Double,
        ),
        // DELETED() => __deleted
        F::DELETED => {
            let (name, field_type) = cx
                .lookup_field(None, "__deleted")
                .map_err(|m| Error::InvalidField("__deleted".into(), m))?;
            ok(Expression::Field { name, field_type }, field_type)
        }

        // DTOC(x) => TO_CHAR(x, 'MM/DD/YY')
        F::DTOC => {
            // Equivalent to DTOS
            let fmt = dst_tree.push_expr(if args.len() == 2 {
                "YYYYMMDD".into()
            } else {
                "MM/DD/YY".into()
            });
            ok(
                Expression::FunctionCall {
                    name: "TO_CHAR",
                    args: dst_tree.push_args([argid(0)?, fmt].into_iter()),
                },
                FieldType::Character(8),
            )
        }
        F::DTOS => {
            let fmt = dst_tree.push_expr("YYYYMMDD".into());
            ok(
                Expression::FunctionCall {
                    name: "TO_CHAR",
                    args: dst_tree.push_args([argid(0)?, fmt].into_iter()),
                },
                FieldType::Character(8),
            )
        }

        //the result of EMPTY depends on the type
        F::EMPTY => {
            let expression = match argtype(0)? {
                FieldType::Logical => {
                    // COALESCE(x, false) = false
                    let coalesce = dst_tree.push_fn_call("COALESCE", &[argid(0)?, exps::LIT_FALSE]);
                    Expression::BinaryOperator(
                        coalesce,
                        BinaryOp::Eq,
                        dst_tree.push_expr(Expression::BoolLiteral(false)),
                        Parenthesize::No,
                    )
                }
                FieldType::Integer
                | FieldType::Currency
                | FieldType::Double
                | FieldType::Float
                | FieldType::Numeric { .. } => {
                    // COALESCE(x, 0) = 0
                    let coalesce = dst_tree.push_fn_call("COALESCE", &[argid(0)?, exps::LIT_0]);
                    Expression::BinaryOperator(
                        coalesce,
                        BinaryOp::Eq,
                        exps::LIT_0,
                        Parenthesize::No,
                    )
                }
                FieldType::Character(_) | FieldType::Memo => {
                    let trim = dst_tree.push_fn_call("TRIM", &[argid(0)?]);
                    Expression::BinaryOperator(
                        trim,
                        BinaryOp::Eq,
                        exps::EMPTY_STR,
                        Parenthesize::No,
                    )
                }
                FieldType::Date | FieldType::DateTime => Expression::BinaryOperator(
                    argid(0)?,
                    BinaryOp::Eq,
                    exps::COALESCE_DATE,
                    Parenthesize::No,
                ),
                FieldType::MemoBinary | FieldType::CharacterBinary(_) | FieldType::General => {
                    // COALESCE(LENGTH(x), 0) = 0
                    let length_call = dst_tree.push_fn_call("length", &[argid(0)?]);
                    let coalesce_call =
                        dst_tree.push_fn_call("COALESCE", &[length_call, exps::LIT_0]);
                    Expression::BinaryOperator(
                        coalesce_call,
                        BinaryOp::Eq,
                        exps::LIT_0,
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
            let true_ty = argtype(1)?;
            let false_ty = argtype(2)?;
            let ty = match (true_ty, false_ty) {
                (FieldType::Character(true_len), FieldType::Character(false_len)) => {
                    FieldType::Character(true_len.max(false_len)) //get the max of the two because the length shouldn't depend on the values
                }
                _ => true_ty, // otherwise the result type will be the type of the when_true expression
            };

            //TODO use scratch here
            let mut branches = Vec::new();
            // We have to take ownership of args for the loop to work
            //OPT: come back and try to rework this
            let mut inner_args = args;

            // Add this IIF as a CaseBranch. If when_false is an IIF, traverse
            //  into it and repeat. We'll eventually encounter a when_false that
            //  is not an IIF: that will become our ELSE value.
            let r#else = loop {
                match inner_args {
                    [cond, when_true, when_false] => {
                        // Convert this IIF to a WHEN
                        let cond = cx
                            .translate_expr(src_tree.get_expr_unchecked(*cond), src_tree, dst_tree)?
                            .0;
                        let cond = dst_tree.push_expr(cond);
                        let then = cx
                            .translate_expr(
                                src_tree.get_expr_unchecked(*when_true),
                                src_tree,
                                dst_tree,
                            )?
                            .0;
                        let then = dst_tree.push_expr(then);
                        branches.push(dst_tree.push_expr(Expression::CaseBranch { cond, then }));

                        let when_false = src_tree.get_expr_unchecked(*when_false);
                        let parser::Expression::FunctionCall { name: F::IIF, args } = when_false
                        else {
                            break when_false;
                        };
                        // Go around with another branch
                        inner_args = src_tree.get_args(args);
                    }
                    _ => panic!("IIF should always have three arguments"),
                }
            };
            let (r#else, _) = cx.translate_expr(r#else, src_tree, dst_tree)?;
            let r#else = dst_tree.push_expr(r#else);

            ok(
                Expression::Case {
                    branches: dst_tree.push_args(branches.into_iter()),
                    r#else,
                },
                ty,
            )
        }
        // LEFT(x, n) => SUBSTR(x, 1, n)
        F::LEFT => ok(
            Expression::FunctionCall {
                name: "SUBSTR",
                args: dst_tree.push_args([argid(0)?, exps::LIT_1, argid(1)?].into_iter()),
            },
            FieldType::Memo,
        ),

        // MONTH(x) => DATE_PART('MONTH', x)
        F::MONTH => ok(
            Expression::FunctionCall {
                name: "DATE_PART",
                args: dst_tree.push_args([exps::LIT_MONTH, argid(0)?].into_iter()),
            },
            FieldType::Double,
        ),

        F::PADL => ok(
            Expression::FunctionCall {
                name: "LPAD",
                //NOTE: Postgres uses spaces as the fill char by default so we
                //  omit the third argument
                args: dst_tree.push_args([argid(0)?, argid(1)?].into_iter()),
            },
            FieldType::Memo,
        ),

        // RECNO() => RECNO5
        F::RECNO => {
            let (name, field_type) = cx
                .lookup_field(None, "RECNO5")
                .map_err(|m| Error::InvalidField("RECNO5".into(), m))?;
            ok(Expression::Field { name, field_type }, field_type)
        }

        // RIGHT(x, n) => RIGHT(x, n)
        F::RIGHT => {
            let x = argid(0)?;
            let n = match argexpr(1)? {
                Expression::NumberLiteral(v) => v.parse::<u32>().map_err(|_| wrong_type(1)),
                _ => Err(wrong_type(1)),
            }?;
            let out_ty = match argtype(0)? {
                FieldType::Character(len) => FieldType::Character(len - n),
                _ => FieldType::Memo,
            };
            ok(
                Expression::FunctionCall {
                    name: "RIGHT",
                    args: dst_tree.push_args([x, argid(1)?].into_iter()),
                },
                out_ty,
            )
        }

        // STOD(x) => COALESCE(TO_DATE(NULLIF(TRIM(x),''),'YYYYMMDD'),'0001-01-01')
        F::STOD => date("YYYYMMDD", argid(0)?, dst_tree),
        // STR(num, len, dec) => PRINTF("%{len}.{dec}f", num)
        F::STR => {
            match get_str_fn_args(args, src_tree, dst_tree, cx)? {
                StrArgs::WithArgs {
                    val_arg, fmt, len, ..
                } => {
                    let fmt = dst_tree.push_expr(fmt.into());
                    let expression = dst_tree.push_fn_call("TO_CHAR", &[val_arg, fmt]);
                    //if the length of the evaluated expression is greater than the specified len, fill the len with asterisks instead of showing any value at all
                    let len_expr = dst_tree.push_fn_call("LENGTH", &[expression]);
                    let rhs = dst_tree.push_expr((len as i64).into());
                    let cond = dst_tree.push_expr(Expression::BinaryOperator(
                        len_expr,
                        BinaryOp::Le,
                        rhs,
                        Parenthesize::No,
                    ));
                    let asterisks = "*".repeat(len);
                    let iif = Expression::Iif {
                        cond,
                        when_true: expression,
                        when_false: dst_tree.push_expr(asterisks.into()),
                    };
                    ok(iif, FieldType::Character(len as u32))
                }
                StrArgs::WithoutArgs(val_arg) => {
                    ok(Expression::Cast(val_arg, "text"), FieldType::Memo)
                }
            }
        }
        F::SUBSTR => translate_substr("SUBSTR", args, src_tree, dst_tree, cx),
        F::UPPER => ok(
            Expression::FunctionCall {
                name: "UPPER",
                args: dst_tree.push_args([argid(0)?].into_iter()),
            },
            argtype(0)?,
        ),
        // VAL(x) => CASE WHEN pg_input_is_valid(x, 'numeric') THEN CAST(x AS NUMERIC) ELSE 0 END
        F::VAL => {
            let numeric =
                dst_tree.push_expr(Expression::SingleQuoteStringLiteral("numeric".into()));
            let cond = dst_tree.push_fn_call("pg_input_is_valid", &[argid(0)?, numeric]);
            let when_true = dst_tree.push_expr(Expression::Cast(argid(0)?, "numeric"));
            // codebase inteprets any non-numeric string as a 0
            let when_false = exps::LIT_0;
            ok(
                Expression::Iif {
                    cond,
                    when_true,
                    when_false,
                },
                FieldType::Numeric { len: 0, dec: 0 },
            )
        }

        // YEAR(x) => DATE_PART('YEAR', x)
        F::YEAR => ok(
            Expression::FunctionCall {
                name: "DATE_PART",
                args: dst_tree.push_args([exps::LIT_YEAR, argid(0)?].into_iter()),
            },
            FieldType::Double,
        ),

        F::Unknown(unknown) => Err(Error::UnsupportedFunction(unknown.clone())),
    }
}

pub fn translate_binary_op<'parse, 'field_lookup, T: TranslationContext>(
    cx: &'field_lookup T,
    ast_l: &'parse parser::Expression<'parse>,
    op: &'parse parser::BinaryOp,
    r: &'parse parser::Expression<'parse>,
    src_tree: &'parse crate::parser::ParseTree<'parse>,
    dst_tree: &mut SQLTree<'field_lookup, 'parse>,
) -> ExpResult<'field_lookup, 'parse> {
    let (l, ty) = translate_expr(ast_l, src_tree, dst_tree, cx)?;
    translate_binary_op_right(cx, ast_l, l, ty, op, r, src_tree, dst_tree)
}

/// The same as translate_binary_op but useful if you've already translated l and don't want to do it again
pub fn translate_binary_op_right<'parse, 'field_lookup, T: TranslationContext>(
    cx: &'field_lookup T,
    ast_l: &'parse parser::Expression<'parse>,
    l: Expression<'field_lookup, 'parse>,
    ty: FieldType,
    op: &'parse parser::BinaryOp,
    r: &'parse parser::Expression<'parse>,
    src_tree: &'parse crate::parser::ParseTree<'parse>,
    dst_tree: &mut SQLTree<'field_lookup, 'parse>,
) -> ExpResult<'field_lookup, 'parse> {
    let tr_binop = |l, op, r, ty| ok(Expression::BinaryOperator(l, op, r, Parenthesize::Yes), ty);
    let mut binop = |l, op, r, ty| {
        //OPT: order of operations is preserved by parenthesizing everything.
        // It'd be nice to analyze precedence to only do so when necessary.
        let l = dst_tree.push_expr(l);
        let r = translate_expr(r, src_tree, dst_tree, cx)?.0;
        let r = dst_tree.push_expr(r);
        tr_binop(l, op, r, ty)
    };
    match (op, ty) {
        // For these types, simple addition is fine
        (
            parser::BinaryOp::Add,
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Numeric { .. }
            | FieldType::Date,
        ) => binop(l, BinaryOp::Add, r, ty),
        (
            parser::BinaryOp::Sub,
            FieldType::Double | FieldType::Float | FieldType::Integer | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Sub, r, ty),

        // Subtracting from a date will "just work" but we need to change
        //  the returned type to numeric (number of days)
        (parser::BinaryOp::Sub, FieldType::Date) => {
            binop(l, BinaryOp::Sub, r, FieldType::Numeric { len: 99, dec: 0 })
        }

        // Add on a character type maps to CONCAT
        (parser::BinaryOp::Add, FieldType::Character(len)) => {
            let (r, r_ty) = translate_expr(r, src_tree, dst_tree, cx)?;
            let ty = match r_ty {
                FieldType::Character(r_len) => FieldType::Character(len + r_len), //combine the lengths
                _ => FieldType::Memo, // everything else is a memo
            };
            tr_binop(
                dst_tree.push_expr(l),
                BinaryOp::Concat,
                dst_tree.push_expr(r),
                ty,
            )
        }

        (parser::BinaryOp::Add, FieldType::Memo) => binop(l, BinaryOp::Concat, r, FieldType::Memo),

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
        (parser::BinaryOp::Sub, FieldType::Character(_) | FieldType::Memo) => {
            let l = dst_tree.push_expr(l);
            let r = translate_expr(r, src_tree, dst_tree, cx)?.0;
            let r = dst_tree.push_expr(r);
            let without_spaces = dst_tree.push_fn_call("RTRIM", &[l]);
            let length_without_spaces = dst_tree.push_fn_call("LENGTH", &[without_spaces]);
            let length_with_spaces = dst_tree.push_fn_call("LENGTH", &[l]);
            let num_spaces = dst_tree.push_expr(Expression::BinaryOperator(
                length_with_spaces,
                BinaryOp::Sub,
                length_without_spaces,
                Parenthesize::No,
            ));
            let repeated_spaces = dst_tree.push_fn_call("REPEAT", &[exps::LIT_SPACE, num_spaces]);
            ok(
                Expression::FunctionCall {
                    name: "CONCAT",
                    args: dst_tree.push_args([without_spaces, r, repeated_spaces].into_iter()),
                },
                FieldType::Memo,
            )
        }

        // Mul and Div are numeric only
        (
            parser::BinaryOp::Mul,
            FieldType::Double | FieldType::Float | FieldType::Integer | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Mul, r, ty),
        (
            parser::BinaryOp::Div,
            FieldType::Double | FieldType::Float | FieldType::Integer | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Div, r, ty),
        // Numbers, bools, and single characters get actual equality
        (
            parser::BinaryOp::Eq,
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Logical
            | FieldType::Date
            | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Eq, r, FieldType::Logical),
        (
            parser::BinaryOp::Ne,
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Logical
            | FieldType::Date
            | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Ne, r, FieldType::Logical),
        (
            parser::BinaryOp::Lt,
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Logical
            | FieldType::Date
            | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Lt, r, FieldType::Logical),
        (
            parser::BinaryOp::Le,
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Logical
            | FieldType::Date
            | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Le, r, FieldType::Logical),
        (
            parser::BinaryOp::Gt,
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Logical
            | FieldType::Date
            | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Gt, r, FieldType::Logical),
        (
            parser::BinaryOp::Ge,
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Logical
            | FieldType::Date
            | FieldType::Numeric { .. },
        ) => binop(l, BinaryOp::Ge, r, FieldType::Logical),

        // AND and OR are only for Logical
        (parser::BinaryOp::And, FieldType::Logical) => {
            binop(l, BinaryOp::And, r, FieldType::Logical)
        }
        (parser::BinaryOp::Or, FieldType::Logical) => binop(l, BinaryOp::Or, r, FieldType::Logical),

        // When comparing fixed-len character strings, we have to accomodate
        //  Codebase's 'starts-with' logic
        (
            parser::BinaryOp::Lt
            | parser::BinaryOp::Le
            | parser::BinaryOp::Gt
            | parser::BinaryOp::Ge,
            FieldType::Character(len),
        ) => {
            let l_tr = dst_tree.push_expr(l);
            let r_tr = translate_expr(r, src_tree, dst_tree, cx)?.0;
            let r_tr = dst_tree.push_expr(r_tr);
            let left = cx.string_comp_left(l_tr, r_tr, dst_tree);
            let left = dst_tree.push_expr(left);
            let right = cx.string_comp_right(r_tr, len, dst_tree);
            let right = dst_tree.push_expr(right);
            tr_binop(left, op.try_into().unwrap(), right, FieldType::Logical)
        }
        // Similar logic with a memo
        (
            parser::BinaryOp::Lt
            | parser::BinaryOp::Le
            | parser::BinaryOp::Gt
            | parser::BinaryOp::Ge,
            FieldType::Memo,
        ) => {
            let left = dst_tree.push_expr(l);
            let right = translate_expr(r, src_tree, dst_tree, cx)?.0;
            let right = dst_tree.push_expr(right);
            let left = cx.string_comp_left(left, right, dst_tree);
            tr_binop(
                dst_tree.push_expr(left),
                op.try_into().unwrap(),
                right,
                FieldType::Logical,
            )
        }
        (parser::BinaryOp::Eq, FieldType::Memo | FieldType::Character(_)) if is_trim(ast_l) => {
            binop(l, BinaryOp::Eq, r, FieldType::Logical)
        }
        (parser::BinaryOp::Ne, FieldType::Memo | FieldType::Character(_)) if is_trim(ast_l) => {
            binop(l, BinaryOp::Ne, r, FieldType::Logical)
        }
        (parser::BinaryOp::Eq, FieldType::Memo) => {
            binop(l, BinaryOp::StartsWith, r, FieldType::Logical)
        }
        (parser::BinaryOp::Eq, FieldType::Character(len)) => {
            let right = translate_expr(r, src_tree, dst_tree, cx)?.0;
            let right = dst_tree.push_expr(right);
            let trimmed_r = cx.string_comp_right(right, len, dst_tree);
            let trimmed_r = dst_tree.push_expr(trimmed_r);
            let left = dst_tree.push_expr(l);
            tr_binop(left, BinaryOp::StartsWith, trimmed_r, FieldType::Logical)
        }
        (parser::BinaryOp::Ne, FieldType::Memo) => {
            let starts_with = binop(l, BinaryOp::StartsWith, r, FieldType::Logical)?.0;
            let starts_with = dst_tree.push_expr(starts_with);
            let expr = Expression::UnaryOperator(UnaryOp::Not, starts_with);
            ok(expr, FieldType::Logical)
        }
        (parser::BinaryOp::Ne, FieldType::Character(len)) => {
            let right = translate_expr(r, src_tree, dst_tree, cx)?.0;
            let right = dst_tree.push_expr(right);
            let trimmed_r = cx.string_comp_right(right, len, dst_tree);
            let starts_with = tr_binop(
                dst_tree.push_expr(l),
                BinaryOp::StartsWith,
                dst_tree.push_expr(trimmed_r),
                FieldType::Logical,
            );
            let expr = Expression::UnaryOperator(UnaryOp::Not, dst_tree.push_expr(starts_with?.0));
            ok(expr, FieldType::Logical)
        }
        (
            parser::BinaryOp::Eq,
            FieldType::CharacterBinary(_) | FieldType::General | FieldType::MemoBinary,
        ) => binop(l, BinaryOp::Eq, r, FieldType::Logical),
        (
            parser::BinaryOp::Ne,
            FieldType::CharacterBinary(_) | FieldType::General | FieldType::MemoBinary,
        ) => binop(l, BinaryOp::Ne, r, FieldType::Logical),

        // SQL doesn't have an exponentation operator, use the POW function
        (parser::BinaryOp::Exp, FieldType::Integer) => {
            let l = dst_tree.push_expr(l);
            let exponent = translate_expr(r, src_tree, dst_tree, cx)?.0;
            let exponent = dst_tree.push_expr(exponent);
            ok(
                Expression::FunctionCall {
                    name: "POW",
                    args: dst_tree.push_args([l, exponent].into_iter()),
                },
                ty,
            )
        }

        // SQL doesn't have a contain operator, use the STRPOS function
        //NOTE(justin): not using LIKE here because the needle might contain
        // LIKE wildcards (% and _).
        (parser::BinaryOp::Contain, FieldType::Character(_)) => {
            // Note that in CodeBase the haystack is the right arg
            let haystack = translate_expr(r, src_tree, dst_tree, cx)?.0;
            let haystack = dst_tree.push_expr(haystack);
            let needle = dst_tree.push_expr(l);
            let strpos = dst_tree.push_fn_call("STRPOS", &[haystack, needle]);
            ok(Expression::Cast(strpos, "bool"), FieldType::Logical)
        }

        (op, ty) => Err(Error::Other(format!(
            "Unsupported operator/type combination: {op:?} and {ty:?}"
        ))),
    }
}

pub enum StrArgs {
    WithArgs {
        val_arg: ExpressionId,
        fmt: String,
        len: usize,
        dec: usize,
    },
    WithoutArgs(ExpressionId),
}

pub fn get_str_fn_args<'parse, 'field_lookup>(
    args: &'parse [parser::ExpressionId],
    src_tree: &'parse crate::parser::ParseTree<'parse>,
    dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    cx: &'field_lookup impl TranslationContext,
) -> std::result::Result<StrArgs, Error> {
    let dst_args = translate_args(args, src_tree, dst_tree, cx)?;
    let name = F::STR;

    // Gets the ExpressionId for the argument at `index`
    let argid = |index| {
        dst_args
            .get(index)
            .map(|&(a, _)| a)
            .ok_or(Error::IncorrectArgCount(format!("{name:?}"), index))
    };

    if args.len() == 1 {
        return Ok(StrArgs::WithoutArgs(argid(0)?));
    }

    let wrong_type = |index| wrong_type(index, &F::STR, args);

    if args.len() < 2 {
        return Err(Error::IncorrectArgCount(format!("{name:?}"), 1));
    }

    let val_arg = argid(0)?;
    let len_arg = dst_tree.get_expr_unchecked(dst_args[1].0);

    // `len` and dec` must be constants according to CB docs, so we can get them and convert to integers
    let len: i64 = match len_arg {
        Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(1)),
        _ => Err(wrong_type(1)),
    }?;
    let len: usize = len
        .try_into()
        .map_err(|_| Error::Other("STR length must be a positive integer".into()))?;

    // codebase treats a missing dec arg the same as a zero
    if args.len() == 2 {
        return Ok(StrArgs::WithArgs {
            val_arg,
            fmt: format!("FM{:9<len$}0", ""),
            len,
            dec: 0,
        });
    }

    let dec_arg = dst_tree.get_expr_unchecked(dst_args[2].0);
    let dec: i64 = match dec_arg {
        Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(2)),
        _ => Err(wrong_type(2)),
    }?;
    let dec: usize = dec
        .try_into()
        .map_err(|_| Error::Other("STR dec must be a positive integer".into()))?;

    //clamp dec to 15 (codebase max)
    let mut dec: usize = dec.min(15);

    if len <= dec + 1 {
        dec = len.saturating_sub(2); //to allow space for the '.', something like 2,1 doesn't make sense since there would be no space for the leading 0 so codebase just removes the dec
    }

    let fmt = if dec > 0 {
        let x = len - dec - 1;
        format!("FM{:9<x$}0.{:0<dec$}", "", "")
    } else {
        format!("FM{:9<len$}0", "")
    };

    Ok(StrArgs::WithArgs {
        val_arg,
        fmt,
        len,
        dec,
    })
}

pub fn translate_substr<'parse, 'field_lookup>(
    func: &'static str,
    in_args: &'parse [parser::ExpressionId],
    src_tree: &'parse parser::ParseTree,
    dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    cx: &'field_lookup impl TranslationContext,
) -> std::result::Result<(Expression<'field_lookup, 'parse>, FieldType), Error> {
    let name = F::SUBSTR;
    let mut args = translate_args(in_args, src_tree, dst_tree, cx)?;

    let wrong_type = |index| wrong_type(index, &name, in_args);

    let parsed_index = args
        .get(1)
        .ok_or_else(|| Error::IncorrectArgCount(func.to_string(), 1))?;
    let parsed_index = dst_tree.get_expr_unchecked(parsed_index.0);
    let parsed_index: u32 = match parsed_index {
        Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(1)),
        _ => Err(wrong_type(1)),
    }?;
    if parsed_index == 0 {
        //SUBSTR in codebase treats 0 and 1 exactly the same, replace
        // the 0 with a 1
        args[1] = (exps::LIT_1, FieldType::Integer);
    }

    // Optional length argument--if present we want the result type to be a
    //  fixed length string
    let ty = if let Some(len) = args.get(2) {
        let len = dst_tree.get_expr_unchecked(len.0);
        let len: u32 = match len {
            Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(2)),
            _ => Err(wrong_type(2)),
        }?;
        FieldType::Character(len)
    } else {
        FieldType::Memo
    };

    let args = dst_tree.push_args(args.iter().map(|&(a, _)| a));
    ok(Expression::FunctionCall { name: func, args }, ty)
}

pub fn translate_args<'parse, 'field_lookup>(
    args: &'parse [parser::ExpressionId],
    src_tree: &'parse ParseTree,
    dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    cx: &'field_lookup impl TranslationContext,
) -> Result<Vec<(ExpressionId, FieldType)>, Error> {
    let mut ret = Vec::new();
    for arg in args {
        let arg = src_tree.get_expr_unchecked(*arg);
        let (arg, ft) = translate_expr(arg, src_tree, dst_tree, cx)?;
        ret.push((dst_tree.push_expr(arg), ft));
    }
    Ok(ret)
}

pub fn wrong_type<'a>(index: usize, name: &'a F, _args: &'a [parser::ExpressionId]) -> Error {
    Error::ArgWrongType {
        func_name: format!("{:?}", name),
        wrong_arg_index: index,
    }
}

fn is_trim(ast_l: &parser::Expression) -> bool {
    matches!(
        ast_l,
        parser::Expression::FunctionCall { name, .. }
            if *name == crate::codebase_functions::CodebaseFunction::TRIM
    )
}

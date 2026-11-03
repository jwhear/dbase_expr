use std::borrow::Cow;

use crate::{
    codebase_functions::CodebaseFunction as F,
    parser::{self, ExpressionId, ParseTree},
    translate::{
        BinaryOp as TranslateBinaryOp, Error, ExpResult, Expression, FieldType, Parenthesize,
        SQLTree, TranslationContext, exps, ok,
        postgres::{
            self, translate_args, translate_binary_op_right, translate_expr as default_translate,
            wrong_type,
        },
    },
};

pub struct SqliteTranslator<'field_lookup, F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(Cow<'field_lookup, str>, FieldType), String>,
{
    pub field_lookup: F,
}

impl<'fl, F> TranslationContext for SqliteTranslator<'fl, F>
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
        src_tree: &'parse ParseTree,
        dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    ) -> ExpResult<'field_lookup, 'parse> {
        translate_expr(source, src_tree, dst_tree, self)
    }

    fn translate_fn_call<'field_lookup, 'parse>(
        &'field_lookup self,
        name: &'parse crate::codebase_functions::CodebaseFunction,
        args: &'parse [parser::ExpressionId],
        src_tree: &'parse ParseTree,
        dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    ) -> ExpResult<'field_lookup, 'parse> {
        translate_fn_call(name, args, src_tree, dst_tree, self)
    }

    fn translate_binary_op<'field_lookup, 'parse>(
        &'field_lookup self,
        l: &'parse parser::Expression,
        op: &'parse parser::BinaryOp,
        r: &'parse parser::Expression,
        src_tree: &'parse ParseTree,
        dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    ) -> ExpResult<'field_lookup, 'parse> {
        translate_binary_op(l, op, r, src_tree, dst_tree, self)
    }
}

fn expr_between_right_side<'field_lookup, 'parse>(
    expression: ExpressionId,
    dst_tree: &mut SQLTree<'field_lookup, 'parse>,
) -> Expression<'field_lookup, 'parse> {
    let char = dst_tree.push_expr(Expression::BareFunctionCall("char(0xFFFF)"));
    let appended = dst_tree.push_expr(Expression::BinaryOperator(
        expression,
        TranslateBinaryOp::Concat,
        char,
        Parenthesize::No,
    ));
    Expression::BinaryOperator(
        expression,
        TranslateBinaryOp::And,
        appended,
        Parenthesize::No,
    )
}

/// Date arithmetic. SQLite stores dates as 'YYYY-MM-DD' text and won't do
///  arithmetic on them, so go through julianday:
///
///   date - date => CAST(JULIANDAY(l) - JULIANDAY(r) AS INTEGER)   (days between)
///   date +/- n  => DATE(JULIANDAY(l) +/- n)
///
/// Like the Postgres translation, a non-integer n is rounded to whole days.
fn date_arith<'field_lookup, 'parse>(
    l: ExpressionId,
    op: TranslateBinaryOp,
    r: ExpressionId,
    r_ty: FieldType,
    dst_tree: &mut SQLTree<'field_lookup, 'parse>,
) -> (Expression<'field_lookup, 'parse>, FieldType) {
    let julian_l = dst_tree.push_fn_call("JULIANDAY", &[l]);
    if op == TranslateBinaryOp::Sub && matches!(r_ty, FieldType::Date | FieldType::DateTime) {
        let julian_r = dst_tree.push_fn_call("JULIANDAY", &[r]);
        let diff = dst_tree.push_expr(Expression::BinaryOperator(
            julian_l,
            TranslateBinaryOp::Sub,
            julian_r,
            Parenthesize::No,
        ));
        return (
            Expression::Cast(diff, "INTEGER"),
            FieldType::Numeric { len: 99, dec: 0 },
        );
    }
    let is_integer = r_ty == FieldType::Integer
        || matches!(dst_tree.get_expr_unchecked(r), Expression::NumberLiteral(s) if !s.contains('.'));
    let n = if is_integer {
        r
    } else {
        dst_tree.push_fn_call("ROUND", &[r])
    };
    let sum = dst_tree.push_expr(Expression::BinaryOperator(
        julian_l,
        op,
        n,
        Parenthesize::Yes,
    ));
    (
        Expression::FunctionCall {
            name: "DATE".into(),
            args: dst_tree.push_args([sum].into_iter()),
        },
        FieldType::Date,
    )
}

pub fn translate_expr<'field_lookup, 'parse>(
    source: &'parse parser::Expression,
    src_tree: &'parse ParseTree,
    dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    cx: &'field_lookup impl TranslationContext,
) -> ExpResult<'field_lookup, 'parse> {
    // Chains like `BDAY + 3 + 1` parse as a Sequence rather than nested binary
    //  operators. If the chain starts with a date, fold it left so each step
    //  gets the date arithmetic treatment; otherwise use the default handling.
    if let parser::Expression::Sequence(
        operands,
        op @ (parser::BinaryOp::Add | parser::BinaryOp::Sub),
    ) = source
    {
        let operands = src_tree.get_args(operands);
        let first = src_tree.get_expr_unchecked(operands[0]);
        let (first_expr, first_ty) = cx.translate_expr(first, src_tree, dst_tree)?;
        if first_ty == FieldType::Date {
            let op = match op {
                parser::BinaryOp::Add => TranslateBinaryOp::Add,
                _ => TranslateBinaryOp::Sub,
            };
            let (mut acc, mut acc_ty) = (first_expr, first_ty);
            for operand in &operands[1..] {
                let l = dst_tree.push_expr(acc);
                let (r, r_ty) =
                    cx.translate_expr(src_tree.get_expr_unchecked(*operand), src_tree, dst_tree)?;
                let r = dst_tree.push_expr(r);
                (acc, acc_ty) = if acc_ty == FieldType::Date {
                    date_arith(l, op, r, r_ty, dst_tree)
                } else {
                    // e.g. (date - date) - n is plain numeric arithmetic
                    (
                        Expression::BinaryOperator(l, op, r, Parenthesize::Yes),
                        acc_ty,
                    )
                };
            }
            return ok(acc, acc_ty);
        }
    }
    default_translate(source, src_tree, dst_tree, cx)
}

pub fn translate_fn_call<'parse, 'field_lookup>(
    name: &'parse F,
    args: &'parse [parser::ExpressionId],
    src_tree: &'parse ParseTree<'parse>,
    dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    cx: &'field_lookup impl TranslationContext,
) -> ExpResult<'field_lookup, 'parse> {
    let dst_args = translate_args(args, src_tree, dst_tree, cx)?;
    // Gets the ExpressionId for the argument at `index`
    let argid = |index| {
        dst_args
            .get(index)
            .map(|&(a, _)| a)
            .ok_or(Error::IncorrectArgCount(format!("{name:?}"), index))
    };
    let wrong_type = |index| wrong_type(index, name, args);

    //these are only the ones that are different from Postgres, everything else falls through to postgres
    match name {
        F::CHR => ok(
            Expression::FunctionCall {
                name: "CHAR".into(), // SQLite equivalent
                args: dst_tree.push_args([argid(0)?].into_iter()),
            },
            FieldType::Character(1),
        ),

        // SQLite's DATE() only understands ISO 8601, so the 'MM/DD/YY' input is
        //  rearranged by position. Like Codebase, the parts may be space-padded
        //  (' 8/ 1/26'), and two-digit years 00-68 are 20xx, 69-99 are 19xx.
        //
        //   r = REPLACE(RTRIM(x), ' ', '0')
        //   COALESCE(DATE(CASE WHEN LENGTH(r) = 8 THEN
        //       (CASE WHEN SUBSTR(r,7,2) < '69' THEN '20' ELSE '19' END)
        //       || SUBSTR(r,7,2) || '-' || SUBSTR(r,1,2) || '-' || SUBSTR(r,4,2)
        //     ELSE NULL END), '0001-01-01')
        //
        // Blank or malformed input gives NULL from DATE() and so '0001-01-01'.
        F::CTOD => {
            let rtrim = dst_tree.push_fn_call("RTRIM", &[argid(0)?]);
            let space = exps::lit_space(dst_tree);
            let zero = dst_tree.push_expr("0".into());
            let r = dst_tree.push_fn_call("REPLACE", &[rtrim, space, zero]);

            let lit_1 = exps::lit_1(dst_tree);
            let lit_2 = dst_tree.push_expr(2.into());
            let lit_4 = dst_tree.push_expr(4.into());
            let lit_7 = dst_tree.push_expr(7.into());
            let month = dst_tree.push_fn_call("SUBSTR", &[r, lit_1, lit_2]);
            let day = dst_tree.push_fn_call("SUBSTR", &[r, lit_4, lit_2]);
            let yy = dst_tree.push_fn_call("SUBSTR", &[r, lit_7, lit_2]);

            let pivot = dst_tree.push_expr("69".into());
            let is_2000s = dst_tree.push_expr(Expression::BinaryOperator(
                yy,
                TranslateBinaryOp::Lt,
                pivot,
                Parenthesize::No,
            ));
            let lit_20 = dst_tree.push_expr("20".into());
            let lit_19 = dst_tree.push_expr("19".into());
            let century = dst_tree.push_expr(Expression::Iif {
                cond: is_2000s,
                when_true: lit_20,
                when_false: lit_19,
            });
            let dash = exps::lit_dash(dst_tree);
            let parts = dst_tree.push_args([century, yy, dash, month, dash, day].into_iter());
            let iso = dst_tree.push_expr(Expression::BinaryOperatorSequence(
                TranslateBinaryOp::Concat,
                parts,
            ));

            let len = dst_tree.push_fn_call("LENGTH", &[r]);
            let lit_8 = dst_tree.push_expr(8.into());
            let is_len_8 = dst_tree.push_expr(Expression::BinaryOperator(
                len,
                TranslateBinaryOp::Eq,
                lit_8,
                Parenthesize::No,
            ));
            let null = dst_tree.push_expr(Expression::BareFunctionCall("NULL"));
            let guarded = dst_tree.push_expr(Expression::Iif {
                cond: is_len_8,
                when_true: iso,
                when_false: null,
            });

            let date = dst_tree.push_fn_call("DATE", &[guarded]);
            let coalesce_date = exps::coalesce_date(dst_tree);
            let coalesce = Expression::FunctionCall {
                name: "COALESCE".into(),
                args: dst_tree.push_args([date, coalesce_date].into_iter()),
            };
            ok(coalesce, FieldType::Date)
        }

        // DATE() => DATE('now', 'localtime')
        //  Must be 'YYYY-MM-DD' to compare with date fields.
        F::DATE => {
            let now = dst_tree.push_expr("now".into());
            let localtime = dst_tree.push_expr("localtime".into());
            ok(
                Expression::FunctionCall {
                    name: "DATE".into(),
                    args: dst_tree.push_args([now, localtime].into_iter()),
                },
                FieldType::Date,
            )
        }

        F::DAY => {
            let fmt = dst_tree.push_expr("%d".into());
            let strftime = dst_tree.push_fn_call("STRFTIME", &[fmt, argid(0)?]);
            ok(Expression::Cast(strftime, "REAL"), FieldType::Double)
        }

        // DTOC(x)    => STRFTIME('%m/%d/', x) || SUBSTR(STRFTIME('%Y', x), 3, 2)
        // DTOC(x, 1) => STRFTIME('%Y%m%d', x)
        //  SQLite's STRFTIME has no '%y' (two-digit year), so slice it out of '%Y'.
        F::DTOC => {
            let x = argid(0)?;
            if args.len() == 2 {
                // Equivalent to DTOS
                let fmt = dst_tree.push_expr("%Y%m%d".into());
                return ok(
                    Expression::FunctionCall {
                        name: "STRFTIME".into(),
                        args: dst_tree.push_args([fmt, x].into_iter()),
                    },
                    FieldType::Character(8),
                );
            }
            let md_fmt = dst_tree.push_expr("%m/%d/".into());
            let month_day = dst_tree.push_fn_call("STRFTIME", &[md_fmt, x]);
            let year_fmt = dst_tree.push_expr("%Y".into());
            let year = dst_tree.push_fn_call("STRFTIME", &[year_fmt, x]);
            let lit_2 = dst_tree.push_expr(2.into());
            let lit_3 = dst_tree.push_expr(3.into());
            let yy = dst_tree.push_fn_call("SUBSTR", &[year, lit_3, lit_2]);
            ok(
                Expression::BinaryOperator(
                    month_day,
                    TranslateBinaryOp::Concat,
                    yy,
                    Parenthesize::Yes,
                ),
                FieldType::Character(8),
            )
        }

        F::DTOS => {
            let fmt = dst_tree.push_expr("%Y%m%d".into());
            ok(
                Expression::FunctionCall {
                    name: "STRFTIME".into(),
                    args: dst_tree.push_args([fmt, argid(0)?].into_iter()),
                },
                FieldType::Character(8),
            )
        }

        // SQLite doesn't have LPAD, so transform
        //   PADL(x, n) -> SUBSTR(PRINTF('%<n>s', x), -n)
        F::PADL => {
            let lit_n: u32 = match dst_tree.get_expr_unchecked(argid(1)?) {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(1)),
                _ => Err(wrong_type(1)),
            }?;
            let fmt = dst_tree.push_expr(format!("%{lit_n}s").into());
            let printf = dst_tree.push_fn_call("PRINTF", &[fmt, argid(0)?]);
            let n = argid(1)?;
            let negative_n = dst_tree.push_expr(Expression::UnaryOperator(super::UnaryOp::Neg, n));
            ok(
                Expression::FunctionCall {
                    name: "SUBSTR".into(),
                    args: dst_tree.push_args([printf, negative_n].into_iter()),
                },
                FieldType::Character(lit_n),
            )
        }

        F::MONTH => {
            let fmt = dst_tree.push_expr("%m".into());
            let strftime = dst_tree.push_fn_call("STRFTIME", &[fmt, argid(0)?]);
            ok(Expression::Cast(strftime, "REAL"), FieldType::Double)
        }

        F::RIGHT => {
            let n: u32 = match dst_tree.get_expr_unchecked(argid(1)?) {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(1)),
                _ => Err(wrong_type(1)),
            }?;
            let out_ty = match dst_args[0].1 {
                FieldType::Character(len) => FieldType::Character(len - n),
                _ => FieldType::Memo,
            };
            let n = -i64::from(n);
            let n = dst_tree.push_expr(n.into());
            ok(
                Expression::FunctionCall {
                    name: "SUBSTR".into(),
                    args: dst_tree.push_args([argid(0)?, n].into_iter()),
                },
                out_ty,
            )
        }
        F::STOD => {
            // Convert YYYYMMDD -> 'YYYY-MM-DD'
            // COALESCE(
            //   SUBSTR(TRIM(x),1,4)   // extract year
            //   || '-' ||
            //   SUBSTR(TRIM(x),5,2)   // extract month
            //   || '-' ||
            //   SUBSTR(TRIM(x),7,2)
            // ,'0001-01-01')
            let trim = dst_tree.push_fn_call("TRIM", &[argid(0)?]);
            let lit_2 = dst_tree.push_expr(2.into());
            let lit_4 = dst_tree.push_expr(4.into());
            let lit_5 = dst_tree.push_expr(5.into());
            let lit_7 = dst_tree.push_expr(7.into());
            let lit_1 = exps::lit_1(dst_tree);
            let extract_year = dst_tree.push_fn_call("SUBSTR", &[trim, lit_1, lit_4]);
            let extract_month = dst_tree.push_fn_call("SUBSTR", &[trim, lit_5, lit_2]);
            let extract_day = dst_tree.push_fn_call("SUBSTR", &[trim, lit_7, lit_2]);
            let lit_dash = exps::lit_dash(dst_tree);
            let concat_parts = dst_tree.push_args(
                [extract_year, lit_dash, extract_month, lit_dash, extract_day].into_iter(),
            );
            // concatenate the bits together with dashes
            let date_str = dst_tree.push_expr(Expression::BinaryOperatorSequence(
                TranslateBinaryOp::Concat,
                concat_parts,
            ));
            let coalesce_date = exps::coalesce_date(dst_tree);
            let coalesce = Expression::FunctionCall {
                name: "COALESCE".into(),
                args: dst_tree.push_args([date_str, coalesce_date].into_iter()),
            };
            ok(coalesce, FieldType::Date)
        }

        // PRINTF('%{n}.{d}', x)
        F::STR => {
            let (val_arg, len, dec) = postgres::get_str_fn_args(args, src_tree, dst_tree, cx)?;
            let fmt = dst_tree.push_expr(format!("%{len}.{dec}f").into()); // e.g. "%.2f"
            let expression = dst_tree.push_fn_call("PRINTF", &[fmt, val_arg]);
            //if the length of the evaluated expression is greater than the specified len, fill the len with asterisks instead of showing any value at all
            let len_expr = dst_tree.push_fn_call("LENGTH", &[expression]);
            let rhs = dst_tree.push_expr((len as i64).into());
            let cond = dst_tree.push_expr(Expression::BinaryOperator(
                len_expr,
                super::BinaryOp::Le,
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
        // TIME() -> time('now', 'localtime')
        F::TIME => {
            let now = dst_tree.push_expr("now".into());
            let localtime = dst_tree.push_expr("localtime".into());
            ok(
                Expression::FunctionCall {
                    name: "time".into(),
                    args: dst_tree.push_args([now, localtime].into_iter()),
                },
                FieldType::Character(8),
            )
        }
        F::VAL => ok(
            Expression::Cast(argid(0)?, "REAL"),
            FieldType::Numeric { len: 0, dec: 0 },
        ),

        F::YEAR => {
            let fmt = dst_tree.push_expr("%Y".into());
            let strftime = dst_tree.push_fn_call("STRFTIME", &[fmt, argid(0)?]);
            ok(Expression::Cast(strftime, "REAL"), FieldType::Double)
        }

        other => postgres::translate_fn_call(other, args, src_tree, dst_tree, cx),
    }
}

pub fn translate_binary_op<'field_lookup, 'parse>(
    l: &'parse parser::Expression,
    op: &'parse parser::BinaryOp,
    r: &'parse parser::Expression,
    src_tree: &'parse ParseTree,
    dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    cx: &'field_lookup impl TranslationContext,
) -> ExpResult<'field_lookup, 'parse> {
    let (translated_l, ty) = default_translate(l, src_tree, dst_tree, cx)?;
    match (op, ty) {
        (
            op @ (parser::BinaryOp::Eq | parser::BinaryOp::Ne),
            ty @ (FieldType::Memo | FieldType::Character(_)),
        ) => {
            let translated_r = default_translate(r, src_tree, dst_tree, cx)?.0;
            let modified_r = expr_between_right_side(
                match ty {
                    FieldType::Memo => dst_tree.push_expr(translated_r),
                    FieldType::Character(len) => {
                        let translated_r = dst_tree.push_expr(translated_r);
                        cx.string_comp_right(translated_r, len, dst_tree)
                    }
                    _ => unreachable!(),
                },
                dst_tree,
            );
            let binop = match op {
                parser::BinaryOp::Eq => TranslateBinaryOp::Between,
                parser::BinaryOp::Ne => TranslateBinaryOp::NotBetween,
                _ => unreachable!(),
            };

            ok(
                Expression::BinaryOperator(
                    dst_tree.push_expr(translated_l),
                    binop,
                    dst_tree.push_expr(modified_r),
                    Parenthesize::Yes,
                ),
                FieldType::Logical,
            )
        }
        (op @ (parser::BinaryOp::Add | parser::BinaryOp::Sub), FieldType::Date) => {
            let l = dst_tree.push_expr(translated_l);
            let (r, r_ty) = cx.translate_expr(r, src_tree, dst_tree)?;
            let r = dst_tree.push_expr(r);
            let op = match op {
                parser::BinaryOp::Add => TranslateBinaryOp::Add,
                _ => TranslateBinaryOp::Sub,
            };
            let (expr, ty) = date_arith(l, op, r, r_ty, dst_tree);
            ok(expr, ty)
        }
        (parser::BinaryOp::Contain, FieldType::Character(_)) => {
            let translated_r = default_translate(r, src_tree, dst_tree, cx)?.0;
            let haystack = dst_tree.push_expr(translated_r);
            let needle = dst_tree.push_expr(translated_l);
            let instr = dst_tree.push_fn_call(
                "INSTR",
                // Note that in CodeBase the haystack is the right arg
                &[haystack, needle],
            );
            let lit_0 = exps::lit_0(dst_tree);
            ok(
                Expression::BinaryOperator(instr, super::BinaryOp::Gt, lit_0, Parenthesize::Yes),
                FieldType::Logical,
            )
        }
        // Sub on a character type also maps to CONCAT but with the
        //  trailing spaces of the first argument "moved" to the end
        //  of the result. We can map this as:
        //
        // format('%s%s%.*c', RTRIM(l), r, LENGTH(l) - LENGTH( RTRIM(l)), ' ')
        //
        (parser::BinaryOp::Sub, FieldType::Character(_) | FieldType::Memo) => {
            let translated_l = dst_tree.push_expr(translated_l);
            let translated_r = cx.translate_expr(r, src_tree, dst_tree)?.0;
            let translated_r = dst_tree.push_expr(translated_r);
            let without_spaces = dst_tree.push_fn_call("RTRIM", &[translated_l]);
            let length_without_spaces = dst_tree.push_fn_call("LENGTH", &[without_spaces]);
            let length_with_spaces = dst_tree.push_fn_call("LENGTH", &[translated_l]);
            let num_spaces = dst_tree.push_expr(Expression::BinaryOperator(
                length_with_spaces,
                super::BinaryOp::Sub,
                length_without_spaces,
                Parenthesize::No,
            ));
            let fmt =
                dst_tree.push_expr(Expression::SingleQuoteStringLiteral(Cow::from("%s%s%.*c")));
            let lit_space = exps::lit_space(dst_tree);
            ok(
                Expression::FunctionCall {
                    name: "format".into(),
                    args: dst_tree.push_args(
                        [fmt, without_spaces, translated_r, num_spaces, lit_space].into_iter(),
                    ),
                },
                FieldType::Memo,
            )
        }
        _ => translate_binary_op_right(cx, l, translated_l, ty, op, r, src_tree, dst_tree),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sqlite_contains_to_instr() {
        let translator = SqliteTranslator {
            field_lookup: |_alias, _name| Ok((Cow::from("SCREEN"), FieldType::Character(32))),
        };
        let input = "'Wizard '$(SCREEN)";
        let pt = crate::parse(input).expect("parses");
        let (res, FieldType::Logical) = translator.translate(&pt).expect("translates") else {
            panic!("Expected a Logical");
        };

        use crate::to_sql::{Printer, PrinterConfig, SqlitePrinterContext};
        let p = Printer::new(
            res,
            PrinterConfig {
                context: Box::new(SqlitePrinterContext { pad_strings: false }),
            },
        );

        let sql = format!("{p}");
        assert_eq!(r#"(INSTR("SCREEN",'Wizard ')>0)"#, sql);
    }

    #[test]
    fn sqlite_trim_query_len() {
        let translator = SqliteTranslator {
            field_lookup: |_alias, _name| Ok((Cow::from("SCREEN"), FieldType::Character(5))),
        };
        let input = "SCREEN = 'XYZ  X'";
        let pt = crate::parse(input).expect("parses");
        let (res, FieldType::Logical) = translator.translate(&pt).expect("translates") else {
            panic!("Expected a Logical");
        };

        use crate::to_sql::{Printer, PrinterConfig, SqlitePrinterContext};
        let p = Printer::new(
            res,
            PrinterConfig {
                context: Box::new(SqlitePrinterContext { pad_strings: false }),
            },
        );

        //trimming the right side of the query to the length of the field (removes the 'X' in this case)
        let sql = format!("{p}");
        assert_eq!(
            r#"("SCREEN" BETWEEN SUBSTR('XYZ  X',1,5) AND SUBSTR('XYZ  X',1,5) ||  char(0xFFFF) )"#,
            sql
        );
    }

    #[test]
    fn sqlite_sub_concat() {
        let translator = SqliteTranslator {
            field_lookup: |_alias, _name| Ok((Cow::from("SCREEN"), FieldType::Character(32))),
        };
        let input = "'ab  '-'cd'";
        let pt = crate::parse(input).expect("parses");
        let (res, _) = translator.translate(&pt).expect("translates");

        use crate::to_sql::{Printer, PrinterConfig, SqlitePrinterContext};
        let p = Printer::new(
            res,
            PrinterConfig {
                context: Box::new(SqlitePrinterContext { pad_strings: false }),
            },
        );

        let sql = format!("{p}");
        // format('%s%s%.*c', RTRIM(l), r, LENGTH(l) - LENGTH( RTRIM(l)), ' ')
        assert_eq!(
            r#"format('%s%s%.*c',RTRIM('ab  '),'cd',LENGTH('ab  ')-LENGTH(RTRIM('ab  ')),' ')"#,
            sql
        );
    }

    /// Translates `input` for SQLite with every field being a Date named BDAY
    fn sqlite_date_sql(input: &str) -> (String, FieldType) {
        let translator = SqliteTranslator {
            field_lookup: |_alias, _name| Ok((Cow::from("BDAY"), FieldType::Date)),
        };
        let pt = crate::parse(input).expect("parses");
        let (res, ty) = translator.translate(&pt).expect("translates");

        use crate::to_sql::{Printer, PrinterConfig, SqlitePrinterContext};
        let p = Printer::new(
            res,
            PrinterConfig {
                context: Box::new(SqlitePrinterContext { pad_strings: false }),
            },
        );
        (format!("{p}"), ty)
    }

    const BDAY: &str = r#"COALESCE("BDAY", DATE('0001-01-01'))"#;

    #[test]
    fn sqlite_dtoc_two_digit_year() {
        let (sql, ty) = sqlite_date_sql("DTOC(BDAY)");
        assert_eq!(
            format!("(STRFTIME('%m/%d/',{BDAY}) || SUBSTR(STRFTIME('%Y',{BDAY}),3,2))"),
            sql
        );
        assert_eq!(FieldType::Character(8), ty);

        let (sql, _) = sqlite_date_sql("DTOC(BDAY, 1)");
        assert_eq!(format!("STRFTIME('%Y%m%d',{BDAY})"), sql);
    }

    #[test]
    fn sqlite_date_is_iso() {
        let (sql, _) = sqlite_date_sql("BDAY = DATE()");
        assert_eq!(format!("({BDAY}=DATE('now','localtime'))"), sql);
    }

    #[test]
    fn sqlite_ctod() {
        let (sql, ty) = sqlite_date_sql("CTOD('08/10/26')");
        let r = "REPLACE(RTRIM('08/10/26'),' ','0')";
        assert_eq!(
            format!(
                "COALESCE(DATE((CASE WHEN LENGTH({r})=8 THEN \
                 ((CASE WHEN SUBSTR({r},7,2)<'69' THEN '20' ELSE '19' END) || SUBSTR({r},7,2) || '-' || SUBSTR({r},1,2) || '-' || SUBSTR({r},4,2)) \
                 ELSE  NULL  END)),'0001-01-01')"
            ),
            sql
        );
        assert_eq!(FieldType::Date, ty);
    }

    #[test]
    fn sqlite_date_plus_minus_days() {
        let (sql, ty) = sqlite_date_sql("BDAY + 3");
        assert_eq!(format!("DATE((JULIANDAY({BDAY})+3))"), sql);
        assert_eq!(FieldType::Date, ty);

        let (sql, ty) = sqlite_date_sql("BDAY - 3");
        assert_eq!(format!("DATE((JULIANDAY({BDAY})-3))"), sql);
        assert_eq!(FieldType::Date, ty);

        // Non-integer day counts are rounded, as in the Postgres translation
        let (sql, _) = sqlite_date_sql("BDAY + 1.6");
        assert_eq!(format!("DATE((JULIANDAY({BDAY})+ROUND(1.6)))"), sql);
    }

    #[test]
    fn sqlite_date_minus_date() {
        let (sql, ty) = sqlite_date_sql("BDAY - BDAY");
        assert_eq!(
            format!("CAST (JULIANDAY({BDAY})-JULIANDAY({BDAY}) AS INTEGER)"),
            sql
        );
        assert_eq!(FieldType::Numeric { len: 99, dec: 0 }, ty);
    }

    #[test]
    fn sqlite_date_sequence() {
        let (sql, ty) = sqlite_date_sql("BDAY + 3 + 1");
        assert_eq!(
            format!("DATE((JULIANDAY(DATE((JULIANDAY({BDAY})+3)))+1))"),
            sql
        );
        assert_eq!(FieldType::Date, ty);

        // date - date yields a number; the remaining steps are plain arithmetic
        let (sql, ty) = sqlite_date_sql("BDAY - BDAY - 1");
        assert_eq!(
            format!("(CAST (JULIANDAY({BDAY})-JULIANDAY({BDAY}) AS INTEGER)-1)"),
            sql
        );
        assert_eq!(FieldType::Numeric { len: 99, dec: 0 }, ty);
    }

    #[test]
    fn sqlite_stod() {
        let translator = SqliteTranslator {
            field_lookup: |_alias, _name| panic!("This shouldn't be used"),
        };
        let input = "STOD('20260810')";
        let pt = crate::parse(input).expect("parses");
        let (res, _) = translator.translate(&pt).expect("translates");

        use crate::to_sql::{Printer, PrinterConfig, SqlitePrinterContext};
        let p = Printer::new(
            res,
            PrinterConfig {
                context: Box::new(SqlitePrinterContext { pad_strings: false }),
            },
        );

        let sql = format!("{p}");
        assert_eq!(
            r#"COALESCE((SUBSTR(TRIM('20260810'),1,4) || '-' || SUBSTR(TRIM('20260810'),5,2) || '-' || SUBSTR(TRIM('20260810'),7,2)),'0001-01-01')"#,
            sql
        );
    }
}

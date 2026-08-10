use std::borrow::Cow;

use crate::{
    codebase_functions::CodebaseFunction as F,
    parser::{self, ExpressionId, ParseTree},
    translate::{
        BinaryOp as TranslateBinaryOp, Error, ExpResult, Expression, FieldType, Parenthesize,
        SQLTree, TranslationContext, exps, ok,
        postgres::{
            self, translate_binary_op_right, translate_expr as default_translate, wrong_type,
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

pub fn translate_expr<'field_lookup, 'parse>(
    source: &'parse parser::Expression,
    src_tree: &'parse ParseTree,
    dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    cx: &'field_lookup impl TranslationContext,
) -> ExpResult<'field_lookup, 'parse> {
    default_translate(source, src_tree, dst_tree, cx)
}

pub fn translate_fn_call<'parse, 'field_lookup>(
    name: &'parse F,
    args: &'parse [parser::ExpressionId],
    src_tree: &'parse ParseTree<'parse>,
    dst_tree: &mut SQLTree<'field_lookup, 'parse>,
    cx: &'field_lookup impl TranslationContext,
) -> ExpResult<'field_lookup, 'parse> {
    // Lazily translates the specified argument, stores it in the tree, and
    //  returns the ExpressionId
    let mut arg = |index| {
        let arg = args
            .get(index)
            .ok_or_else(|| Error::IncorrectArgCount(format!("{name:?}"), index))?;
        let arg = src_tree.get_expr_unchecked(*arg);
        let (exp, ft) = translate_expr(arg, src_tree, dst_tree, cx)?;
        let id = dst_tree.push_expr(exp);
        Ok((id, ft))
    };
    let mut argid = |index| arg(index).map(|(id, _ft)| id);
    let wrong_type = |index| wrong_type(index, name, args);

    //these are only the ones that are different from Postgres, everything else falls through to postgres
    match name {
        F::CHR => {
            let x = argid(0)?;
            ok(
                Expression::FunctionCall {
                    name: "CHAR".into(), // SQLite equivalent
                    args: dst_tree.push_args([x].into_iter()),
                },
                FieldType::Character(1),
            )
        }

        //COALESCE(DATE(NULLIF(TRIM(x),''),'0001-01-01')
        F::CTOD => {
            let x = argid(0)?;
            let trim = dst_tree.push_fn_call("TRIM", &[x]);
            let null_if = dst_tree.push_fn_call("NULLIF", &[trim, exps::EMPTY_STR]);
            // Convert format -> 'YYYY-MM-DD' using SUBSTR
            let printf = dst_tree.push_fn_call(
                "printf",
                &[null_if], // assumes date in ISO 8601 or needs pre-processing
            );
            let date = dst_tree.push_fn_call("DATE", &[printf]);
            let coalesce = Expression::FunctionCall {
                name: "COALESCE".into(),
                args: dst_tree.push_args([date, exps::COALESCE_DATE].into_iter()),
            };
            ok(coalesce, FieldType::Date)
        }

        // DATE() => STRFTIME('%Y%m%d', 'now', 'localtime')
        F::DATE => {
            let fmt = dst_tree.push_expr("%Y%m%d".into());
            let now = dst_tree.push_expr("now".into());
            let localtime = dst_tree.push_expr("localtime".into());
            ok(
                Expression::FunctionCall {
                    name: "strftime".into(),
                    args: dst_tree.push_args([fmt, now, localtime].into_iter()),
                },
                FieldType::Date,
            )
        }

        F::DAY => {
            let x = argid(0)?;
            let fmt = dst_tree.push_expr("%d".into());
            let strftime = dst_tree.push_fn_call("STRFTIME", &[fmt, x]);
            ok(Expression::Cast(strftime, "REAL"), FieldType::Double)
        }

        F::DTOC => {
            let x = argid(0)?;
            let fmt = if args.len() == 2 {
                // Equivalent to DTOS
                "%Y%m%d"
            } else {
                "%m/%d/%y"
            };
            let fmt = dst_tree.push_expr(fmt.into());

            ok(
                Expression::FunctionCall {
                    name: "STRFTIME".into(),
                    args: dst_tree.push_args([fmt, x].into_iter()),
                },
                FieldType::Character(8),
            )
        }

        F::DTOS => {
            let x = argid(0)?;
            let fmt = dst_tree.push_expr("%Y%m%d".into());
            ok(
                Expression::FunctionCall {
                    name: "STRFTIME".into(),
                    args: dst_tree.push_args([fmt, x].into_iter()),
                },
                FieldType::Character(8),
            )
        }

        // SQLite doesn't have LPAD, so transform
        //   PADL(x, n) -> SUBSTR(PRINTF('%<n>s', x), -n)
        F::PADL => {
            let x = argid(0)?;
            let n = argid(1)?;
            let lit_n: u32 = match dst_tree.get_expr_unchecked(n) {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(1)),
                _ => Err(wrong_type(1)),
            }?;
            let fmt = dst_tree.push_expr(format!("%{lit_n}s").into());
            let printf = dst_tree.push_fn_call("PRINTF", &[fmt, x]);
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
            let x = argid(0)?;
            let fmt = dst_tree.push_expr("%m".into());
            let strftime = dst_tree.push_fn_call("STRFTIME", &[fmt, x]);
            ok(Expression::Cast(strftime, "REAL"), FieldType::Double)
        }

        F::RIGHT => {
            let (x, x_ty) = arg(0)?;
            let (n, _) = arg(1)?;
            let n: u32 = match dst_tree.get_expr_unchecked(n) {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(1)),
                _ => Err(wrong_type(1)),
            }?;
            let out_ty = match x_ty {
                FieldType::Character(len) => FieldType::Character(len - n),
                _ => FieldType::Memo,
            };
            let n = -i64::from(n);
            let n = dst_tree.push_expr(n.into());
            ok(
                Expression::FunctionCall {
                    name: "SUBSTR".into(),
                    args: dst_tree.push_args([x, n].into_iter()),
                },
                out_ty,
            )
        }
        F::STOD => {
            //               | extract_year    | | extract_month   | | extract_day     |
            // COALESCE(DATE(SUBSTR(TRIM(x),1,4),SUBSTR(TRIM(x),5,2),SUBSTR(TRIM(x),7,2)),'0001-01-01')
            let x = argid(0)?;
            let trim = dst_tree.push_fn_call("TRIM", &[x]);
            // Convert format -> 'YYYY-MM-DD' using SUBSTR
            //TODO this is actually converting to YYYYMMDD!
            let lit_2 = dst_tree.push_expr(2.into());
            let lit_4 = dst_tree.push_expr(4.into());
            let lit_5 = dst_tree.push_expr(5.into());
            let lit_7 = dst_tree.push_expr(7.into());
            let extract_year = dst_tree.push_fn_call("SUBSTR", &[trim, exps::LIT_1, lit_4]);
            let extract_month = dst_tree.push_fn_call("SUBSTR", &[trim, lit_5, lit_2]);
            let extract_day = dst_tree.push_fn_call("SUBSTR", &[trim, lit_7, lit_2]);
            let date = dst_tree.push_fn_call("DATE", &[extract_year, extract_month, extract_day]);
            let coalesce = Expression::FunctionCall {
                name: "COALESCE".into(),
                args: dst_tree.push_args([date, exps::COALESCE_DATE].into_iter()),
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
            let x = argid(0)?;
            let fmt = dst_tree.push_expr("%Y".into());
            let strftime = dst_tree.push_fn_call("STRFTIME", &[fmt, x]);
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
        (parser::BinaryOp::Contain, FieldType::Character(_)) => {
            let translated_r = default_translate(r, src_tree, dst_tree, cx)?.0;
            let haystack = dst_tree.push_expr(translated_r);
            let needle = dst_tree.push_expr(translated_l);
            let instr = dst_tree.push_fn_call(
                "INSTR",
                // Note that in CodeBase the haystack is the right arg
                &[haystack, needle],
            );
            ok(
                Expression::BinaryOperator(
                    instr,
                    super::BinaryOp::Gt,
                    exps::LIT_0,
                    Parenthesize::Yes,
                ),
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
            ok(
                Expression::FunctionCall {
                    name: "format".into(),
                    args: dst_tree.push_args(
                        [
                            fmt,
                            without_spaces,
                            translated_r,
                            num_spaces,
                            exps::LIT_SPACE,
                        ]
                        .into_iter(),
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
}

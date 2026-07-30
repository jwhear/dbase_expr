use crate::{
    codebase_functions::CodebaseFunction as F,
    parser::{self, ParseTree},
    translate::{
        BinaryOp as TranslateBinaryOp, Error, ExpResult, Expression, FieldType, Parenthesize,
        SQLTree, TranslationContext, exps, ok,
        postgres::{
            self, translate_args, translate_binary_op_right, translate_expr as default_translate,
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

    fn translate_expr(
        &self,
        source: &parser::Expression,
        src_tree: &ParseTree,
        dst_tree: &mut SQLTree,
    ) -> ExpResult {
        default_translate(source, src_tree, dst_tree, self)
    }

    fn translate_fn_call(
        &self,
        name: &crate::codebase_functions::CodebaseFunction,
        args: &[parser::ExpressionId],
        src_tree: &ParseTree,
        dst_tree: &mut SQLTree,
    ) -> ExpResult {
        translate_fn_call(name, args, src_tree, dst_tree, self)
    }

    fn translate_binary_op(
        &self,
        l: &parser::Expression,
        op: &parser::BinaryOp,
        r: &parser::Expression,
        src_tree: &ParseTree,
        dst_tree: &mut SQLTree,
    ) -> ExpResult {
        let (translated_l, ty) = self.translate_expr(l, src_tree, dst_tree)?;
        match (op, ty) {
            (
                op @ (parser::BinaryOp::Eq | parser::BinaryOp::Ne),
                ty @ (FieldType::Memo | FieldType::Character(_)),
            ) => {
                let translated_r = self.translate_expr(r, src_tree, dst_tree)?.0;
                let modified_r = expr_between_right_side(
                    match ty {
                        FieldType::Memo => translated_r,
                        FieldType::Character(len) => {
                            let translated_r = dst_tree.push_expr(translated_r);
                            self.string_comp_right(translated_r, len, dst_tree)
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
                let translated_r = self.translate_expr(r, src_tree, dst_tree)?.0;
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
                let translated_r = self.translate_expr(r, src_tree, dst_tree)?.0;
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
                let fmt = dst_tree.push_expr(Expression::SingleQuoteStringLiteral(String::from(
                    "%s%s%.*c",
                )));
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
            _ => translate_binary_op_right(self, l, translated_l, ty, op, r, src_tree, dst_tree),
        }
    }
}

fn expr_between_right_side(expression: Expression, dst_tree: &mut SQLTree) -> Expression {
    let expression = dst_tree.push_expr(expression);
    let char = dst_tree.push_expr(Expression::BareFunctionCall("char(0xFFFF)".to_string()));
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

pub fn translate_fn_call<'a>(
    name: &'a F,
    args: &'a [parser::ExpressionId],
    src_tree: &'a ParseTree<'a>,
    dst_tree: &mut SQLTree,
    cx: &'a impl TranslationContext,
) -> ExpResult {
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
                name: "CHAR", // SQLite equivalent
                args: dst_tree.push_args([argid(0)?].into_iter()),
            },
            FieldType::Character(1),
        ),

        F::CTOD => {
            //COALESCE(DATE(NULLIF(TRIM(x),''),'0001-01-01')
            let trim = dst_tree.push_fn_call("TRIM", &[argid(0)?]);
            let null_if = dst_tree.push_fn_call("NULLIF", &[trim, exps::EMPTY_STR]);
            // Convert format -> 'YYYY-MM-DD' using SUBSTR
            let printf = dst_tree.push_fn_call(
                "printf",
                &[null_if], // assumes date in ISO 8601 or needs pre-processing
            );
            let date = dst_tree.push_fn_call("DATE", &[printf]);
            let coalesce = Expression::FunctionCall {
                name: "COALESCE",
                args: dst_tree.push_args([date, exps::COALESCE_DATE].into_iter()),
            };
            ok(coalesce, FieldType::Date)
        }

        F::DAY => {
            let fmt = dst_tree.push_expr("'%d'".into());
            let strftime = dst_tree.push_fn_call("STRFTIME", &[fmt, argid(0)?]);
            ok(
                Expression::FunctionCall {
                    name: "CAST",
                    args: dst_tree.push_args([strftime].into_iter()),
                },
                FieldType::Double,
            )
        }

        F::DTOC => {
            let fmt = if args.len() == 2 {
                // Equivalent to DTOS
                "%Y%m%d"
            } else {
                "%m/%d/%y"
            };
            let fmt = dst_tree.push_expr(fmt.into());

            ok(
                Expression::FunctionCall {
                    name: "STRFTIME",
                    args: dst_tree.push_args([fmt, argid(0)?].into_iter()),
                },
                FieldType::Character(8),
            )
        }

        F::DTOS => {
            let fmt = dst_tree.push_expr("%Y%m%d".into());
            ok(
                Expression::FunctionCall {
                    name: "STRFTIME",
                    args: dst_tree.push_args([fmt, argid(0)?].into_iter()),
                },
                FieldType::Character(8),
            )
        }

        F::MONTH => {
            let fmt = dst_tree.push_expr("%m".into());
            let strftime = dst_tree.push_fn_call("STRFTIME", &[fmt, argid(0)?]);
            ok(
                Expression::FunctionCall {
                    name: "CAST",
                    args: dst_tree.push_args([strftime].into_iter()),
                },
                FieldType::Double,
            )
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
                    name: "SUBSTR",
                    args: dst_tree.push_args([argid(0)?, n].into_iter()),
                },
                out_ty,
            )
        }
        F::STOD => {
            //               | extract_year    | | extract_month   | | extract_day     |
            // COALESCE(DATE(SUBSTR(TRIM(x),1,4),SUBSTR(TRIM(x),5,2),SUBSTR(TRIM(x),7,2)),'0001-01-01')
            let trim = dst_tree.push_fn_call("TRIM", &[argid(0)?]);
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
                name: "COALESCE",
                args: dst_tree.push_args([date, exps::COALESCE_DATE].into_iter()),
            };
            ok(coalesce, FieldType::Date)
        }
        F::STR => {
            let len: i64 = match dst_tree.get_expr_unchecked(argid(1)?) {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(1)),
                _ => Err(wrong_type(1)),
            }?;
            let dec: i64 = match dst_tree.get_expr_unchecked(argid(2)?) {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(2)),
                _ => Err(wrong_type(2)),
            }?;
            let fmt = dst_tree.push_expr(format!("%{}.{}f", len, dec).into()); // e.g. "%.2f"
            ok(
                Expression::FunctionCall {
                    name: "PRINTF",
                    args: dst_tree.push_args([fmt, argid(0)?].into_iter()),
                },
                FieldType::Character(len as u32),
            )
        }
        F::VAL => ok(
            Expression::Cast(argid(0)?, "REAL"),
            FieldType::Numeric { len: 0, dec: 0 },
        ),

        F::YEAR => {
            let fmt = dst_tree.push_expr("%Y".into());
            let strftime = dst_tree.push_fn_call("STRFTIME", &[fmt, argid(0)?]);
            ok(
                Expression::FunctionCall {
                    name: "CAST",
                    args: dst_tree.push_args([strftime].into_iter()),
                },
                FieldType::Double,
            )
        }

        other => postgres::translate_fn_call(other, args, src_tree, dst_tree, cx),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sqlite_contains_to_instr() {
        let translator = SqliteTranslator {
            field_lookup: |_alias, _name| Ok((String::from("SCREEN"), FieldType::Character(32))),
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
            field_lookup: |_alias, _name| Ok((String::from("SCREEN"), FieldType::Character(5))),
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
            field_lookup: |_alias, _name| Ok((String::from("SCREEN"), FieldType::Character(32))),
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

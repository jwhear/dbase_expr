use super::{
    BinaryOp, Error, Expression, FieldType, Result, TranslationContext, UnaryOp,
    escape_single_quotes, ok, string_comp_left, string_comp_right,
};
use crate::{
    ast::{self, Expression as E},
    codebase_functions::CodebaseFunction as F,
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

    fn translate(&self, source: &ast::Expression) -> Result {
        translate(source, self)
    }

    fn translate_fn_call(
        &self,
        name: &crate::codebase_functions::CodebaseFunction,
        args: &[Box<ast::Expression>],
    ) -> std::result::Result<(Box<Expression>, FieldType), Error> {
        translate_fn_call(name, args, self)
    }
}

/// Translates dBase expression to a SQL expression.
pub fn translate<C: TranslationContext>(source: &E, cx: &C) -> Result {
    // helper for creating binary operators
    let binop = |l, op, r, ty| ok(Expression::BinaryOperator(l, op, translate(r, cx)?.0), ty);

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
        E::SingleQuoteStringLiteral(v) => {
            let v = escape_single_quotes(v);
            let len = v.len();
            ok(
                Expression::SingleQuoteStringLiteral(v),
                FieldType::Character(len as u32),
            )
        }
        E::DoubleQuoteStringLiteral(v) => {
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
            ok(
                Expression::Field {
                    alias: alias.clone(),
                    name,
                    field_type,
                },
                field_type,
            )
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
            let (l, ty) = translate(l, cx)?;
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
                    FieldType::Double
                    | FieldType::Float
                    | FieldType::Integer
                    | FieldType::Numeric { .. },
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
                    let without_spaces = Box::new(Expression::FunctionCall {
                        name: "RTRIM".into(),
                        args: vec![l.clone()],
                    });
                    let length_without_spaces = Box::new(Expression::FunctionCall {
                        name: "LENGTH".into(),
                        args: vec![without_spaces.clone()],
                    });
                    let length_with_spaces = Box::new(Expression::FunctionCall {
                        name: "LENGTH".into(),
                        args: vec![l],
                    });
                    let num_spaces = Box::new(Expression::BinaryOperator(
                        length_with_spaces,
                        BinaryOp::Sub,
                        length_without_spaces,
                    ));
                    let repeated_spaces = Box::new(Expression::FunctionCall {
                        name: "REPEAT".into(),
                        args: vec!["' '".into(), num_spaces],
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
                    FieldType::Double
                    | FieldType::Float
                    | FieldType::Integer
                    | FieldType::Numeric { .. },
                ) => binop(l, BinaryOp::Mul, r, ty),
                (
                    ast::BinaryOp::Div,
                    FieldType::Double
                    | FieldType::Float
                    | FieldType::Integer
                    | FieldType::Numeric { .. },
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
                (ast::BinaryOp::And, FieldType::Logical) => {
                    binop(l, BinaryOp::And, r, FieldType::Logical)
                }
                (ast::BinaryOp::Or, FieldType::Logical) => {
                    binop(l, BinaryOp::Or, r, FieldType::Logical)
                }
                (ast::BinaryOp::Lt, FieldType::Character(len)) => {
                    let left = string_comp_left(l, translate(r, cx)?.0);
                    let right = string_comp_right(r.clone(), len);
                    binop(left, BinaryOp::Lt, &right, FieldType::Logical)
                }
                (ast::BinaryOp::Le, FieldType::Character(len)) => {
                    let left = string_comp_left(l, translate(r, cx)?.0);
                    let right = string_comp_right(r.clone(), len);
                    binop(left, BinaryOp::Le, &right, FieldType::Logical)
                }
                (ast::BinaryOp::Gt, FieldType::Character(len)) => {
                    let left = string_comp_left(l, translate(r, cx)?.0);
                    let right = string_comp_right(r.clone(), len);
                    binop(left, BinaryOp::Gt, &right, FieldType::Logical)
                }
                (ast::BinaryOp::Ge, FieldType::Character(len)) => {
                    let left = string_comp_left(l, translate(r, cx)?.0);
                    let right = string_comp_right(r.clone(), len);
                    binop(left, BinaryOp::Ge, &right, FieldType::Logical)
                }
                (ast::BinaryOp::Lt, FieldType::Memo) => {
                    let left = string_comp_left(l, translate(r, cx)?.0);
                    binop(left, BinaryOp::Lt, r, FieldType::Logical)
                }
                (ast::BinaryOp::Le, FieldType::Memo) => {
                    let left = string_comp_left(l, translate(r, cx)?.0);
                    binop(left, BinaryOp::Le, r, FieldType::Logical)
                }
                (ast::BinaryOp::Gt, FieldType::Memo) => {
                    let left = string_comp_left(l, translate(r, cx)?.0);
                    binop(left, BinaryOp::Gt, r, FieldType::Logical)
                }
                (ast::BinaryOp::Ge, FieldType::Memo) => {
                    let left = string_comp_left(l, translate(r, cx)?.0);
                    binop(left, BinaryOp::Ge, r, FieldType::Logical)
                }
                (ast::BinaryOp::Eq, FieldType::Character(_) | FieldType::Memo) => {
                    binop(l, BinaryOp::StartsWith, r, FieldType::Logical)
                }
                (ast::BinaryOp::Ne, FieldType::Character(_) | FieldType::Memo) => {
                    let starts_with =
                        Expression::BinaryOperator(l, BinaryOp::StartsWith, translate(r, cx)?.0);
                    ok(
                        Expression::UnaryOperator(UnaryOp::Not, Box::new(starts_with)),
                        FieldType::Logical,
                    )
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
                    let strpos = Box::new(Expression::FunctionCall {
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
        E::FunctionCall { name, args } => cx.translate_fn_call(name, args),
    }
}

// This function does the kind of gross work of converting dBase function calls
//  to the SQL equivalent.  Some are super straightforward: `CHR(97)` -> `CHR(97)`
//  but others have no exact equivalent and have to resolve to a nested bundle.
pub fn translate_fn_call(
    name: &F,
    args: &[Box<ast::Expression>],
    cx: &impl TranslationContext,
) -> std::result::Result<(Box<Expression>, FieldType), Error> {
    let arg = |index: usize| get_arg(index, args, cx, name);
    let all_args = || get_all_args(args, cx);
    let wrong_type = |index| wrong_type(index, name, args);

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
        // CTOD(x) => TO_DATE(x, 'MM/DD/YY')
        F::CTOD => ok(
            Expression::FunctionCall {
                name: "TO_DATE".into(),
                args: vec![
                    arg(0)??.0,
                    //TODO the date format can be changed on the Codebase object
                    "MM/DD/YY".into(),
                ],
            },
            FieldType::Date,
        ),
        // DATE() => CURRENT_DATE
        F::DATE => ok(
            //TODO do we need to format as a string here?
            Expression::BareFunctionCall("CURRENT_DATE".to_string()),
            FieldType::Date,
        ),
        // DAY(x) => DATE_PART('DAY' FROM x)
        F::DAY => ok(
            Expression::FunctionCall {
                name: "DATE_PART".into(),
                args: vec!["DAY".into(), arg(0)??.0],
            },
            FieldType::Double,
        ),
        // DELETED() => __deleted
        F::DELETED => ok(
            Expression::Field {
                alias: None,
                name: "__deleted".into(),
                field_type: FieldType::Logical,
            },
            FieldType::Logical,
        ),

        // DTOC(x) => TO_CHAR(x, 'MM/DD/YY')
        F::DTOC => {
            if args.len() == 2 {
                // Equivalent to DTOS
                ok(
                    Expression::FunctionCall {
                        name: "TO_CHAR".into(),
                        args: vec![arg(0)??.0, "YYYYMMDD".into()],
                    },
                    FieldType::Character(8),
                )
            } else {
                ok(
                    Expression::FunctionCall {
                        name: "TO_CHAR".into(),
                        args: vec![
                            arg(0)??.0,
                            //TODO this is controlled by the Code4
                            "MM/DD/YY".into(),
                        ],
                    },
                    FieldType::Character(8),
                )
            }
        }
        F::DTOS => ok(
            Expression::FunctionCall {
                name: "TO_CHAR".into(),
                args: vec![arg(0)??.0, "YYYYMMDD".into()],
            },
            FieldType::Character(8),
        ),

        // IIF(x, y, z) => Iif expression
        F::IIF => {
            // The result type will be the type of the when_true expression
            let (when_true, ty) = arg(1)??;
            ok(
                Expression::Iif {
                    cond: arg(0)??.0,
                    when_true,
                    when_false: arg(2)??.0,
                },
                ty,
            )
        }
        // LEFT(x, n) => SUBSTR(x, 1, n)
        F::LEFT => ok(
            Expression::FunctionCall {
                name: "SUBSTR".to_string(),
                args: vec![arg(0)??.0, 1.into(), arg(1)??.0],
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
                args: vec!["MONTH".into(), arg(0)??.0],
            },
            FieldType::Double,
        ),

        // RECNO() => RECNO5
        F::RECNO => ok(
            Expression::Field {
                alias: None,
                name: "RECNO5".into(),
                field_type: FieldType::Integer,
            },
            FieldType::Integer,
        ),

        // RIGHT(x, n) => RIGHT(x, n)
        F::RIGHT => {
            let (x, ty) = arg(0)??;
            let n = match arg(1)??.0.as_ref() {
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

        // STOD(x) => TO_DATE(x, 'YYYYMMDD')
        F::STOD => ok(
            Expression::FunctionCall {
                name: "TO_DATE".to_string(),
                args: vec![arg(0)??.0, "YYYYMMDD".into()],
            },
            FieldType::Date,
        ),
        // STR(num, len, dec) => PRINTF("%{len}.{dec}f", num)
        F::STR => {
            // `len` and dec` must be constants according to CB docs, so we can
            //   get them and convert to integers, then mix up a printf call
            let len: i64 = match arg(1)??.0.as_ref() {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(1)),
                _ => Err(wrong_type(1)),
            }?;
            let dec: i64 = match arg(2)??.0.as_ref() {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(2)),
                _ => Err(wrong_type(2)),
            }?;
            // We need FMx.y where 'x' is '9' repeated len - dec - 1 times and
            //  'y' is '0' repeated dec times
            let fmt = format!(
                "FM{:9<x$}.{:0<y$}",
                "",
                "",
                x = (len - dec - 1) as usize,
                y = dec as usize
            );
            ok(
                Expression::FunctionCall {
                    name: "TO_CHAR".to_string(),
                    args: vec![
                        arg(0)??.0, // value to be formatted
                        fmt.into(),
                    ],
                },
                FieldType::Character(len as u32),
            )
        }
        F::SUBSTR => {
            let len: u32 = match arg(2)??.0.as_ref() {
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
                args: vec!["YEAR".into(), arg(0)??.0],
            },
            FieldType::Double,
        ),

        F::Unknown(unsupported) => Err(Error::UnsupportedFunction(unsupported.clone())),
    }
}

pub fn get_arg(
    index: usize,
    args: &[Box<ast::Expression>],
    cx: &impl TranslationContext,
    name: &F,
) -> std::result::Result<std::result::Result<(Box<Expression>, FieldType), Error>, Error> {
    args.get(index)
        .map(|a| translate(a, cx))
        .ok_or(Error::IncorrectArgCount(format!("{name:?}"), index))
}

pub fn get_all_args(
    args: &[Box<ast::Expression>],
    cx: &impl TranslationContext,
) -> std::result::Result<Vec<Box<Expression>>, Error> {
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

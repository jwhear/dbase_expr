use crate::ast;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    StartsWith,
    And,
    Or,
    Concat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    BoolLiteral(bool),
    NumberLiteral(String),
    SingleQuoteStringLiteral(String),
    Field {
        alias: Option<String>,
        name: String,
        field_type: FieldType,
    },
    FunctionCall {
        name: String,
        args: Vec<Box<Expression>>,
    },
    BinaryOperator(Box<Expression>, BinaryOp, Box<Expression>),
    UnaryOperator(UnaryOp, Box<Expression>),
    Cast(Box<Expression>, &'static str),
    Iif {
        cond: Box<Expression>,
        when_true: Box<Expression>,
        when_false: Box<Expression>,
    },
    // used for things like "CURRENT_DATE" which are functions but don't
    //  allow the parentheses.
    BareFunctionCall(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    UnsupportedFunction(String),
    IncorrectArgCount(String, usize),
    ArgWrongType {
        func: ast::Expression,
        wrong_arg_index: usize,
    },
    Other(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnsupportedFunction(name) => write!(f, "Unsupported function: {name}"),
            Self::IncorrectArgCount(name, count) => write!(
                f,
                "Function {name} called with an incorrect number of arguments (got {count})"
            ),
            Self::ArgWrongType {
                func,
                wrong_arg_index,
            } => write!(
                f,
                "Function {func:?}: argument {wrong_arg_index} is the wrong type",
            ),
            Self::Other(msg) => write!(f, "Error: {msg}"),
        }
    }
}

impl std::error::Error for Error {}

// These From implementations help the translation implementation
impl From<&str> for Box<Expression> {
    fn from(s: &str) -> Self {
        Box::new(Expression::SingleQuoteStringLiteral(s.to_string()))
    }
}
impl From<String> for Box<Expression> {
    fn from(s: String) -> Self {
        Box::new(Expression::SingleQuoteStringLiteral(s))
    }
}
impl From<i64> for Box<Expression> {
    fn from(s: i64) -> Self {
        Box::new(Expression::NumberLiteral(s.to_string()))
    }
}
pub type Result = std::result::Result<(Box<Expression>, FieldType), Error>;

fn ok(exp: Expression, ty: FieldType) -> Result {
    Ok((Box::new(exp), ty))
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum FieldType {
    //Binary = b'B',
    Character(u32) = b'C',
    CharacterBinary(u32) = b'Z',
    Currency = b'Y',
    DateTime = b'T',
    Date = b'D',
    Double = b'B',
    Float = b'F',
    General = b'G',
    Integer = b'I',
    Logical = b'L',
    Memo = b'M',
    MemoBinary = b'X',
    Numeric { len: u32, dec: u32 } = b'N',
    //Unicode = b'U',
}

impl FieldType {
    pub fn fixed_len(&self) -> Option<u32> {
        match self {
            Self::Character(len) | Self::CharacterBinary(len) | Self::Numeric { len, .. } => {
                Some(*len)
            }
            _ => None,
        }
    }
}

/// This trait allows the caller to control translation. See `DefaultPostgresTranslator`
///  for a good starting place.
pub trait TranslationContext {
    /// Called to determine the proper name and type of a field.
    ///   `alias`: the table reference if the field is qualified (in `foo.x` the alias is `foo`)
    ///   `field`: the name of the field from the expression
    ///
    /// On success, returns a tuple of the proper (e.g. capitalized) name and the field type
    fn lookup_field(
        &self,
        alias: Option<&str>,
        field: &str,
    ) -> std::result::Result<(String, FieldType), String>;

    /// Called to translate a function call.
    ///   `name`: the name of the function in the original expression
    ///   `args`: the arguments to the function
    ///
    /// On success, returns an expression and the type the expression would return.
    fn translate_fn_call(
        &self,
        name: &str,
        args: &[Box<ast::Expression>],
    ) -> std::result::Result<(Box<Expression>, FieldType), Error>;
}

/// Translates dBase expression to a SQL expression.
pub fn translate<C: TranslationContext>(source: &ast::Expression, cx: &C) -> Result {
    use ast::Expression as E;

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
                .map_err(|e| Error::Other(e))?;
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

/// This type provides default function translation for Postgres. You can
///  "inherit" while allowing overriding by implementing the TranslationContext
///  trait and dispatching to `default_translate_fn_call` any function calls
///  you're not interested in overriding.
pub struct DefaultPostgresTranslator<F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
{
    pub field_lookup: F,
}

impl<F> TranslationContext for DefaultPostgresTranslator<F>
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

    fn translate_fn_call(
        &self,
        name: &str,
        args: &[Box<ast::Expression>],
    ) -> std::result::Result<(Box<Expression>, FieldType), Error> {
        default_translate_fn_call(name, args, self)
    }
}

// This function does the kind of gross work of converting dBase function calls
//  to the SQL equivalent.  Some are super straightforward: `CHR(97)` -> `CHR(97)`
//  but others have no exact equivalent and have to resolve to a nested bundle.
pub fn default_translate_fn_call(
    name: &str,
    args: &[Box<ast::Expression>],
    cx: &impl TranslationContext,
) -> std::result::Result<(Box<Expression>, FieldType), Error> {
    let name = name.to_uppercase();
    // Helper to get the specified argument or return the appropriate error
    let arg = |index: usize| {
        args.get(index)
            .map(|a| translate(a, cx))
            .ok_or(Error::IncorrectArgCount(name.clone(), index))
    };

    // Translates all arguments and puts them into a Result<Vec>
    let all_args = || {
        let res: std::result::Result<Vec<_>, Error> =
            args.iter().map(|a| translate(a, cx).map(|r| r.0)).collect();
        res
    };

    // Helper for constructing an error if the CodeBase function was called with
    //  an incorrect argument type (usually a function that requires an integer
    //  or string literal)
    let wrong_type = |index| Error::ArgWrongType {
        func: ast::Expression::FunctionCall {
            name: name.clone(),
            args: args.into(),
        },
        wrong_arg_index: index,
    };

    match name.as_str() {
        // ALLTRIM(x) => TRIM(x)
        "ALLTRIM" => ok(
            Expression::FunctionCall {
                name: "TRIM".to_string(),
                args: vec![arg(0)??.0],
            },
            FieldType::Memo,
        ),
        // CHR(x) => CHR(x)
        "CHR" => ok(
            Expression::FunctionCall {
                name,
                args: all_args()?,
            },
            FieldType::Character(1),
        ),
        // CTOD(x) => TO_DATE(x, 'MM/DD/YY')
        "CTOD" => ok(
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
        "DATE" => ok(
            //TODO do we need to format as a string here?
            Expression::BareFunctionCall("CURRENT_DATE".to_string()),
            FieldType::Date,
        ),
        // DAY(x) => DATE_PART('DAY' FROM x)
        "DAY" => ok(
            Expression::FunctionCall {
                name: "DATE_PART".into(),
                args: vec!["DAY".into(), arg(0)??.0],
            },
            FieldType::Double,
        ),
        // DELETED() => __deleted
        "DELETED" => ok(
            Expression::Field {
                alias: None,
                name: "__deleted".into(),
                field_type: FieldType::Logical,
            },
            FieldType::Logical,
        ),

        // DTOC(x) => TO_CHAR(x, 'MM/DD/YY')
        "DTOC" => {
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
        "DTOS" => ok(
            Expression::FunctionCall {
                name: "TO_CHAR".into(),
                args: vec![arg(0)??.0, "YYYYMMDD".into()],
            },
            FieldType::Character(8),
        ),

        // IIF(x, y, z) => Iif expression
        "IIF" => {
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
        "LEFT" => ok(
            Expression::FunctionCall {
                name: "SUBSTR".to_string(),
                args: vec![arg(0)??.0, 1.into(), arg(1)??.0],
            },
            FieldType::Memo,
        ),
        "LTRIM" => ok(
            Expression::FunctionCall {
                name: "LTRIM".into(),
                args: vec![arg(0)??.0],
            },
            FieldType::Memo,
        ),

        // MONTH(x) => DATE_PART('MONTH', x)
        "MONTH" => ok(
            Expression::FunctionCall {
                name: "DATE_PART".into(),
                args: vec!["MONTH".into(), arg(0)??.0],
            },
            FieldType::Double,
        ),

        // RECNO() => RECNO5
        "RECNO" => ok(
            Expression::Field {
                alias: None,
                name: "RECNO5".into(),
                field_type: FieldType::Integer,
            },
            FieldType::Integer,
        ),

        // RIGHT(x, n) => RIGHT(x, n)
        "RIGHT" => {
            let (x, ty) = arg(0)??;
            let n = match arg(1)??.0.as_ref() {
                Expression::NumberLiteral(v) => {
                    u32::from_str_radix(&v, 10).map_err(|_| wrong_type(1))
                }
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

        // STOD(x) => TO_DATE(x, 'YYYYMMDD')
        "STOD" => ok(
            Expression::FunctionCall {
                name: "TO_DATE".to_string(),
                args: vec![arg(0)??.0, "YYYYMMDD".into()],
            },
            FieldType::Date,
        ),
        // STR(num, len, dec) => PRINTF("%{len}.{dec}f", num)
        "STR" => {
            // `len` and dec` must be constants according to CB docs, so we can
            //   get them and convert to integers, then mix up a printf call
            let len = match arg(1)??.0.as_ref() {
                Expression::NumberLiteral(v) => {
                    i64::from_str_radix(&v, 10).map_err(|_| wrong_type(1))
                }
                _ => Err(wrong_type(1)),
            }?;
            let dec = match arg(2)??.0.as_ref() {
                Expression::NumberLiteral(v) => {
                    i64::from_str_radix(&v, 10).map_err(|_| wrong_type(2))
                }
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
        "SUBSTR" => {
            let len = match arg(2)??.0.as_ref() {
                Expression::NumberLiteral(v) => {
                    u32::from_str_radix(&v, 10).map_err(|_| wrong_type(2))
                }
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
        "TRIM" => ok(
            Expression::FunctionCall {
                name: "RTRIM".into(),
                args: vec![arg(0)??.0],
            },
            FieldType::Memo,
        ),
        "UPPER" => {
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
        "VAL" => ok(
            Expression::Cast(arg(0)??.0, "numeric"),
            FieldType::Numeric { len: 0, dec: 0 },
        ),

        // YEAR(x) => DATE_PART('YEAR', x)
        "YEAR" => ok(
            Expression::FunctionCall {
                name: "DATE_PART".into(),
                args: vec!["YEAR".into(), arg(0)??.0],
            },
            FieldType::Double,
        ),

        _ => Err(Error::UnsupportedFunction(name)),
    }
}

//NOTE(justin): This function almost certainly has a bug hiding in it.
fn escape_single_quotes(s: &str) -> String {
    let mut res = String::new();
    res.reserve(s.len());

    let mut is_escaped = false;
    for c in s.chars() {
        if c == '\\' && !is_escaped {
            is_escaped = true;
        } else {
            is_escaped = false;
        }

        if c == '\'' && !is_escaped {
            res.push('\\');
        }

        res.push(c);
    }
    res
}

// The left side of the string comparison should be truncated to the length of the right side (basically a startswith compare)
fn string_comp_left(l: Box<Expression>, r: Box<Expression>) -> Box<Expression> {
    let right_side_len_expression = Box::new(Expression::FunctionCall {
        name: "LENGTH".into(),
        args: vec![r],
    });
    let left_side = Box::new(Expression::FunctionCall {
        name: "SUBSTR".into(),
        args: vec![
            l,
            Box::new(Expression::NumberLiteral("1".into())),
            right_side_len_expression,
        ],
    });
    left_side
}

// The right side of the string comparison should be truncated to the fixed length, no need to evaluate additional characters
fn string_comp_right(r: Box<ast::Expression>, len: u32) -> Box<ast::Expression> {
    let expression = Box::new(ast::Expression::FunctionCall {
        name: "SUBSTR".into(),
        args: vec![
            r,
            Box::new(ast::Expression::NumberLiteral("1".into())),
            Box::new(ast::Expression::NumberLiteral(len.to_string().into())),
        ],
    });
    expression
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_escape_single_quotes() {
        assert_eq!("foo", escape_single_quotes("foo"));
        assert_eq!(r"\'", escape_single_quotes(r"'"));
        assert_eq!(r"\\'", escape_single_quotes(r"\'"));
    }
}

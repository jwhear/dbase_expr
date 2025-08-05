use crate::ast;

pub mod postgres;

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

/// This is the output type of translation: a Codebase AST goes in, a SQL AST
///  comes out.
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

/// This trait allows the caller to control translation. When implementing a new
///  translation target, a reasonable strategy is to delegate to the Postgres
///  translator but intercept anything that needs to be handled differently:
///
/// ```rust
/// struct MyCustomTranslator
/// {
///     my_state: SomeState,
/// }
///
/// impl TranslationContext for MyCustomTranslator
/// {
///     fn lookup_field(
///         &self,
///         alias: Option<&str>,
///         field: &str,
///     ) -> std::result::Result<(String, FieldType), String> {
///         //TODO use self.my_state
///     }
///     
///     fn translate(&self, source: &ast::Expression) -> Result {
///         //TODO handle specific cases which are different from Postgres,
///         // including cases which should be errors
///
///         // Everything else can be delegated:
///         translate::postgres::translate(source, self)
///     }
///     
///     fn translate_fn_call(
///         &self,
///         name: &str,
///         args: &[Box<ast::Expression>],
///     ) -> std::result::Result<(Box<Expression>, FieldType), Error> {
///         //TODO similar pattern: most function calls probably resolve to the
///         // same thing that Postgres uses but handle the differences here
///
///         // and delegate the rest...
///         translate_fn_call(name, args, self)
///     }
/// }
///
/// ```
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

    /// Called to translate an expression generally.
    fn translate(&self, source: &ast::Expression) -> Result;

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
pub fn string_comp_left(l: Box<Expression>, r: Box<Expression>) -> Box<Expression> {
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
pub fn string_comp_right(r: Box<ast::Expression>, len: u32) -> Box<ast::Expression> {
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

/// This type provides default function translation for Postgres. You can
///  "inherit" while allowing overriding by implementing the TranslationContext
///  trait and dispatching to `default_translate_fn_call` any function calls
///  you're not interested in overriding.
pub struct DefaultSqliteTranslator<F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
{
    pub field_lookup: F,
}

impl<F> TranslationContext for DefaultSqliteTranslator<F>
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
        sqlite_translate_fn_call(name, args, self)
    }
}

// This function does the kind of gross work of converting dBase function calls
//  to the SQL equivalent.  Some are super straightforward: `CHR(97)` -> `CHR(97)`
//  but others have no exact equivalent and have to resolve to a nested bundle.
pub fn sqlite_translate_fn_call(
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
        "ALLTRIM" => ok(
            Expression::FunctionCall {
                name: "TRIM".to_string(),
                args: vec![arg(0)??.0],
            },
            FieldType::Memo,
        ),

        "CHR" => ok(
            Expression::FunctionCall {
                name: "CHAR".to_string(), // SQLite equivalent
                args: all_args()?,
            },
            FieldType::Character(1),
        ),

        "CTOD" => ok(
            Expression::FunctionCall {
                name: "DATE".to_string(),
                args: vec![arg(0)??.0], // assumes date in ISO 8601 or needs pre-processing
            },
            FieldType::Date,
        ),

        "DATE" => ok(
            Expression::BareFunctionCall("CURRENT_DATE".to_string()),
            FieldType::Date,
        ),

        "DAY" => ok(
            Expression::FunctionCall {
                name: "CAST".to_string(),
                args: vec![Box::new(Expression::FunctionCall {
                    name: "STRFTIME".to_string(),
                    args: vec!["'%d'".into(), arg(0)??.0],
                })],
            },
            FieldType::Double,
        ),

        "DELETED" => ok(
            Expression::Field {
                alias: None,
                name: "__deleted".into(),
                field_type: FieldType::Logical,
            },
            FieldType::Logical,
        ),

        "DTOC" => {
            if args.len() == 2 {
                // Equivalent to DTOS
                ok(
                    Expression::FunctionCall {
                        name: "STRFTIME".into(),
                        args: vec!["'%Y%m%d'".into(), arg(0)??.0],
                    },
                    FieldType::Character(8),
                )
            } else {
                ok(
                    Expression::FunctionCall {
                        name: "STRFTIME".into(),
                        args: vec!["'%m/%d/%y'".into(), arg(0)??.0],
                    },
                    FieldType::Character(8),
                )
            }
        }

        "DTOS" => ok(
            Expression::FunctionCall {
                name: "STRFTIME".into(),
                args: vec!["'%Y%m%d'".into(), arg(0)??.0],
            },
            FieldType::Character(8),
        ),

        "IIF" => {
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

        "MONTH" => ok(
            Expression::FunctionCall {
                name: "CAST".to_string(),
                args: vec![Box::new(Expression::FunctionCall {
                    name: "STRFTIME".into(),
                    args: vec!["'%m'".into(), arg(0)??.0],
                })],
            },
            FieldType::Double,
        ),

        "RECNO" => ok(
            Expression::Field {
                alias: None,
                name: "RECNO5".into(),
                field_type: FieldType::Integer,
            },
            FieldType::Integer,
        ),

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
                    name: "SUBSTR".into(),
                    args: vec![x, (-i64::try_from(n).unwrap()).into()],
                },
                out_ty,
            )
        }

        "STOD" => {
            // Convert 'YYYYMMDD' -> 'YYYY-MM-DD' using SUBSTR
            ok(
                Expression::FunctionCall {
                    name: "DATE".into(),
                    args: vec![Box::new(Expression::FunctionCall {
                        name: "printf".into(),
                        args: vec![
                            "'%s-%s-%s'".into(),
                            Box::new(Expression::FunctionCall {
                                name: "SUBSTR".into(),
                                args: vec![arg(0)??.0.clone(), 1.into(), 4.into()],
                            }),
                            Box::new(Expression::FunctionCall {
                                name: "SUBSTR".into(),
                                args: vec![arg(0)??.0.clone(), 5.into(), 2.into()],
                            }),
                            Box::new(Expression::FunctionCall {
                                name: "SUBSTR".into(),
                                args: vec![arg(0)??.0, 7.into(), 2.into()],
                            }),
                        ],
                    })],
                },
                FieldType::Date,
            )
        }

        "STR" => {
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
            let fmt = format!("%{}.{}f", len, dec); // e.g. "%.2f"
            ok(
                Expression::FunctionCall {
                    name: "PRINTF".to_string(),
                    args: vec![fmt.into(), arg(0)??.0],
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

        "VAL" => ok(
            Expression::Cast(arg(0)??.0, "REAL"),
            FieldType::Numeric { len: 0, dec: 0 },
        ),

        "YEAR" => ok(
            Expression::FunctionCall {
                name: "CAST".to_string(),
                args: vec![Box::new(Expression::FunctionCall {
                    name: "STRFTIME".into(),
                    args: vec!["'%Y'".into(), arg(0)??.0],
                })],
            },
            FieldType::Double,
        ),

        _ => Err(Error::UnsupportedFunction(name)),
    }
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

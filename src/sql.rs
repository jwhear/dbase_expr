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
    Like,
    And,
    Or,
    Concat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StorageClass {
    None,
    Text,
    Real,
    Integer,
    Numeric,
}

#[derive(Debug, Clone)]
pub enum Expression {
    BoolLiteral(bool),
    NumberLiteral(String),
    SingleQuoteStringLiteral(String),
    Field {
        alias: Option<String>,
        name: String,
    },
    FunctionCall {
        name: String,
        args: Vec<Box<Expression>>,
    },
    BinaryOperator(Box<Expression>, BinaryOp, Box<Expression>),
    UnaryOperator(UnaryOp, Box<Expression>),
    Cast(Box<Expression>, StorageClass),
}

#[derive(Debug, Clone)]
pub enum Error {
    UnsupportedFunction(String),
    IncorrectArgCount(String, usize),
    ArgWrongType {
        func: ast::Expression,
        wrong_arg_index: usize,
    },
}

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
pub type Result = std::result::Result<Box<Expression>, Error>;

fn ok(exp: Expression) -> Result {
    Ok(Box::new(exp))
}

pub fn translate(source: &ast::Expression) -> Result {
    use ast::Expression as E;

    let binop = |l, op, r| ok(Expression::BinaryOperator(translate(l)?, op, translate(r)?));

    match source {
        E::BoolLiteral(v) => ok(Expression::BoolLiteral(*v)),
        E::NumberLiteral(v) => ok(Expression::NumberLiteral(v.clone())),
        E::SingleQuoteStringLiteral(v) => ok(Expression::SingleQuoteStringLiteral(
            escape_single_quotes(v),
        )),
        E::DoubleQuoteStringLiteral(v) => ok(Expression::SingleQuoteStringLiteral(
            escape_single_quotes(v),
        )),
        E::Field { alias, name } => ok(Expression::Field {
            alias: alias.clone(),
            name: name.clone(),
        }),
        E::UnaryOperator(op, r) => match op {
            ast::UnaryOp::Not => ok(Expression::UnaryOperator(UnaryOp::Not, translate(r)?)),
            ast::UnaryOp::Neg => ok(Expression::UnaryOperator(UnaryOp::Neg, translate(r)?)),
        },
        E::BinaryOperator(l, op, r) => match op {
            //TODO Add, Sub are ambiguous: could be concat or days (for dates)
            // Use julianday(date) to transform a date into days
            ast::BinaryOp::Add => binop(l, BinaryOp::Add, r),
            ast::BinaryOp::Sub => binop(l, BinaryOp::Sub, r),
            ast::BinaryOp::Mul => binop(l, BinaryOp::Mul, r),
            ast::BinaryOp::Div => binop(l, BinaryOp::Div, r),
            ast::BinaryOp::Eq => binop(l, BinaryOp::Eq, r),
            ast::BinaryOp::Ne => binop(l, BinaryOp::Ne, r),
            ast::BinaryOp::Lt => binop(l, BinaryOp::Lt, r),
            ast::BinaryOp::Le => binop(l, BinaryOp::Le, r),
            ast::BinaryOp::Gt => binop(l, BinaryOp::Gt, r),
            ast::BinaryOp::Ge => binop(l, BinaryOp::Ge, r),
            ast::BinaryOp::And => binop(l, BinaryOp::And, r),
            ast::BinaryOp::Or => binop(l, BinaryOp::Or, r),

            // SQL doesn't have an exponentation operator, use the POW function
            ast::BinaryOp::Exp => ok(Expression::FunctionCall {
                name: "POW".to_string(),
                args: vec![translate(l)?, translate(r)?],
            }),

            // SQL doesn't have a contain operator, use the INSTR function
            //NOTE(justin): not using LIKE here because the needle might contain
            // LIKE wildcards (% and _).
            ast::BinaryOp::Contain => ok(Expression::FunctionCall {
                name: "INSTR".to_string(),
                // Note that in CodeBase the haystack is the right arg, while in
                //  SQLite it's the left arg
                args: vec![translate(r)?, translate(l)?],
            }),
        },
        E::FunctionCall { name, args } => translate_function_call(name, args),
    }
}

fn translate_function_call(name: &str, args: &[Box<ast::Expression>]) -> Result {
    let name = name.to_uppercase();

    // Helper to get the specified argument or return the appropriate error
    let arg = |index: usize| {
        args.get(index)
            .map(|a| translate(a))
            .ok_or(Error::IncorrectArgCount(name.clone(), index))
    };

    // Translates all arguments and puts them into a Result<Vec>
    let all_args = || {
        let res: std::result::Result<Vec<_>, Error> = args.iter().map(|a| translate(a)).collect();
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

    // Helper macro: expands into a FunctionCall expression with literal arguments
    macro_rules! f {
        ($name:literal, $($args:expr),*) => {
            Box::new(Expression::FunctionCall {
                name: $name.to_string(),
                args: vec![$($args.into()),*],
            })
        };
    }

    match name.as_str() {
        // ALLTRIM(x) => TRIM(x)
        "ALLTRIM" => ok(Expression::FunctionCall {
            name: "TRIM".to_string(),
            args: vec![arg(0)??],
        }),
        // CHR(x) => CHR(x)
        "CHR" => ok(Expression::FunctionCall {
            name,
            args: all_args()?,
        }),
        // DATE() => STRFTIME('%Y%m%d')
        "DATE" => ok(Expression::FunctionCall {
            name: "STRFTIME".to_string(),
            args: vec!["%Y%m%d".into()],
        }),
        // IIF(x, y, z) => IIF(x, y, z)
        "IIF" => ok(Expression::FunctionCall {
            name,
            args: all_args()?,
        }),
        // LEFT(x, n) => SUBSTR(x, 1, n)
        "LEFT" => ok(Expression::FunctionCall {
            name: "SUBSTR".to_string(),
            args: vec![arg(0)??, 1.into(), arg(1)??],
        }),
        // STOD(x) => CONCAT_WS('-', SUBSTR(x, 1, 4), SUBSTR(x, 5, 2), SUBSTR(x, 7, 2))
        //                           | 4 digit year|  | 2 digit month| | 2 digit day |
        "STOD" => ok(Expression::FunctionCall {
            name: "CONCAT_WS".to_string(),
            args: vec![
                "-".into(),
                f!("SUBSTR", arg(0)??, 1i64, 4i64),
                f!("SUBSTR", arg(0)??, 5i64, 2i64),
                f!("SUBSTR", arg(0)??, 7i64, 2i64),
            ],
        }),
        // STR(num, len, dec) => PRINTF("%{len}.{dec}f", num)
        "STR" => {
            // `len` and dec` must be constants according to CB docs, so we can
            //   get them and convert to integers, then mix up a printf call
            let len = match arg(1)??.as_ref() {
                Expression::NumberLiteral(v) => {
                    i64::from_str_radix(&v, 10).map_err(|_| wrong_type(1))
                }
                _ => Err(wrong_type(1)),
            }?;
            let dec = match arg(2)??.as_ref() {
                Expression::NumberLiteral(v) => {
                    i64::from_str_radix(&v, 10).map_err(|_| wrong_type(2))
                }
                _ => Err(wrong_type(2)),
            }?;
            ok(Expression::FunctionCall {
                name: "PRINTF".to_string(),
                args: vec![
                    format!("%{len}.{dec}f").into(),
                    arg(0)??, // value to be formatted
                ],
            })
        }
        // VAL(x) => CAST (x as NUMERIC)
        "VAL" => ok(Expression::Cast(arg(0)??, StorageClass::Numeric)),
        _ => Err(Error::UnsupportedFunction(name)),
    }
}

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

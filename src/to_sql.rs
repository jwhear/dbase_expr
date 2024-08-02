use crate::sql::{BinaryOp, Expression, StorageClass, UnaryOp};
use std::fmt::{Display, Formatter, Result};

#[derive(Debug, Clone, Copy)]
pub struct PrinterConfig {
    //pretty: bool,
    //indent: i32,
}
impl Default for PrinterConfig {
    fn default() -> Self {
        Self {
            //pretty: false,
            //indent: 0,
        }
    }
}

pub struct Printer<T> {
    tree: T,
    config: PrinterConfig,
}

impl<T> Printer<T> {
    pub fn new(tree: T) -> Self {
        Self {
            tree,
            config: PrinterConfig::default(),
        }
    }
}

pub trait ToSQL {
    fn to_sql(&self, out: &mut Formatter, conf: PrinterConfig) -> Result;
}

impl<T> ToSQL for Box<T>
where
    T: ToSQL,
{
    fn to_sql(&self, out: &mut Formatter, conf: PrinterConfig) -> Result {
        self.as_ref().to_sql(out, conf)
    }
}

impl<T> Display for Printer<T>
where
    T: ToSQL,
{
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.tree.to_sql(f, self.config)
    }
}

impl ToSQL for BinaryOp {
    fn to_sql(&self, out: &mut Formatter, _: PrinterConfig) -> Result {
        match self {
            BinaryOp::Add => write!(out, "+"),
            BinaryOp::Sub => write!(out, "-"),
            BinaryOp::Mul => write!(out, "*"),
            BinaryOp::Div => write!(out, "/"),

            BinaryOp::Eq => write!(out, "="),
            BinaryOp::Ne => write!(out, "!="),
            BinaryOp::Lt => write!(out, "<"),
            BinaryOp::Le => write!(out, "<="),
            BinaryOp::Gt => write!(out, ">"),
            BinaryOp::Ge => write!(out, ">="),
            BinaryOp::Like => write!(out, " LIKE "),

            BinaryOp::And => write!(out, " AND "),
            BinaryOp::Or => write!(out, " OR "),
            BinaryOp::Concat => write!(out, " || "),
        }
    }
}

impl ToSQL for StorageClass {
    fn to_sql(&self, out: &mut Formatter, _: PrinterConfig) -> Result {
        match self {
            StorageClass::None => write!(out, "NONE"),
            StorageClass::Text => write!(out, "TEXT"),
            StorageClass::Real => write!(out, "REAL"),
            StorageClass::Integer => write!(out, "INTEGER"),
            StorageClass::Numeric => write!(out, "NUMERIC"),
        }
    }
}

impl ToSQL for Expression {
    fn to_sql(&self, out: &mut Formatter, conf: PrinterConfig) -> Result {
        match self {
            Expression::BoolLiteral(v) => {
                write!(out, "{}", if *v { "TRUE" } else { "FALSE" })
            }
            Expression::NumberLiteral(v) => write!(out, "{v}"),
            Expression::SingleQuoteStringLiteral(v) => write!(out, "'{v}'"),

            // The content of the double quoted string is about to be put into
            //  single quotes, so escape any bare single quotes
            Expression::Field { alias: None, name } => write!(out, "{name}"),
            Expression::Field {
                alias: Some(alias),
                name,
            } => write!(out, "{alias}.{name}"),
            Expression::UnaryOperator(op, exp) => {
                write!(out, "(")?;
                match op {
                    UnaryOp::Not => write!(out, "NOT "),
                    UnaryOp::Neg => write!(out, "-"),
                }?;
                exp.to_sql(out, conf)?;
                write!(out, ")")
            }
            Expression::BinaryOperator(l, op, r) => {
                //TODO(justin): order of operations is preserved by parenthesizing
                // everything.  It'd be nice to analyze precedence to only do so
                // when necessary.
                write!(out, "(")?;
                l.to_sql(out, conf)?;
                op.to_sql(out, conf)?;
                r.to_sql(out, conf)?;
                write!(out, ")")
            }
            Expression::FunctionCall { name, args } => {
                write!(out, "{name}(")?;
                let mut is_first = true;
                for arg in args.iter() {
                    if is_first {
                        is_first = false;
                    } else {
                        write!(out, ",")?;
                    }
                    arg.to_sql(out, conf)?;
                }
                write!(out, ")")
            }
            Expression::Cast(expr, to) => {
                write!(out, "CAST (")?;
                expr.to_sql(out, conf)?;
                write!(out, " AS ")?;
                to.to_sql(out, conf)?;
                write!(out, ")")
            }
        }
    }
}

/*fn translate_function_call(name: &str, args: &Vec<Box<Expression>>) -> Expression {
    let name = name.to_uppercase();

    macro_rules! f {
        ($name:literal, $($args:expr),*) => {
            Expression::FunctionCall {
                name: $name.to_string(),
                args: vec![$($args.into()),*],
            }
        };
    }

    match name.as_str() {
        "ALLTRIM" => f!("trim", args[0].clone()),
        "DATE" => f!("strftime", "%Y%m%d"),
        // SQLite just stores dates as strings.  We do need to introduce dashes
        //  to transform "YYYYMMDD" to "YYYY-MM-DD"
        "STOD" => f!(
            "concat_ws",
            "-",
            f!("substr", 1, 4),
            f!("substr", 5, 2),
            f!("substr", 7, 2)
        ),
        unknown => panic!("Unknown function name: {unknown}"),
    }
}*/

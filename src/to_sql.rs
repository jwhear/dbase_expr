use crate::translate::{BinaryOp, Expression, FieldType, UnaryOp};
use std::fmt::{Display, Formatter, Result};

#[derive(Debug, Clone, Copy)]
pub struct PrinterConfig {
    //pretty: bool,
    //indent: i32,
    pad_string_fields: bool,
}
impl Default for PrinterConfig {
    fn default() -> Self {
        Self {
            //pretty: false,
            //indent: 0,
            pad_string_fields: true,
        }
    }
}

pub struct Printer<T> {
    tree: T,
    config: PrinterConfig,
}

impl<T> Printer<T> {
    pub fn new(tree: T, pad_string_fields: bool) -> Self {
        Self {
            tree,
            config: PrinterConfig { pad_string_fields },
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
            BinaryOp::NotLike => write!(out, " NOT LIKE "),

            BinaryOp::And => write!(out, " AND "),
            BinaryOp::Or => write!(out, " OR "),
            BinaryOp::Concat => write!(out, " || "),
        }
    }
}

impl ToSQL for Expression {
    fn to_sql(&self, out: &mut Formatter, conf: PrinterConfig) -> Result {
        // Fixed-length fields may be stored as NULL or right-trimmed. We need
        //  pad them back out
        let mut padded = |inner: &str, width: u32| match conf.pad_string_fields {
            true => write!(out, "RPAD(COALESCE({}, ''), {}, ' ')", inner, width),
            false => write!(out, "COALESCE({}, '')", inner),
        };

        match self {
            Expression::BoolLiteral(v) => {
                write!(out, "{}", if *v { "TRUE" } else { "FALSE" })
            }
            Expression::NumberLiteral(v) => write!(out, "{v}"),
            Expression::SingleQuoteStringLiteral(v) => write!(out, "'{v}'"),

            Expression::Field {
                alias,
                name,
                field_type,
            } => {
                let full_name = if let Some(alias) = alias {
                    format!("{alias}.\"{name}\"")
                } else {
                    format!("\"{name}\"")
                };
                if let FieldType::Character(width) = field_type {
                    padded(&full_name, *width)
                } else {
                    out.write_str(&full_name)
                }
            }
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
                write!(out, " AS {to}")?;
                write!(out, ")")
            }
            Expression::Iif {
                cond,
                when_true,
                when_false,
            } => {
                write!(out, "CASE WHEN ")?;
                cond.to_sql(out, conf)?;
                write!(out, " THEN ")?;
                when_true.to_sql(out, conf)?;
                write!(out, " ELSE ")?;
                when_false.to_sql(out, conf)?;
                write!(out, " END")
            }
            Expression::BareFunctionCall(name) => write!(out, " {name} "),
        }
    }
}

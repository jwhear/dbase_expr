use crate::translate::{BinaryOp, Expression, FieldType, UnaryOp};
use std::fmt::{Display, Formatter, Result};

pub trait PrinterContext: std::fmt::Debug {
    fn write_padding(&self, out: &mut Formatter<'_>, inner: &str, width: u32) -> std::fmt::Result;
    fn box_clone(&self) -> Box<dyn PrinterContext>;
}

impl Clone for Box<dyn PrinterContext> {
    fn clone(&self) -> Box<dyn PrinterContext> {
        self.box_clone()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PostgresPrinterContext;

impl PrinterContext for PostgresPrinterContext {
    fn write_padding(&self, out: &mut Formatter<'_>, inner: &str, width: u32) -> std::fmt::Result {
        write!(out, "RPAD(COALESCE({}, ''), {}, ' ')", inner, width)
    }
    fn box_clone(&self) -> Box<dyn PrinterContext> {
        Box::new(*self) // requires Copy on PostgresPrinterContext
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SqlitePrinterContext;

impl PrinterContext for SqlitePrinterContext {
    fn write_padding(&self, out: &mut Formatter<'_>, inner: &str, width: u32) -> std::fmt::Result {
        let spaces = " ".repeat(width as usize);
        write!(
            out,
            "COALESCE({}, '') || SUBSTR('{}', 1, CASE WHEN {} - LENGTH(COALESCE({}, '')) > 0 THEN {} - LENGTH(COALESCE({}, '')) ELSE 0 END)",
            inner, spaces, width, inner, width, inner
        )
    }
    fn box_clone(&self) -> Box<dyn PrinterContext> {
        Box::new(*self) // requires Copy on PostgresPrinterContext
    }
}

#[derive(Debug, Clone)]
pub struct PrinterConfig {
    pub context: Box<dyn PrinterContext>,
}

impl Default for PrinterConfig {
    fn default() -> Self {
        Self {
            context: Box::new(PostgresPrinterContext),
        }
    }
}

pub struct Printer<T> {
    tree: T,
    config: PrinterConfig,
}

impl<T> Printer<T> {
    pub fn new(tree: T, config: PrinterConfig) -> Self {
        Self { tree, config }
    }
}

pub trait ToSQL {
    fn to_sql(&self, out: &mut Formatter, conf: &PrinterConfig) -> Result;
}

impl<T> ToSQL for Box<T>
where
    T: ToSQL,
{
    fn to_sql(&self, out: &mut Formatter, conf: &PrinterConfig) -> Result {
        self.as_ref().to_sql(out, conf)
    }
}

impl<T> Display for Printer<T>
where
    T: ToSQL,
{
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.tree.to_sql(f, &self.config)
    }
}

impl ToSQL for BinaryOp {
    fn to_sql(&self, out: &mut Formatter, _: &PrinterConfig) -> Result {
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
            BinaryOp::And => write!(out, " AND "),
            BinaryOp::Or => write!(out, " OR "),
            BinaryOp::Concat => write!(out, " || "),
            BinaryOp::StartsWith => write!(out, " ^@ "),
            BinaryOp::Between => write!(out, " BETWEEN "),
            BinaryOp::NotBetween => write!(out, " NOT BETWEEN "),
        }
    }
}

impl ToSQL for Expression {
    fn to_sql(&self, out: &mut Formatter, conf: &PrinterConfig) -> Result {
        match self {
            Expression::BoolLiteral(v) => {
                write!(out, "{}", if *v { "TRUE" } else { "FALSE" })
            }
            Expression::NumberLiteral(v) => write!(out, "{v}"),
            Expression::SingleQuoteStringLiteral(v) => write!(out, "'{v}'"),

            Expression::Collate(v, c) => {
                v.to_sql(out, conf)?;
                write!(out, " COLLATE {c}")
            }
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
                    conf.context.write_padding(out, &full_name, *width)
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
            Expression::BinaryOperator(l, op, r, p) => {
                //TODO(justin): order of operations is preserved by parenthesizing
                // everything.  It'd be nice to analyze precedence to only do so
                // when necessary.
                if *p {
                    write!(out, "(")?;
                }
                l.to_sql(out, conf)?;
                op.to_sql(out, conf)?;
                r.to_sql(out, conf)?;
                if *p {
                    write!(out, ")")?;
                }
                Ok(())
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

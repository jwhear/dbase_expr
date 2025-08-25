use crate::translate::{BinaryOp, COALESCE_DATE, Expression, FieldType, UnaryOp};
use std::{
    cell::RefCell,
    fmt::{Display, Formatter, Result},
    rc::Rc,
};

pub trait PrinterContext: std::fmt::Debug {
    fn write_padding(&self, out: &mut Formatter<'_>, inner: &str, width: u32) -> std::fmt::Result;
    fn coalesce_date(&self, out: &mut Formatter<'_>, inner: &str) -> std::fmt::Result;
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
    fn coalesce_date(&self, out: &mut Formatter<'_>, inner: &str) -> std::fmt::Result {
        write!(out, "COALESCE({}, DATE '{}')", inner, COALESCE_DATE)
    }
    fn box_clone(&self) -> Box<dyn PrinterContext> {
        Box::new(*self) // requires Copy on PostgresPrinterContext
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SqlitePrinterContext {
    pub pad_strings: bool,
}

impl PrinterContext for SqlitePrinterContext {
    fn write_padding(&self, out: &mut Formatter<'_>, inner: &str, width: u32) -> std::fmt::Result {
        if self.pad_strings {
            let spaces = " ".repeat(width as usize);
            write!(
                out,
                "COALESCE({}, '') || SUBSTR('{}', 1, CASE WHEN {} - LENGTH(COALESCE({}, '')) > 0 THEN {} - LENGTH(COALESCE({}, '')) ELSE 0 END)",
                inner, spaces, width, inner, width, inner
            )
        } else {
            write!(out, "{}", inner)
        }
    }
    fn coalesce_date(&self, out: &mut Formatter<'_>, inner: &str) -> std::fmt::Result {
        write!(out, "COALESCE({}, DATE('{}'))", inner, COALESCE_DATE)
    }
    fn box_clone(&self) -> Box<dyn PrinterContext> {
        Box::new(*self) // requires Copy on PostgresPrinterContext
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MssqlPrinterContext;

impl PrinterContext for MssqlPrinterContext {
    fn write_padding(&self, out: &mut Formatter<'_>, inner: &str, width: u32) -> std::fmt::Result {
        write!(
            out,
            "LEFT(COALESCE({}, '') + REPLICATE(' ', {}), {})",
            inner, width, width
        )
    }
    fn coalesce_date(&self, out: &mut Formatter<'_>, inner: &str) -> std::fmt::Result {
        write!(out, "COALESCE({}, '{}')", inner, COALESCE_DATE)
    }
    fn box_clone(&self) -> Box<dyn PrinterContext> {
        Box::new(*self)
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

impl<T> ToSQL for Rc<RefCell<T>>
where
    T: ToSQL,
{
    fn to_sql(&self, out: &mut Formatter, conf: &PrinterConfig) -> Result {
        self.as_ref().borrow().to_sql(out, conf)
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
                } else if let FieldType::Date = field_type {
                    conf.context.coalesce_date(out, &full_name)
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
                p.open(out)?;
                l.to_sql(out, conf)?;
                op.to_sql(out, conf)?;
                r.to_sql(out, conf)?;
                p.close(out)
            }
            Expression::BinaryOperatorSequence(op, exprs) => {
                assert!(exprs.len() >= 2);
                write!(out, "(")?;
                exprs[0].to_sql(out, conf)?;
                for e in &exprs[1..] {
                    op.to_sql(out, conf)?;
                    e.to_sql(out, conf)?;
                }
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
                write!(out, "(CASE WHEN ")?;
                cond.to_sql(out, conf)?;
                write!(out, " THEN ")?;
                when_true.to_sql(out, conf)?;
                write!(out, " ELSE ")?;
                when_false.to_sql(out, conf)?;
                write!(out, " END)")
            }
            Expression::Case { branches, r#else } => {
                write!(out, "(CASE")?;
                for branch in branches {
                    write!(out, " WHEN ")?;
                    branch.cond.to_sql(out, conf)?;
                    write!(out, " THEN ")?;
                    branch.then.to_sql(out, conf)?;
                }
                write!(out, " ELSE ")?;
                r#else.to_sql(out, conf)?;
                write!(out, " END) ")
            }
            Expression::BareFunctionCall(name) => write!(out, " {name} "),
        }
    }
}

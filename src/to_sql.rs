use crate::translate::{BinaryOp, COALESCE_DATE_DEFAULT, Expression, FieldType, SQLTree, UnaryOp};
use std::fmt::{Display, Formatter, Result, Write};

pub trait PrinterContext: std::fmt::Debug {
    fn format(
        &self,
        out: &mut Formatter<'_>,
        name: &str,
        field_type: &FieldType,
    ) -> std::fmt::Result;
    /// Write a full binary operator expression: `l <op> r`.
    /// Default behavior prints `l`, the dialect token, then `r`.
    /// Dialects can override to customize behavior (e.g., MSSQL STARTSWITH).
    fn write_operator(
        &self,
        out: &mut Formatter,
        l: &Expression,
        op: &BinaryOp,
        r: &Expression,
        tree: &SQLTree,
        conf: &PrinterConfig,
    ) -> std::fmt::Result {
        write_binary_default(out, l, op, r, tree, conf)
    }
    fn box_clone(&self) -> Box<dyn PrinterContext>;
}

#[inline]
fn write_binary_default(
    out: &mut Formatter,
    l: &Expression,
    op: &BinaryOp,
    r: &Expression,
    tree: &SQLTree,
    conf: &PrinterConfig,
) -> std::fmt::Result {
    l.to_sql(out, tree, conf)?;
    op.to_sql(out, tree, conf)?;
    r.to_sql(out, tree, conf)
}

impl Clone for Box<dyn PrinterContext> {
    fn clone(&self) -> Box<dyn PrinterContext> {
        self.box_clone()
    }
}

struct Quoted<'a>(&'a str);
impl<'a> Display for Quoted<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "\"{}\"", self.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PostgresPrinterContext;

impl PrinterContext for PostgresPrinterContext {
    fn format(
        &self,
        out: &mut Formatter<'_>,
        name: &str,
        field_type: &FieldType,
    ) -> std::fmt::Result {
        let quoted = Quoted(name);
        match field_type {
            FieldType::Character(width) => {
                write!(out, "RPAD(COALESCE({quoted}, ''), {width}, ' ')")
            }
            FieldType::Date => write!(out, "COALESCE({quoted}, DATE '{COALESCE_DATE_DEFAULT}')",),
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Numeric { .. }
                if name != "RECNO5" =>
            {
                //no reason to coalesce RECNO5
                write!(out, "COALESCE({quoted}, 0)")
            }
            FieldType::Logical if name != "__deleted" => {
                //no reason to coalesce __deleted
                write!(out, "COALESCE({quoted}, FALSE)")
            }
            FieldType::Memo => write!(out, "COALESCE({quoted}, '')"),
            _ => quoted.fmt(out),
        }
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
    fn format(
        &self,
        out: &mut Formatter<'_>,
        name: &str,
        field_type: &FieldType,
    ) -> std::fmt::Result {
        let quoted = Quoted(name);
        match field_type {
            FieldType::Character(width) => {
                if self.pad_strings {
                    let spaces = " ".repeat(*width as usize);
                    write!(
                        out,
                        "COALESCE({quoted}, '') || SUBSTR('{spaces}', 1, CASE WHEN {width} - LENGTH(COALESCE({quoted}, '')) > 0 THEN {width} - LENGTH(COALESCE({quoted}, '')) ELSE 0 END)",
                    )
                } else {
                    quoted.fmt(out)
                }
            }
            FieldType::Date => write!(out, "COALESCE({quoted}, DATE('{COALESCE_DATE_DEFAULT}'))",),
            _ => quoted.fmt(out),
        }
    }
    fn box_clone(&self) -> Box<dyn PrinterContext> {
        Box::new(*self) // requires Copy on PostgresPrinterContext
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MssqlPrinterContext;

impl PrinterContext for MssqlPrinterContext {
    fn format(
        &self,
        out: &mut Formatter<'_>,
        name: &str,
        field_type: &FieldType,
    ) -> std::fmt::Result {
        let quoted = Quoted(name);
        match field_type {
            FieldType::Character(width) => {
                write!(
                    out,
                    "LEFT(COALESCE({quoted}, '') + REPLICATE(' ', {width}), {width})",
                )
            }
            FieldType::Date => write!(out, "COALESCE({quoted}, '{COALESCE_DATE_DEFAULT}')"),
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Numeric { .. }
                if name != "RECNO5" =>
            {
                //no reason to coalesce RECNO5
                write!(out, "COALESCE({quoted}, 0)")
            }
            FieldType::Logical => write!(out, "COALESCE({quoted}, FALSE)"),
            FieldType::Memo => write!(out, "COALESCE({quoted}, '')"),
            _ => quoted.fmt(out),
        }
    }
    fn box_clone(&self) -> Box<dyn PrinterContext> {
        Box::new(*self)
    }
    fn write_operator(
        &self,
        out: &mut Formatter,
        l: &Expression,
        op: &BinaryOp,
        r: &Expression,
        tree: &SQLTree,
        conf: &PrinterConfig,
    ) -> std::fmt::Result {
        match op {
            BinaryOp::StartsWith => write_binary_default(out, l, &BinaryOp::Eq, r, tree, conf),
            _ => write_binary_default(out, l, op, r, tree, conf),
        }
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
    fn to_sql(&self, out: &mut Formatter, tree: &SQLTree, conf: &PrinterConfig) -> Result;
}

impl<'field_lookup, 'parse> Display for Printer<SQLTree<'field_lookup, 'parse>> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.tree.is_empty() {
            return Ok(());
        }
        match self.tree.get_root() {
            None => Ok(()),
            Some(root) => root.to_sql(f, &self.tree, &self.config),
        }
    }
}

impl ToSQL for BinaryOp {
    fn to_sql(&self, out: &mut Formatter, _tree: &SQLTree, _: &PrinterConfig) -> Result {
        match self {
            BinaryOp::Add => out.write_str("+"),
            BinaryOp::Sub => out.write_str("-"),
            BinaryOp::Mul => out.write_str("*"),
            BinaryOp::Div => out.write_str("/"),
            BinaryOp::Eq => out.write_str("="),
            BinaryOp::Ne => out.write_str("!="),
            BinaryOp::Lt => out.write_str("<"),
            BinaryOp::Le => out.write_str("<="),
            BinaryOp::Gt => out.write_str(">"),
            BinaryOp::Ge => out.write_str(">="),
            BinaryOp::And => out.write_str(" AND "),
            BinaryOp::Or => out.write_str(" OR "),
            BinaryOp::Concat => out.write_str(" || "),
            BinaryOp::StartsWith => out.write_str(" ^@ "),
            BinaryOp::Between => out.write_str(" BETWEEN "),
            BinaryOp::NotBetween => out.write_str(" NOT BETWEEN "),
        }
    }
}

impl<'field_lookup, 'parse> ToSQL for Expression<'field_lookup, 'parse> {
    fn to_sql(&self, out: &mut Formatter, tree: &SQLTree, conf: &PrinterConfig) -> Result {
        match self {
            Expression::BoolLiteral(v) => out.write_str(if *v { "TRUE" } else { "FALSE" }),
            Expression::NumberLiteral(v) => out.write_str(v),
            Expression::SingleQuoteStringLiteral(v) => write!(out, "'{v}'"),
            Expression::Field { name, field_type } => conf.context.format(out, name, field_type),
            Expression::UnaryOperator(op, exp) => {
                out.write_str("(")?;
                match op {
                    UnaryOp::Not => out.write_str("NOT "),
                    UnaryOp::Neg => out.write_str("-"),
                }?;
                tree.get_expr_unchecked(*exp).to_sql(out, tree, conf)?;
                out.write_str(")")
            }
            Expression::BinaryOperator(l, op, r, p) => {
                p.open(out)?;
                conf.context.write_operator(
                    out,
                    tree.get_expr_unchecked(*l),
                    op,
                    tree.get_expr_unchecked(*r),
                    tree,
                    conf,
                )?;
                p.close(out)
            }
            Expression::BinaryOperatorSequence(op, exprs) => {
                let exprs = tree.get_args(exprs);
                assert!(exprs.len() >= 2);
                out.write_str("(")?;
                tree.get_expr_unchecked(exprs[0]).to_sql(out, tree, conf)?;
                for e in &exprs[1..] {
                    op.to_sql(out, tree, conf)?;
                    tree.get_expr_unchecked(*e).to_sql(out, tree, conf)?;
                }
                out.write_str(")")
            }
            Expression::FunctionCall { name, args } => {
                let args = tree.get_args(args);
                out.write_str(name)?;
                out.write_char('(')?;
                let mut is_first = true;
                for arg in args.iter() {
                    if is_first {
                        is_first = false;
                    } else {
                        out.write_str(",")?;
                    }
                    tree.get_expr_unchecked(*arg).to_sql(out, tree, conf)?;
                }
                out.write_str(")")
            }
            Expression::Cast(expr, to) => {
                out.write_str("CAST (")?;
                tree.get_expr_unchecked(*expr).to_sql(out, tree, conf)?;
                out.write_str(" AS ")?;
                out.write_str(to)?;
                out.write_str(")")
            }
            Expression::Iif {
                cond,
                when_true,
                when_false,
            } => {
                out.write_str("(CASE WHEN ")?;
                tree.get_expr_unchecked(*cond).to_sql(out, tree, conf)?;
                out.write_str(" THEN ")?;
                tree.get_expr_unchecked(*when_true)
                    .to_sql(out, tree, conf)?;
                out.write_str(" ELSE ")?;
                tree.get_expr_unchecked(*when_false)
                    .to_sql(out, tree, conf)?;
                out.write_str(" END)")
            }
            Expression::Case { branches, r#else } => {
                let branches = tree.get_args(branches);
                out.write_str("(CASE")?;
                for branch in branches {
                    let Expression::CaseBranch { cond, then } = tree.get_expr_unchecked(*branch)
                    else {
                        panic!("Incorrectly structured tree: expected a CaseWhen");
                    };
                    out.write_str(" WHEN ")?;
                    tree.get_expr_unchecked(*cond).to_sql(out, tree, conf)?;
                    out.write_str(" THEN ")?;
                    tree.get_expr_unchecked(*then).to_sql(out, tree, conf)?;
                }
                out.write_str(" ELSE ")?;
                tree.get_expr_unchecked(*r#else).to_sql(out, tree, conf)?;
                out.write_str(" END) ")
            }
            Expression::BareFunctionCall(name) => {
                out.write_char(' ')?;
                out.write_str(name)?;
                out.write_char(' ')
            }
            Expression::CaseBranch { .. } => panic!("incorrectly formed tree: exposed CaseBranch"),
        }
    }
}

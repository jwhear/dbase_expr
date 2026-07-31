use crate::translate::{BinaryOp, COALESCE_DATE_DEFAULT, Expression, FieldType, SQLTree, UnaryOp};
use std::fmt::{Display, Formatter, Result};

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

#[derive(Debug, Clone, Copy)]
pub struct PostgresPrinterContext;

impl PrinterContext for PostgresPrinterContext {
    fn format(
        &self,
        out: &mut Formatter<'_>,
        name: &str,
        field_type: &FieldType,
    ) -> std::fmt::Result {
        let quoted = format!("\"{name}\"");
        match field_type {
            FieldType::Character(width) => {
                write!(out, "RPAD(COALESCE({}, ''), {}, ' ')", quoted, width)
            }
            FieldType::Date => write!(
                out,
                "COALESCE({}, DATE '{}')",
                quoted, COALESCE_DATE_DEFAULT
            ),
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Numeric { .. }
                if name != "RECNO5" =>
            {
                //no reason to coalesce RECNO5
                write!(out, "COALESCE({}, 0)", quoted)
            }
            FieldType::Logical if name != "__deleted" => {
                //no reason to coalesce __deleted
                write!(out, "COALESCE({}, FALSE)", quoted)
            }
            FieldType::Memo => write!(out, "COALESCE({}, '')", quoted),
            _ => out.write_str(&quoted),
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
        let quoted = format!("\"{name}\"");
        match field_type {
            FieldType::Character(width) => {
                if self.pad_strings {
                    let spaces = " ".repeat(*width as usize);
                    write!(
                        out,
                        "COALESCE({quoted}, '') || SUBSTR('{spaces}', 1, CASE WHEN {width} - LENGTH(COALESCE({quoted}, '')) > 0 THEN {width} - LENGTH(COALESCE({quoted}, '')) ELSE 0 END)",
                    )
                } else {
                    write!(out, "{}", quoted)
                }
            }
            FieldType::Date => write!(
                out,
                "COALESCE({}, DATE('{}'))",
                quoted, COALESCE_DATE_DEFAULT
            ),
            _ => out.write_str(&quoted),
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
        let quoted = format!("\"{name}\"");
        match field_type {
            FieldType::Character(width) => {
                write!(
                    out,
                    "LEFT(COALESCE({}, '') + REPLICATE(' ', {}), {})",
                    quoted, width, width
                )
            }
            FieldType::Date => write!(out, "COALESCE({}, '{}')", quoted, COALESCE_DATE_DEFAULT),
            FieldType::Double
            | FieldType::Float
            | FieldType::Integer
            | FieldType::Numeric { .. }
                if name != "RECNO5" =>
            {
                //no reason to coalesce RECNO5
                write!(out, "COALESCE({}, 0)", quoted)
            }
            FieldType::Logical => write!(out, "COALESCE({}, FALSE)", quoted),
            FieldType::Memo => write!(out, "COALESCE({}, '')", quoted),
            _ => out.write_str(&quoted),
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

impl<'field_lookup> Display for Printer<SQLTree<'field_lookup>> {
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

impl<'field_lookup> ToSQL for Expression<'field_lookup> {
    fn to_sql(&self, out: &mut Formatter, tree: &SQLTree, conf: &PrinterConfig) -> Result {
        match self {
            Expression::BoolLiteral(v) => {
                write!(out, "{}", if *v { "TRUE" } else { "FALSE" })
            }
            Expression::NumberLiteral(v) => write!(out, "{v}"),
            Expression::SingleQuoteStringLiteral(v) => write!(out, "'{v}'"),
            Expression::Field { name, field_type } => conf.context.format(out, name, field_type),
            Expression::UnaryOperator(op, exp) => {
                write!(out, "(")?;
                match op {
                    UnaryOp::Not => write!(out, "NOT "),
                    UnaryOp::Neg => write!(out, "-"),
                }?;
                tree.get_expr_unchecked(*exp).to_sql(out, tree, conf)?;
                write!(out, ")")
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
                write!(out, "(")?;
                tree.get_expr_unchecked(exprs[0]).to_sql(out, tree, conf)?;
                for e in &exprs[1..] {
                    op.to_sql(out, tree, conf)?;
                    tree.get_expr_unchecked(*e).to_sql(out, tree, conf)?;
                }
                write!(out, ")")
            }
            Expression::FunctionCall { name, args } => {
                let args = tree.get_args(args);
                write!(out, "{name}(")?;
                let mut is_first = true;
                for arg in args.iter() {
                    if is_first {
                        is_first = false;
                    } else {
                        write!(out, ",")?;
                    }
                    tree.get_expr_unchecked(*arg).to_sql(out, tree, conf)?;
                }
                write!(out, ")")
            }
            Expression::Cast(expr, to) => {
                write!(out, "CAST (")?;
                tree.get_expr_unchecked(*expr).to_sql(out, tree, conf)?;
                write!(out, " AS {to}")?;
                write!(out, ")")
            }
            Expression::Iif {
                cond,
                when_true,
                when_false,
            } => {
                write!(out, "(CASE WHEN ")?;
                tree.get_expr_unchecked(*cond).to_sql(out, tree, conf)?;
                write!(out, " THEN ")?;
                tree.get_expr_unchecked(*when_true)
                    .to_sql(out, tree, conf)?;
                write!(out, " ELSE ")?;
                tree.get_expr_unchecked(*when_false)
                    .to_sql(out, tree, conf)?;
                write!(out, " END)")
            }
            Expression::Case { branches, r#else } => {
                let branches = tree.get_args(branches);
                write!(out, "(CASE")?;
                for branch in branches {
                    let Expression::CaseBranch { cond, then } = tree.get_expr_unchecked(*branch)
                    else {
                        panic!("Incorrectly structured tree: expected a CaseWhen");
                    };
                    write!(out, " WHEN ")?;
                    tree.get_expr_unchecked(*cond).to_sql(out, tree, conf)?;
                    write!(out, " THEN ")?;
                    tree.get_expr_unchecked(*then).to_sql(out, tree, conf)?;
                }
                write!(out, " ELSE ")?;
                tree.get_expr_unchecked(*r#else).to_sql(out, tree, conf)?;
                write!(out, " END) ")
            }
            Expression::BareFunctionCall(name) => write!(out, " {name} "),
            Expression::CaseBranch { .. } => panic!("incorrectly formed tree: exposed CaseBranch"),
        }
    }
}

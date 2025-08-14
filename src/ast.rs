use arbitrary::{Arbitrary, Unstructured};

use crate::codebase_functions::CodebaseFunction;

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
    Contain,
    And,
    Or,
    Exp,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConcatOp {
    Add,
    Sub,
}

impl ConcatOp {
    pub fn get_op(&self) -> &BinaryOp {
        match self {
            ConcatOp::Add => &BinaryOp::Add,
            ConcatOp::Sub => &BinaryOp::Sub,
        }
    }
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
    DoubleQuoteStringLiteral(String),
    SingleQuoteStringLiteral(String),
    Field {
        alias: Option<String>,
        name: String,
    },
    FunctionCall {
        name: CodebaseFunction,
        args: Vec<Box<Expression>>,
    },
    BinaryOperator(Box<Expression>, BinaryOp, Box<Expression>),
    UnaryOperator(UnaryOp, Box<Expression>),
    /// Lengthy concatenation is a common pattern `a+b+c+d+chr(34)+...`
    /// These sequences result in a deep tree of BinaryOps that can cause the
    ///  translate and evaluation code to blow out their stacks. We can simplify
    ///  to a Vec.
    Sequence(Vec<Box<Expression>>, ConcatOp),
}

/// Applies simplifications to the AST.
pub fn simplify(expr: Expression) -> Expression {
    let mut expr = expr;
    loop {
        let (new_expr, changed) = simplify_impl(expr);
        if !changed {
            break new_expr;
        }
        expr = new_expr;
    }
}

pub fn simplify_impl(expr: Expression) -> (Expression, bool) {
    match expr {
        // Non-composite types can't be simplified
        Expression::BoolLiteral(_)
        | Expression::NumberLiteral(_)
        | Expression::DoubleQuoteStringLiteral(_)
        | Expression::SingleQuoteStringLiteral(_)
        | Expression::Field { .. } => (expr, false),

        // Attempt to simplify a chain of BinaryOp(Add) to a sequence of operators
        Expression::BinaryOperator(l, BinaryOp::Add, r) => (concat(l, ConcatOp::Add, r), true),

        // Attempt to simplify a chain of BinaryOp(Sub) to a sequence
        Expression::BinaryOperator(l, BinaryOp::Sub, r) => (concat(l, ConcatOp::Sub, r), true),

        // No optimizations below this line: simply recursively simplify operands
        // Recursively simplify operands
        Expression::FunctionCall { name, args } => {
            let mut args_changed = false;
            let mut simple_args = Vec::new();
            for arg in args {
                let (arg, changed) = simplify_impl(*arg);
                simple_args.push(Box::new(arg));
                args_changed |= changed;
            }
            (
                Expression::FunctionCall {
                    name,
                    args: simple_args,
                },
                args_changed,
            )
        }
        Expression::BinaryOperator(left, op, right) => {
            let (left, left_changed) = simplify_impl(*left);
            let (right, right_changed) = simplify_impl(*right);
            (
                Expression::BinaryOperator(Box::new(left), op, Box::new(right)),
                left_changed | right_changed,
            )
        }
        Expression::UnaryOperator(op, right) => {
            let (right, changed) = simplify_impl(*right);
            (Expression::UnaryOperator(op, Box::new(right)), changed)
        }
        // Already simplified
        Expression::Sequence(v, op) => (Expression::Sequence(v, op), false),
    }
}

fn concat(l: Box<Expression>, op: ConcatOp, r: Box<Expression>) -> Expression {
    // Trees nest to the left: a + b + c => ((a + b) + c)
    // We'll build up our vector in a loop like so:
    //  [c]
    //  [c, b]
    //  [c, b, a]
    // Then reverse it at the end. For addition proper it's not a big deal
    //  but '+' is also used for concatenation and order is very important
    //TODO simplify elements?

    let expected_op = op.get_op();
    let mut v = vec![r];
    let mut tree = l;
    while let Expression::BinaryOperator(l, binop, r) = *tree {
        tree = l;
        if &binop != expected_op {
            break;
        }
        v.push(r);
    }
    v.push(tree);
    v.reverse();
    Expression::Sequence(v, op)
}

const MAX_DEPTH: usize = 10;
impl<'a> Arbitrary<'a> for BinaryOp {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(u.choose(&[
            BinaryOp::Add,
            BinaryOp::Sub,
            BinaryOp::Mul,
            BinaryOp::Div,
            BinaryOp::Eq,
            BinaryOp::Ne,
            BinaryOp::Lt,
            BinaryOp::Le,
            BinaryOp::Gt,
            BinaryOp::Ge,
            BinaryOp::Contain,
            BinaryOp::And,
            BinaryOp::Or,
            BinaryOp::Exp,
        ])?
        .clone())
    }
}

impl<'a> Arbitrary<'a> for ConcatOp {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(u.choose(&[ConcatOp::Add, ConcatOp::Sub])?.clone())
    }
}

impl<'a> Arbitrary<'a> for UnaryOp {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(u.choose(&[UnaryOp::Not, UnaryOp::Neg])?.clone())
    }
}

impl<'a> Arbitrary<'a> for Expression {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        arbitrary_expr(u, 0)
    }
}

fn arbitrary_expr(u: &mut Unstructured<'_>, depth: usize) -> arbitrary::Result<Expression> {
    if depth >= MAX_DEPTH {
        // Force leaf nodes at max depth
        let bool_literal = Expression::BoolLiteral(bool::arbitrary(u)?);
        let num_literal = Expression::NumberLiteral(String::arbitrary(u)?);
        let dq_literal = Expression::DoubleQuoteStringLiteral(String::arbitrary(u)?);
        let sq_literal = Expression::SingleQuoteStringLiteral(String::arbitrary(u)?);
        let literal_types = [bool_literal, num_literal, dq_literal, sq_literal];
        return Ok(u.choose(&literal_types)?.clone());
    }

    // Weighted choice: more leaves than branches
    match u.int_in_range::<u8>(0..=6)? {
        0 => Ok(Expression::BoolLiteral(bool::arbitrary(u)?)),
        1 => Ok(Expression::NumberLiteral(String::arbitrary(u)?)),
        2 => Ok(Expression::DoubleQuoteStringLiteral(String::arbitrary(u)?)),
        3 => Ok(Expression::SingleQuoteStringLiteral(String::arbitrary(u)?)),
        4 => Ok(Expression::Field {
            alias: Option::<String>::arbitrary(u)?,
            name: String::arbitrary(u)?,
        }),
        5 => Ok(Expression::BinaryOperator(
            Box::new(arbitrary_expr(u, depth + 1)?),
            BinaryOp::arbitrary(u)?,
            Box::new(arbitrary_expr(u, depth + 1)?),
        )),
        6 => Ok(Expression::UnaryOperator(
            UnaryOp::arbitrary(u)?,
            Box::new(arbitrary_expr(u, depth + 1)?),
        )),
        _ => unreachable!(),
    }
}

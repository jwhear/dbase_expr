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
    AddSequence(Vec<Box<Expression>>),
}

/// Applies simplifications to the AST.
pub fn simplify(expr: Expression) -> Expression {
    if let Expression::BinaryOperator(l, BinaryOp::Add, r) = expr {
        // Trees nest to the left: a + b + c => ((a + b) + c)
        // We'll build up our vector in a loop like so:
        //  [c]
        //  [c, b]
        //  [c, b, a]
        // Then reverse it at the end. For addition proper it's not a big deal
        //  but '+' is also used for concatenation and order is very important
        let mut v = vec![r];
        let mut tree = l;
        while let Expression::BinaryOperator(l, BinaryOp::Add, r) = *tree {
            v.push(r);
            tree = l;
        }
        v.push(tree);
        v.reverse();
        Expression::AddSequence(v)
    } else {
        // Leave expression untouched
        expr
    }
}

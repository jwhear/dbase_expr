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
}

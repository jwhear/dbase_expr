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

pub fn apply_string_escapes(s: &str) -> String {
    let mut res = String::new();
    let mut is_escaped = false;
    for c in s.chars() {
        if c == '\\' && !is_escaped {
            is_escaped = true;
        } else {
            res.push(c);
            is_escaped = false;
        }
    }
    res
}

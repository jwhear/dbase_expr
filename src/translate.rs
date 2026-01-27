use std::{cell::RefCell, fmt::Formatter, rc::Rc};

use crate::{
    codebase_functions::CodebaseFunction,
    parser::{self, ParseTree},
};

//pub mod mssql; // Not fully ported to ParseTree revision
pub mod postgres;
pub mod sqlite;

pub const COALESCE_DATE: &str = "0001-01-01";

pub type ExprRef = Rc<RefCell<Expression>>;

pub fn expr_ref(expr: Expression) -> ExprRef {
    Rc::new(RefCell::new(expr))
}

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
    NotBetween,
    Between,
    StartsWith,
    And,
    Or,
    Concat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum Parenthesize {
    #[default]
    Yes,
    No,
}

impl Parenthesize {
    //this is dumb but I like it
    pub fn open(&self, out: &mut Formatter) -> std::result::Result<(), std::fmt::Error> {
        self.write(out, "(")
    }
    pub fn close(&self, out: &mut Formatter) -> std::result::Result<(), std::fmt::Error> {
        self.write(out, ")")
    }
    fn write(&self, out: &mut Formatter, str: &str) -> std::result::Result<(), std::fmt::Error> {
        if self == &Parenthesize::Yes {
            write!(out, "{}", str)
        } else {
            Ok(())
        }
    }
}

/// A WHEN branch for CASE
#[derive(Debug, PartialEq, Eq)]
pub struct When {
    pub cond: ExprRef,
    pub then: ExprRef,
}

/// This is the output type of translation: a Codebase AST goes in, a SQL AST
///  comes out.
#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    BoolLiteral(bool),
    NumberLiteral(String),
    SingleQuoteStringLiteral(String),
    Field {
        name: String,
        field_type: FieldType,
    },
    FunctionCall {
        name: String,
        args: Vec<ExprRef>,
    },
    BinaryOperator(ExprRef, BinaryOp, ExprRef, Parenthesize),
    // This is an optimization of BinaryOperator for things like:
    //   a + b + c + d
    // OR
    //   a || b || c
    BinaryOperatorSequence(BinaryOp, Vec<ExprRef>),
    UnaryOperator(UnaryOp, ExprRef),
    Cast(ExprRef, &'static str),
    Iif {
        cond: ExprRef,
        when_true: ExprRef,
        when_false: ExprRef,
    },
    Case {
        branches: Vec<When>,
        r#else: ExprRef,
    },
    // used for things like "CURRENT_DATE" which are functions but don't
    //  allow the parentheses.
    BareFunctionCall(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    UnsupportedFunction(String),
    IncorrectArgCount(String, usize),
    ArgWrongType {
        func_name: String,
        wrong_arg_index: usize,
    },
    Other(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnsupportedFunction(name) => write!(f, "Unsupported function: {name}"),
            Self::IncorrectArgCount(name, count) => write!(
                f,
                "Function {name} called with an incorrect number of arguments (got {count})"
            ),
            Self::ArgWrongType {
                func_name,
                wrong_arg_index,
            } => write!(
                f,
                "Function {func_name}: argument {wrong_arg_index} is the wrong type",
            ),
            Self::Other(msg) => write!(f, "Error: {msg}"),
        }
    }
}

impl std::error::Error for Error {}

// These From implementations help the translation implementation
impl From<&str> for Expression {
    fn from(s: &str) -> Self {
        Expression::SingleQuoteStringLiteral(s.to_string())
    }
}
impl From<String> for Expression {
    fn from(s: String) -> Self {
        Expression::SingleQuoteStringLiteral(s)
    }
}
impl From<i64> for Expression {
    fn from(s: i64) -> Self {
        Expression::NumberLiteral(s.to_string())
    }
}
pub type Result = std::result::Result<(ExprRef, FieldType), Error>;

fn ok(exp: Expression, ty: FieldType) -> Result {
    Ok((expr_ref(exp), ty))
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[repr(u8)]
pub enum FieldType {
    //Binary = b'B',
    Character(u32) = b'C',
    CharacterBinary(u32) = b'Z',
    Currency = b'Y',
    DateTime = b'T',
    Date = b'D',
    Double = b'B',
    Float = b'F',
    General = b'G',
    Integer = b'I',
    Logical = b'L',
    Memo = b'M',
    MemoBinary = b'X',
    Numeric { len: u32, dec: u32 } = b'N',
    //Unicode = b'U',
}

impl FieldType {
    pub fn fixed_len(&self) -> Option<u32> {
        match self {
            Self::Character(len) | Self::CharacterBinary(len) | Self::Numeric { len, .. } => {
                Some(*len)
            }
            _ => None,
        }
    }
}

/// This trait allows the caller to control translation. When implementing a new
///  translation target, a reasonable strategy is to delegate to the Postgres
///  translator but intercept anything that needs to be handled differently:
///
/// ```rust
/// # use dbase_expr::{ast, translate::{self, Expression, FieldType, TranslationContext, Error, ExprRef}, codebase_functions::CodebaseFunction,};
/// struct MyCustomTranslator
/// {
///     my_state: std::collections::HashMap<String, FieldType>,
/// }
///
/// impl TranslationContext for MyCustomTranslator
/// {
///     fn lookup_field(
///         &self,
///         alias: Option<&str>,
///         field: &str,
///     ) -> std::result::Result<(String, FieldType), String> {
///         let norm = field.to_uppercase();
///         self.my_state.get(&norm)
///             .map(|t| (norm, *t))
///             .ok_or(format!("No field named {field}"))
///     }
///
///     fn translate(&self, source: &parser::Expression, tree: &parser::ParseTree) -> std::result::Result<(ExprRef, FieldType), Error> {
///         // This is the place to handle specific cases which are different from Postgres,
///         //  including cases which should be errors
///
///         // Everything else can be delegated:
///         translate::postgres::translate(source, self)
///     }
///     
///     fn translate_fn_call(
///         &self,
///         name: &CodebaseFunction,
///         args: &[parser::ExpressionId],
///         tree: &ParseTree,
///     ) -> std::result::Result<(ExprRef, FieldType), Error> {
///         // Use a similar pattern here: most function calls probably resolve to the
///         //  same thing that Postgres uses but handle the differences here
///
///         // and delegate the rest...
///         translate::postgres::translate_fn_call(name, args, self, tree)
///     }
///
///     fn translate_binary_op(
///        &self,
///        l: &parser::Expression,
///        op: &parser::BinaryOp,
///        r: &parser::Expression,
///        tree: &parser::ParseTree,
///     ) -> std::result::Result<(ExprRef, FieldType), Error> {
///         translate::postgres::translate_binary_op(self, l, op, r, tree)
///     }
/// }
///
/// ```
pub trait TranslationContext {
    /// Called to determine the proper name and type of a field.
    ///   `alias`: the table reference if the field is qualified (in `foo.x` the alias is `foo`)
    ///   `field`: the name of the field from the expression
    ///
    /// On success, returns a tuple of the proper (e.g. capitalized) name and the field type
    fn lookup_field(
        &self,
        alias: Option<&str>,
        field: &str,
    ) -> std::result::Result<(String, FieldType), String>;

    /// Called to translate an expression generally.
    fn translate(&self, source: &parser::Expression, tree: &ParseTree) -> Result;

    /// Called to translate a function call.
    ///   `name`: the name of the function in the original expression
    ///   `args`: the arguments to the function
    ///
    /// On success, returns an expression and the type the expression would return.
    fn translate_fn_call(
        &self,
        name: &CodebaseFunction,
        args: &[parser::ExpressionId],
        tree: &ParseTree,
    ) -> std::result::Result<(ExprRef, FieldType), Error>;

    /// Called to translate a binary operator expression.
    fn translate_binary_op(
        &self,
        l: &parser::Expression,
        op: &parser::BinaryOp,
        r: &parser::Expression,
        tree: &ParseTree,
    ) -> Result;

    /// Truncate the right side of a string comparison to a fixed length.
    fn string_comp_right(&self, r: ExprRef, len: u32) -> ExprRef {
        expr_ref(Expression::FunctionCall {
            name: "SUBSTR".into(),
            args: vec![
                r,
                expr_ref(Expression::NumberLiteral("1".into())),
                expr_ref(Expression::NumberLiteral(len.to_string())),
            ],
        })
    }

    /// The left side of the string comparison should be truncated to the length of the right side (basically a startswith compare)
    fn string_comp_left(&self, l: ExprRef, r: ExprRef) -> ExprRef {
        let right_side_len_expression = expr_ref(Expression::FunctionCall {
            name: "LENGTH".into(),
            args: vec![r],
        });
        expr_ref(Expression::FunctionCall {
            name: "SUBSTR".into(),
            args: vec![
                l,
                expr_ref(Expression::NumberLiteral("1".into())),
                right_side_len_expression,
            ],
        })
    }
}

//NOTE(justin): This function almost certainly has a bug hiding in it.
fn escape_single_quotes(s: &str) -> String {
    let mut res = String::new();
    res.reserve(s.len());

    let mut is_escaped = false;
    for c in s.chars() {
        is_escaped = c == '\\' && !is_escaped;

        if c == '\'' && !is_escaped {
            res.push('\\');
        }

        res.push(c);
    }
    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_escape_single_quotes() {
        assert_eq!("foo", escape_single_quotes("foo"));
        assert_eq!(r"\'", escape_single_quotes(r"'"));
        assert_eq!(r"\\'", escape_single_quotes(r"\'"));
    }
}

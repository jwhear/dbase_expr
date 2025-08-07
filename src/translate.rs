use std::fmt::Formatter;

use crate::{ast, codebase_functions::CodebaseFunction};

pub mod postgres;
pub mod sqlite;

pub const COALESCE_DATE: &str = "0001-01-01";

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

/// This is the output type of translation: a Codebase AST goes in, a SQL AST
///  comes out.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    BoolLiteral(bool),
    NumberLiteral(String),
    SingleQuoteStringLiteral(String),
    Field {
        alias: Option<String>,
        name: String,
        field_type: FieldType,
    },
    FunctionCall {
        name: String,
        args: Vec<Box<Expression>>,
    },
    BinaryOperator(Box<Expression>, BinaryOp, Box<Expression>, Parenthesize),
    UnaryOperator(UnaryOp, Box<Expression>),
    Cast(Box<Expression>, &'static str),
    Iif {
        cond: Box<Expression>,
        when_true: Box<Expression>,
        when_false: Box<Expression>,
    },
    // used for things like "CURRENT_DATE" which are functions but don't
    //  allow the parentheses.
    BareFunctionCall(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    UnsupportedFunction(String),
    IncorrectArgCount(String, usize),
    ArgWrongType {
        func: ast::Expression,
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
                func,
                wrong_arg_index,
            } => write!(
                f,
                "Function {func:?}: argument {wrong_arg_index} is the wrong type",
            ),
            Self::Other(msg) => write!(f, "Error: {msg}"),
        }
    }
}

impl std::error::Error for Error {}

// These From implementations help the translation implementation
impl From<&str> for Box<Expression> {
    fn from(s: &str) -> Self {
        Box::new(Expression::SingleQuoteStringLiteral(s.to_string()))
    }
}
impl From<String> for Box<Expression> {
    fn from(s: String) -> Self {
        Box::new(Expression::SingleQuoteStringLiteral(s))
    }
}
impl From<i64> for Box<Expression> {
    fn from(s: i64) -> Self {
        Box::new(Expression::NumberLiteral(s.to_string()))
    }
}
pub type Result = std::result::Result<(Box<Expression>, FieldType), Error>;

fn ok(exp: Expression, ty: FieldType) -> Result {
    Ok((Box::new(exp), ty))
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
/// # use dbase_expr::{ast, translate::{self, Expression, FieldType, TranslationContext, Error}, codebase_functions::CodebaseFunction,};
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
///     fn translate(&self, source: &ast::Expression) -> std::result::Result<(Box<Expression>, FieldType), Error> {
///         //TODO handle specific cases which are different from Postgres,
///         // including cases which should be errors
///
///         // Everything else can be delegated:
///         translate::postgres::translate(source, self)
///     }
///     
///     fn translate_fn_call(
///         &self,
///         name: &CodebaseFunction,
///         args: &[Box<ast::Expression>],
///     ) -> std::result::Result<(Box<Expression>, FieldType), Error> {
///         //TODO similar pattern: most function calls probably resolve to the
///         // same thing that Postgres uses but handle the differences here
///
///         // and delegate the rest...
///         translate::postgres::translate_fn_call(name, args, self)
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
    fn translate(&self, source: &ast::Expression) -> Result;

    /// Called to translate a function call.
    ///   `name`: the name of the function in the original expression
    ///   `args`: the arguments to the function
    ///
    /// On success, returns an expression and the type the expression would return.
    fn translate_fn_call(
        &self,
        name: &CodebaseFunction,
        args: &[Box<ast::Expression>],
    ) -> std::result::Result<(Box<Expression>, FieldType), Error>;

    fn translate_binary_op(
        &self,
        l: &Box<ast::Expression>,
        op: &ast::BinaryOp,
        r: &Box<ast::Expression>,
    ) -> Result;
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

// The left side of the string comparison should be truncated to the length of the right side (basically a startswith compare)
pub fn string_comp_left(l: Box<Expression>, r: Box<Expression>) -> Box<Expression> {
    let right_side_len_expression = Box::new(Expression::FunctionCall {
        name: "LENGTH".into(),
        args: vec![r],
    });
    Box::new(Expression::FunctionCall {
        name: "SUBSTR".into(),
        args: vec![
            l,
            Box::new(Expression::NumberLiteral("1".into())),
            right_side_len_expression,
        ],
    })
}

// The right side of the string comparison should be truncated to the fixed length, no need to evaluate additional characters
pub fn string_comp_right(r: Box<ast::Expression>, len: u32) -> Box<ast::Expression> {
    Box::new(ast::Expression::FunctionCall {
        name: CodebaseFunction::SUBSTR,
        args: vec![
            r,
            Box::new(ast::Expression::NumberLiteral("1".into())),
            Box::new(ast::Expression::NumberLiteral(len.to_string())),
        ],
    })
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

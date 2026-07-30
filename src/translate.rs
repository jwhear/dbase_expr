use std::fmt::Formatter;

use crate::{
    codebase_functions::CodebaseFunction,
    expression_tree::{ArgList, ExpressionId, ExpressionTree, Index},
    parser::{self, ParseTree},
};

//pub mod mssql; // Not fully ported to ParseTree revision
pub mod postgres;
pub mod sqlite;

pub const COALESCE_DATE_DEFAULT: &str = "0001-01-01";

pub mod exps {
    use super::{ExpressionId, Index};
    macro_rules! prelude {
        ($name:ident, $index:literal) => {
            pub const $name: ExpressionId = ExpressionId(Index($index));
        };
    }

    prelude!(LIT_0, 0);
    prelude!(LIT_1, 1);
    prelude!(EMPTY_STR, 2);
    prelude!(COALESCE_DATE, 3);
    prelude!(LIT_YEAR, 4);
    prelude!(LIT_MONTH, 5);
    prelude!(LIT_DAY, 6);
    prelude!(LIT_FALSE, 7);
    prelude!(LIT_TRUE, 8);
    prelude!(LIT_SPACE, 9);
}

pub struct SQLTree {
    pub inner: ExpressionTree<Expression>,
    pub prelude_length: usize,
}

impl SQLTree {
    pub fn new() -> Self {
        let mut inner = ExpressionTree::new();

        // LIT_0
        inner.push_expr(Expression::NumberLiteral("0".into()));
        // LIT_1
        inner.push_expr(Expression::NumberLiteral("1".into()));
        // EMPTY_STR
        inner.push_expr(Expression::SingleQuoteStringLiteral("".into()));
        // COALESCE_DATE
        inner.push_expr(Expression::SingleQuoteStringLiteral(
            COALESCE_DATE_DEFAULT.into(),
        ));
        // LIT_YEAR
        inner.push_expr(Expression::SingleQuoteStringLiteral("YEAR".into()));
        // LIT_MONTH
        inner.push_expr(Expression::SingleQuoteStringLiteral("MONTH".into()));
        // LIT_DAY
        inner.push_expr(Expression::SingleQuoteStringLiteral("DAY".into()));
        // LIT_FALS
        inner.push_expr(Expression::BoolLiteral(false));
        // LIT_TRUE
        inner.push_expr(Expression::BoolLiteral(true));
        // LIT_SPACE
        inner.push_expr(Expression::SingleQuoteStringLiteral(" ".into()));

        let prelude_length = inner.expressions.len();

        Self {
            inner,
            prelude_length,
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        // We ignore the prelude for the purposes of is_empty
        self.inner.expressions.len() <= self.prelude_length
    }

    #[inline]
    pub fn get_root(&self) -> Option<&Expression> {
        // We ignore the prelude for the purposes of get_root
        if self.is_empty() {
            None
        } else {
            self.inner.get_root()
        }
    }

    /// Get the expression with [id]. This panics if [id] doesn't reference an
    ///  expression pushed to this tree.
    #[inline]
    pub fn get_expr_unchecked(&self, id: ExpressionId) -> &Expression {
        self.inner.get_expr_unchecked(id)
    }

    #[inline]
    pub fn get_expr(&self, id: ExpressionId) -> Option<&Expression> {
        self.inner.get_expr(id)
    }

    #[inline]
    pub fn get_args(&self, list: &ArgList) -> &[ExpressionId] {
        self.inner.get_args(list)
    }

    #[inline]
    pub fn push_expr(&mut self, expr: Expression) -> ExpressionId {
        self.inner.push_expr(expr)
    }

    #[inline]
    pub fn push_args(&mut self, ids: impl ExactSizeIterator<Item = ExpressionId>) -> ArgList {
        self.inner.push_args(ids)
    }

    pub fn push_fn_call(&mut self, name: &'static str, args: &[ExpressionId]) -> ExpressionId {
        let args = self.push_args(args.iter().copied());
        self.push_expr(Expression::FunctionCall { name, args })
    }
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

/// This is the output type of translation: a Codebase AST goes in, a SQL AST
///  comes out.
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    BoolLiteral(bool),
    NumberLiteral(String),
    SingleQuoteStringLiteral(String),
    Field {
        name: String,
        field_type: FieldType,
    },
    FunctionCall {
        name: &'static str,
        args: ArgList,
    },
    BinaryOperator(ExpressionId, BinaryOp, ExpressionId, Parenthesize),
    // This is an optimization of BinaryOperator for things like:
    //   a + b + c + d
    // OR
    //   a || b || c
    BinaryOperatorSequence(BinaryOp, ArgList),
    UnaryOperator(UnaryOp, ExpressionId),
    Cast(ExpressionId, &'static str),
    Iif {
        cond: ExpressionId,
        when_true: ExpressionId,
        when_false: ExpressionId,
    },
    Case {
        branches: ArgList,
        r#else: ExpressionId,
    },
    // Only used in Case.branches
    CaseBranch {
        cond: ExpressionId,
        then: ExpressionId,
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
    InvalidField(String, String), // field name, error
    EmptyTree,
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
            Self::InvalidField(field, error) => write!(f, "Invalid Field Name ({field}): {error}"),
            Self::EmptyTree => write!(f, "The input ParseTree was empty"),
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
pub type TreeResult = std::result::Result<(SQLTree, FieldType), Error>;
pub type ExpResult = std::result::Result<(Expression, FieldType), Error>;

fn ok(exp: Expression, ty: FieldType) -> ExpResult {
    Ok((exp, ty))
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
/// # use dbase_expr::{parser, translate::{self, Expression, FieldType, TranslationContext, Error, SQLTree, ExpResult }, codebase_functions::CodebaseFunction,};
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
///     fn translate_expr(
///         &self,
///         source: &parser::Expression,
///         src_tree: &parser::ParseTree,
///         dst_tree: &mut SQLTree
///     ) -> ExpResult {
///         // This is the place to handle specific cases which are different from Postgres,
///         //  including cases which should be errors
///
///         // Everything else can be delegated:
///         translate::postgres::translate_expr(source, src_tree, dst_tree, self)
///     }
///     
///     fn translate_fn_call(
///         &self,
///         name: &CodebaseFunction,
///         args: &[parser::ExpressionId],
///         src_tree: &parser::ParseTree,
///         dst_tree: &mut SQLTree
///     ) -> ExpResult {
///         // Use a similar pattern here: most function calls probably resolve to the
///         //  same thing that Postgres uses but handle the differences here
///
///         // and delegate the rest...
///         translate::postgres::translate_fn_call(name, args, src_tree, dst_tree, self)
///     }
///
///     fn translate_binary_op(
///         &self,
///         l: &parser::Expression,
///         op: &parser::BinaryOp,
///         r: &parser::Expression,
///         src_tree: &parser::ParseTree,
///         dst_tree: &mut SQLTree,
///     ) -> ExpResult {
///         translate::postgres::translate_binary_op(self, l, op, r, src_tree, dst_tree)
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

    /// Called to translate a [ParseTree].
    fn translate(&self, tree: &ParseTree) -> TreeResult {
        let root = tree.get_root().ok_or(Error::EmptyTree)?;
        let mut out_tree = SQLTree::new();
        let (root, root_type) = self.translate_expr(root, tree, &mut out_tree)?;
        out_tree.push_expr(root);
        Ok((out_tree, root_type))
    }

    /// Called to translate a specific expression within a [ParseTree].
    fn translate_expr(
        &self,
        source: &parser::Expression,
        in_tree: &ParseTree,
        out_tree: &mut SQLTree,
    ) -> ExpResult;

    /// Called to translate a function call.
    ///   `name`: the name of the function in the original expression
    ///   `args`: the arguments to the function
    ///
    /// On success, returns an expression and the type the expression would return.
    fn translate_fn_call(
        &self,
        name: &CodebaseFunction,
        args: &[parser::ExpressionId],
        in_tree: &ParseTree,
        out_tree: &mut SQLTree,
    ) -> ExpResult;

    /// Called to translate a binary operator expression.
    fn translate_binary_op(
        &self,
        l: &parser::Expression,
        op: &parser::BinaryOp,
        r: &parser::Expression,
        in_tree: &ParseTree,
        out_tree: &mut SQLTree,
    ) -> ExpResult;

    /// Truncate the right side of a string comparison to a fixed length.
    fn string_comp_right(&self, r: ExpressionId, len: u32, out_tree: &mut SQLTree) -> Expression {
        //TODO lit_1 and possibly other nodes are going to be the same. These
        // could be cached and the ExpressionIds reused in other parts of the tree
        let lit_1 = out_tree.push_expr(Expression::NumberLiteral("1".into()));
        let lit_len = out_tree.push_expr(Expression::NumberLiteral(len.to_string()));
        let args = out_tree.push_args([r, lit_1, lit_len].into_iter());
        Expression::FunctionCall {
            name: "SUBSTR".into(),
            args,
        }
    }

    /// The left side of the string comparison should be truncated to the length of the right side (basically a startswith compare)
    /// The output will look like `SUBSTR(l, 1, LENGTH(r))`
    fn string_comp_left(
        &self,
        l: ExpressionId,
        r: ExpressionId,
        out_tree: &mut SQLTree,
    ) -> Expression {
        // First prep a LENGTH(r) call
        let args = out_tree.push_args([r].into_iter());
        let right_side_len = out_tree.push_expr(Expression::FunctionCall {
            name: "LENGTH".into(),
            args: args,
        });

        let lit_1 = out_tree.push_expr(Expression::NumberLiteral("1".into()));
        let args = out_tree.push_args([l, lit_1, right_side_len].into_iter());
        Expression::FunctionCall {
            name: "SUBSTR".into(),
            args,
        }
    }
}

fn escape_single_quotes(s: &str) -> String {
    let mut res = String::new();
    res.reserve(s.len());
    for c in s.chars() {
        if c == '\'' {
            res.push('\''); // Add an extra quote to escape it
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
        //'->''
        assert_eq!(r"''", escape_single_quotes(r"'"));
        //\'->\'''
        assert_eq!(r"\''", escape_single_quotes(r"\'"));
    }
}

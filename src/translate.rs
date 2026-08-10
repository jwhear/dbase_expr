use std::{borrow::Cow, fmt::Formatter};

use crate::{
    codebase_functions::CodebaseFunction,
    expression_tree::{ArgList, ExpressionId, ExpressionTree, Index},
    parser::{self, ParseTree},
};

//pub mod mssql; // Not fully ported to ParseTree revision
pub mod postgres;
pub mod sqlite;

pub const COALESCE_DATE_DEFAULT: &str = "0001-01-01";

/// This module defines a number of commonly used tree elements
///  (e.g. literal false) and defines a function to inject them as a "prelude"
///  in a tree.
pub mod exps {
    use super::{COALESCE_DATE_DEFAULT, Expression, ExpressionId, ExpressionTree, Index};

    // I used a macro here to ensure that the constants and the order of push_expr
    //  is guaranteed to be consistent.
    macro_rules! inject_prelude_gen {
        ( $( ($name:ident, $expr:expr) ),* $(,)? ) => {
            inject_prelude_gen!(@count 0; $( ($name, $expr) ),*);

            pub fn inject_prelude(tree: &mut ExpressionTree<Expression>) -> usize {
                $( tree.push_expr($expr); )*
                tree.expressions.len()
            }
        };

        // Peel off one (name, expr) pair, emit its const, recurse with idx+1
        (@count $idx:expr; ($name:ident, $expr:expr) $(, ($rest_name:ident, $rest_expr:expr))* ) => {
            pub const $name: ExpressionId = ExpressionId(Index($idx));
            inject_prelude_gen!(@count ($idx + 1); $( ($rest_name, $rest_expr) ),*);
        };

        // Base case: nothing left
        (@count $idx:expr; ) => {};
    }

    inject_prelude_gen!(
        (LIT_0, Expression::NumberLiteral("0".into())),
        (LIT_1, Expression::NumberLiteral("1".into())),
        (EMPTY_STR, Expression::SingleQuoteStringLiteral("".into())),
        (
            COALESCE_DATE,
            Expression::SingleQuoteStringLiteral(COALESCE_DATE_DEFAULT.into())
        ),
        (
            LIT_YEAR,
            Expression::SingleQuoteStringLiteral("YEAR".into())
        ),
        (
            LIT_MONTH,
            Expression::SingleQuoteStringLiteral("MONTH".into())
        ),
        (LIT_DAY, Expression::SingleQuoteStringLiteral("DAY".into())),
        (LIT_FALSE, Expression::BoolLiteral(false)),
        (LIT_TRUE, Expression::BoolLiteral(true)),
        (LIT_SPACE, Expression::SingleQuoteStringLiteral(" ".into())),
        (LIT_DASH, Expression::SingleQuoteStringLiteral("-".into())),
    );
}

pub struct SQLTree<'field_lookup, 'parse> {
    pub inner: ExpressionTree<Expression<'field_lookup, 'parse>>,
    pub prelude_length: usize,
}

impl<'field_lookup, 'parse> SQLTree<'field_lookup, 'parse> {
    pub fn new() -> Self {
        // The default capacities in ExpressionTree are better suited to parsing.
        // We usually need more space.
        let expressions = Vec::with_capacity(1024);
        let arg_lists = Vec::with_capacity(128);
        Self::new_from_vecs(expressions, arg_lists)
    }

    /// Create using previously allocated Vecs.
    pub fn new_from_vecs(
        expressions: Vec<Expression<'field_lookup, 'parse>>,
        arg_lists: Vec<ExpressionId>,
    ) -> Self {
        let mut inner = ExpressionTree::new_from_vecs(expressions, arg_lists);
        let prelude_length = exps::inject_prelude(&mut inner);
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
    pub fn get_root(&self) -> Option<&Expression<'field_lookup, 'parse>> {
        // We ignore the prelude for the purposes of get_root
        if self.is_empty() {
            None
        } else {
            self.inner.get_root()
        }
    }

    /// See [ExpressionTree::get_expr_unchecked]
    #[inline]
    pub fn get_expr_unchecked(&self, id: ExpressionId) -> &Expression<'field_lookup, 'parse> {
        self.inner.get_expr_unchecked(id)
    }

    /// See [ExpressionTree::get_expr]
    #[inline]
    pub fn get_expr(&self, id: ExpressionId) -> Option<&Expression<'field_lookup, 'parse>> {
        self.inner.get_expr(id)
    }

    /// See [ExpressionTree::get_args]
    #[inline]
    pub fn get_args(&self, list: &ArgList) -> &[ExpressionId] {
        self.inner.get_args(list)
    }

    /// See [ExpressionTree::push_expr].
    ///
    /// Note that this implementation also performs a cache check: if `expr`
    ///  already exists in this tree, the existing ID is returned.
    #[inline]
    pub fn push_expr(&mut self, expr: Expression<'field_lookup, 'parse>) -> ExpressionId {
        // We can ensure prevent unnecessarily large trees by maintaining a cache
        //  of expressions that we've already seen: if an expression is already in
        //  the tree we can just return its ID.
        // I experimented with HashMap but it took the benchmark from
        //  ~17microsecs to ~27. However: most trees are pretty small and linear
        //  scans are fast, so we can use inner.expressions itself as our cache!
        // In my benchmark, this adds about ~1microsec but makes translation
        //  simpler as that code no longer has to worry about whether an argument
        //  has already been translated or not.
        self.inner
            .expressions
            .iter()
            .position(|e| e == &expr)
            .map(|index| index.into())
            .unwrap_or_else(|| self.inner.push_expr(expr))
    }

    /// See [ExpressionTree::push_args]
    #[inline]
    pub fn push_args(&mut self, ids: impl ExactSizeIterator<Item = ExpressionId>) -> ArgList {
        self.inner.push_args(ids)
    }

    /// Pushes an [Expression::FunctionCall] to the the tree, with the provided
    ///  `name` and `args`. Returns the resulting [ExpressionId]
    pub fn push_fn_call(&mut self, name: &'static str, args: &[ExpressionId]) -> ExpressionId {
        let name = name.into();
        let args = self.push_args(args.iter().copied());
        self.push_expr(Expression::FunctionCall { name, args })
    }
}

impl<'field_lookup, 'parse> Default for SQLTree<'field_lookup, 'parse> {
    fn default() -> Self {
        Self::new()
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
pub struct UnsupportedOperator;

/// Map operators directly across where possible. `Contain` and `Exp` are not
///  directly supported and will result in an error.
impl TryFrom<&parser::BinaryOp> for BinaryOp {
    type Error = UnsupportedOperator;
    fn try_from(value: &parser::BinaryOp) -> Result<Self, Self::Error> {
        use parser::BinaryOp as BO;
        match value {
            BO::Add => Ok(Self::Add),
            BO::Sub => Ok(Self::Sub),
            BO::Mul => Ok(Self::Mul),
            BO::Div => Ok(Self::Div),
            BO::Eq => Ok(Self::Eq),
            BO::Ne => Ok(Self::Ne),
            BO::Lt => Ok(Self::Lt),
            BO::Le => Ok(Self::Le),
            BO::Gt => Ok(Self::Gt),
            BO::Ge => Ok(Self::Ge),
            BO::And => Ok(Self::And),
            BO::Or => Ok(Self::Or),
            BO::Contain | BO::Exp => Err(UnsupportedOperator),
        }
    }
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
pub enum Expression<'field_lookup, 'parse> {
    BoolLiteral(bool),
    NumberLiteral(Cow<'parse, str>),
    SingleQuoteStringLiteral(Cow<'parse, str>),
    Field {
        name: Cow<'field_lookup, str>,
        field_type: FieldType,
    },
    FunctionCall {
        name: Cow<'static, str>,
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
    BareFunctionCall(&'static str),
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
impl<'field_lookup, 'parse> From<&'parse str> for Expression<'field_lookup, 'parse> {
    fn from(s: &'parse str) -> Self {
        Expression::SingleQuoteStringLiteral(escape_single_quotes(s))
    }
}
impl<'field_lookup, 'parse> From<String> for Expression<'field_lookup, 'parse> {
    fn from(s: String) -> Self {
        let escaped = escape_single_quotes(&s).into_owned();
        Expression::SingleQuoteStringLiteral(Cow::from(escaped))
    }
}
impl<'field_lookup, 'parse> From<i64> for Expression<'field_lookup, 'parse> {
    fn from(s: i64) -> Self {
        Expression::NumberLiteral(Cow::from(s.to_string()))
    }
}

/// The result of a translate call is a SQLTree and the type of the root expression.
pub type TreeResult<'field_lookup, 'parse> =
    std::result::Result<(SQLTree<'field_lookup, 'parse>, FieldType), Error>;

/// The result of a translate_expr call is an Expression and its type
pub type ExpResult<'field_lookup, 'parse> =
    std::result::Result<(Expression<'field_lookup, 'parse>, FieldType), Error>;

fn ok<'field_lookup, 'parse>(
    exp: Expression<'field_lookup, 'parse>,
    ty: FieldType,
) -> ExpResult<'field_lookup, 'parse> {
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
/// # use std::borrow::Cow;
/// # use dbase_expr::{parser, translate::{self, Expression, FieldType, TranslationContext, Error, SQLTree, ExpResult }, codebase_functions::CodebaseFunction,};
/// struct MyCustomTranslator
/// {
///     my_state: std::collections::HashMap<String, FieldType>,
/// }
///
/// impl TranslationContext for MyCustomTranslator
/// {
///     fn lookup_field<'field_lookup>(
///         &'field_lookup self,
///         alias: Option<&str>,
///         field: &str,
///     ) -> std::result::Result<(Cow<'field_lookup, str>, FieldType), String> {
///         let norm = field.to_uppercase();
///         self.my_state.get(&norm)
///             .map(|t| (Cow::from(norm), *t))
///             .ok_or(format!("No field named {field}"))
///     }
///
///     fn translate_expr<'field_lookup, 'parse>(
///         &'field_lookup self,
///         source: &'parse parser::Expression,
///         src_tree: &'parse parser::ParseTree,
///         dst_tree: &mut SQLTree<'field_lookup, 'parse>
///     ) -> ExpResult<'field_lookup, 'parse> {
///         // This is the place to handle specific cases which are different from Postgres,
///         //  including cases which should be errors
///
///         // Everything else can be delegated:
///         translate::postgres::translate_expr(source, src_tree, dst_tree, self)
///     }
///     
///     fn translate_fn_call<'field_lookup, 'parse>(
///         &'field_lookup self,
///         name: &'parse CodebaseFunction,
///         args: &'parse [parser::ExpressionId],
///         src_tree: &'parse parser::ParseTree,
///         dst_tree: &mut SQLTree<'field_lookup, 'parse>
///     ) -> ExpResult<'field_lookup, 'parse> {
///         // Use a similar pattern here: most function calls probably resolve to the
///         //  same thing that Postgres uses but handle the differences here
///
///         // and delegate the rest...
///         translate::postgres::translate_fn_call(name, args, src_tree, dst_tree, self)
///     }
///
///     fn translate_binary_op<'field_lookup, 'parse>(
///         &'field_lookup self,
///         l: &'parse parser::Expression,
///         op: &'parse parser::BinaryOp,
///         r: &'parse parser::Expression,
///         src_tree: &'parse parser::ParseTree,
///         dst_tree: &mut SQLTree<'field_lookup, 'parse>,
///     ) -> ExpResult<'field_lookup, 'parse> {
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
    fn lookup_field<'field_lookup>(
        &'field_lookup self,
        alias: Option<&str>,
        field: &str,
    ) -> std::result::Result<(Cow<'field_lookup, str>, FieldType), String>;

    /// Called to translate a [ParseTree].
    fn translate<'field_lookup, 'parse>(
        &'field_lookup self,
        tree: &'parse ParseTree,
    ) -> TreeResult<'field_lookup, 'parse> {
        let root = tree.get_root().ok_or(Error::EmptyTree)?;
        let mut out_tree = SQLTree::new();
        let (root, root_type) = self.translate_expr(root, tree, &mut out_tree)?;
        out_tree.push_expr(root);
        Ok((out_tree, root_type))
    }

    /// Called to translate a specific expression within a [ParseTree].
    fn translate_expr<'field_lookup, 'parse>(
        &'field_lookup self,
        source: &'parse parser::Expression,
        in_tree: &'parse ParseTree,
        out_tree: &mut SQLTree<'field_lookup, 'parse>,
    ) -> ExpResult<'field_lookup, 'parse>;

    /// Called to translate a function call.
    ///   `name`: the name of the function in the original expression
    ///   `args`: the arguments to the function
    ///
    /// On success, returns an expression and the type the expression would return.
    fn translate_fn_call<'field_lookup, 'parse>(
        &'field_lookup self,
        name: &'parse CodebaseFunction,
        args: &'parse [parser::ExpressionId],
        in_tree: &'parse ParseTree,
        out_tree: &mut SQLTree<'field_lookup, 'parse>,
    ) -> ExpResult<'field_lookup, 'parse>;

    /// Called to translate a binary operator expression.
    fn translate_binary_op<'field_lookup, 'parse>(
        &'field_lookup self,
        l: &'parse parser::Expression,
        op: &'parse parser::BinaryOp,
        r: &'parse parser::Expression,
        in_tree: &'parse ParseTree,
        out_tree: &mut SQLTree<'field_lookup, 'parse>,
    ) -> ExpResult<'field_lookup, 'parse>;

    /// Truncate the right side of a string comparison to a fixed length.
    fn string_comp_right(&self, r: ExpressionId, len: u32, dst_tree: &mut SQLTree) -> ExpressionId {
        let already_short_enough = matches!(
            dst_tree.get_expr_unchecked(r),
            Expression::SingleQuoteStringLiteral(s) if s.chars().count() <= len as usize
        );

        if already_short_enough {
            return r;
        }

        let len = dst_tree.push_expr(Expression::NumberLiteral(len.to_string().into()));
        substr_to(r, len, dst_tree)
    }

    /// The left side of the string comparison should be truncated to the length of the right side (basically a startswith compare)
    fn string_comp_left(
        &self,
        l: ExpressionId,
        r: ExpressionId,
        dst_tree: &mut SQLTree,
    ) -> ExpressionId {
        let known_r_len = match dst_tree.get_expr_unchecked(r) {
            Expression::SingleQuoteStringLiteral(s) => Some(s.chars().count()),
            _ => None,
        };

        let Some(r_len) = known_r_len else {
            //substr to the len of the right side (unknown so we use an expression)
            let len_expr = dst_tree.push_fn_call("LENGTH", &[r]);
            return substr_to(l, len_expr, dst_tree);
        };

        let already_short_enough = matches!(
            dst_tree.get_expr_unchecked(l),
            Expression::Field { field_type: FieldType::Character(len), .. } if *len as usize <= r_len
        );

        if already_short_enough {
            return l;
        }

        let r_len = dst_tree.push_expr(Expression::NumberLiteral(r_len.to_string().into()));
        substr_to(l, r_len, dst_tree)
    }
}

fn escape_single_quotes(s: &str) -> Cow<'_, str> {
    const SQ: char = '\'';
    match s.find(SQ) {
        // If no single quotes, no escaping needed
        None => Cow::Borrowed(s),
        Some(start) => {
            let mut res = String::with_capacity(s.len());
            let (before, contains_sq) = s.split_at(start);

            // We know no single quotes before start, so that portion can be memcpy'd
            res.push_str(before);

            // Go character by character for the rest
            for c in contains_sq.chars() {
                if c == SQ {
                    res.push(SQ); // Add an extra quote to escape it
                }
                res.push(c);
            }
            res.into()
        }
    }
}

fn substr_to(expr: ExpressionId, len_expr: ExpressionId, dst_tree: &mut SQLTree) -> ExpressionId {
    dst_tree.push_fn_call("SUBSTR", &[expr, exps::LIT_1, len_expr])
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

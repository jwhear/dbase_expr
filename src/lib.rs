//! This crate provides translation and evaluation of dBase expressions, including
//!  the Codebase extensions.
//!
//! Translation is accomplished by parsing dBase expressions to a parse tree
//!  (the term "ast" is used for convenience though not strictly correct),
//!  transformed to a SQL parse tree, then serialized to a SQL string.
//! Each of these steps is implemented by the modules [grammar] and [ast],
//!  [translate], and [to_sql].
//!
//! Additionally, the [evaluate] module implements evaluation of expressions:
//!  if you have a way to fetch field values on request, evaluation can occur
//!  without translating to SQL and dispatching to a database.
//!
//! ## Supported Expressions
//! The full dBase expression set, as described by the Codebase 6 reference guide
//!  is supported. The full list of supported functions can be found in the
//!  [CodebaseFunction] enum.
//!
//! ## Translation
//! To translate a dBase expression to PostgreSQL:
//! ```
//! # use dbase_expr::translate::FieldType;
//! # fn main() -> Result<(), String> {
//! use dbase_expr::{grammar, translate::{TranslationContext, postgres}, to_sql::{Printer, PrinterConfig}};
//! let parser = grammar::ExprParser::new();
//! let parse_tree = parser.parse(r#"(DATE() + 1) - STOD("20240731")"#)
//!     .map_err(|e| format!("{e}"))?;
//! let translator = postgres::Translator {
//!    field_lookup: |opt_table_alias, field_name| {
//!       // opt_table_alias will be Some(&str) if a table alias was provided
//!       //  (e.g. "bar.foo") and None otherwise.
//!       // field_name is a &str with the name of the field as sliced out of
//!       //  the input: you will probably want to normalize this (e.g. uppercase).
//!
//!       // Normalize the field name if needed and produce the Codebase type
//!       // Return a Ok((<normalized field name>, <field_type>))
//!       //  or
//!       // Err(<string describing the problem>)
//!       Ok((String::from("FOO"), FieldType::Logical))
//!    }
//! };
//! let (sql_tree, result_type) = translator.translate(&parse_tree)
//!    .map_err(|e| format!("{e}"))?;
//! let printer = Printer::new(sql_tree, PrinterConfig::default());
//! let sql = format!("{printer}");
//! # Ok(())
//! # }
//! ```
//!
//! Any component of this pipeline could be replaced, though in practice only
//!  the [TranslationContext] and [PrinterConfig] need to be adjusted for
//!  different SQL backends.
//!
//! For a fuller example, with nice error handling, see
//!
//! ## Backends
//! Different SQL databases having varying types, functions, and even syntax.
//! New translation backends can be implemented using the [TranslationContext]
//!  trait and, if needed, the [PrinterConfig] context.

pub mod codebase_functions;
pub mod evaluate;
//pub mod fuzz_helper; // Not fully adapted: custom functions now return translate::Expression, evalute expects parser::Expression
pub mod lex;
pub mod parser;
pub mod to_sql;
pub mod translate;
pub use parser::Error;
pub use parser::parse;

// These imports are just to make the documentation nicer
#[allow(unused_imports)]
use codebase_functions::CodebaseFunction;
#[allow(unused_imports)]
use to_sql::PrinterConfig;
#[allow(unused_imports)]
use translate::TranslationContext;

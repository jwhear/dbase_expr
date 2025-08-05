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
//! ## Translation
//! To translate a dBase expression to PostgreSQL:
//! ```
//! use dbase_expr::{grammar, translate::postgres, to_sql::Printer};
//! let parser = grammar::ExprParser::new();
//! let parse_tree = parser.parse(my_dbase_expression)?;
//! let translator = postgres::Translator {
//!    field_lookup: |opt_table_alias, field_name| {
//!       //TODO normalize the field name if needed and produce the Codebase type
//!       // Return a Ok((<normalized field name>, <field_type>))
//!       //  or
//!       // Err(<string describing the problem>)
//!    }
//! };
//! let (sql_tree, result_type) = translator.translate(&parse_tree)?;
//! let printer = Printer::new(sql_tree);
//! let sql = format!("{printer}");
//! ```
//!
//! ## Backends
//! Different SQL databases having varying types, functions, and even syntax.
//! New translation backends can be implemented using the [translate::TranslationContext]
//!  trait.

use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod evaluate;
pub mod to_sql;
pub mod translate;
lalrpop_mod!(pub grammar);

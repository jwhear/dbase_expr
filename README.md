A library for parsing CodeBase (dBase) expressions and mechanically translating them into SQL expressions.

| dBase       | SQL           | Notes |
|-------------|---------------|-------|
| `a + b * c` | `(a+(b*c))`   | Note correct operator precedence |
| `(a + b) * c` | `((a+b)*c)` | ditto |
| `.T..AND..FALSE.` | `(TRUE AND FALSE)` ||
| `"T"$L_NAME` | `INSTR(L_NAME,'T')` | Conversion from double quoted string to SQL's single quotes; `$` to `INSTR` function call|
| `(DATE() + 1) - STOD("20240731")` | `((STRFTIME('%Y%m%d')+1)-CONCAT_WS('-',SUBSTR('20240731',1,4),SUBSTR('20240731',5,2),SUBSTR('20240731',7,2)))` | Ugly, but deals with differences in Date representation |

== Running
`cargo run` will run the sample program (`src/main.rs`) which runs a bunch of test strings through the parse-translate-print cycle.

== TODO
[x] Correctly parse dBase expressions into an parse tree
[x] Translate the dBase parse tree to a SQL parse tree
[x] Serialize the SQL parse tree as SQL
[ ] Use type context to implement overloaded operations (primarily `+`, `-`)
[ ] Pretty-printing of SQL

== Design
* The parsing of dBase expressions is implemented with a Lalrpop grammar (`src/grammar.lalrpop`) and supported by the AST in `src/ast.rs`.
* The SQL AST and translation code is in `src/sql.rs`
* The SQL serialization is in `src/to_sql.rs`

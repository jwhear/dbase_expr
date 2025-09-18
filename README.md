A library for parsing CodeBase (dBase) expressions and mechanically translating them into SQL expressions.

| dBase       | SQL           | Notes |
|-------------|---------------|-------|
| `a + b * c` | `(a+(b*c))`   | Note correct operator precedence |
| `(a + b) * c` | `((a+b)*c)` | ditto |
| `.T..AND..FALSE.` | `(TRUE AND FALSE)` ||
| `"T"$L_NAME` | `CAST (STRPOS(RPAD(COALESCE("L_NAME", ''), 20, ' '),'T') AS bool)` | Conversion from double quoted string to SQL's single quotes; `$` to `INSTR` function call|
| `(DATE() + 1) - STOD("20240731")` | `(( CURRENT_DATE +1)-TO_DATE('20240731','YYYYMMDD'))` | Ugly, but deals with differences in Date representation |

## Running
`cargo run` will run the sample program (`src/main.rs`) which runs a bunch of test strings through the parse-translate-print cycle.

## Design
* The parsing of dBase expressions is implemented with a Lalrpop grammar (`src/grammar.lalrpop`) and supported by the AST in `src/ast.rs`.
* The SQL AST and translation code is in `src/sql.rs`
* The SQL serialization is in `src/to_sql.rs`
* Limited support for evaluation of expressions is also provided in `src/evaluate.rs`

This library aims to faithfully emulate dBase expression evaluation as implemented by Codebase. However, there are areas where we deliberately diverge.

== Early parsing termination
This crate expects all expressions to be syntactically valid and returns an error if they are not. Codebase is more lenient with poorly-formed expressions.

Example: `(2.0)) * 3.0`
Codebase evaluates this expression as `2.0` while this crate errors with due to the extra closing paren in the middle of the expression.

While it is possible to extend the grammar to ignore characters after an unmatched closing paren, we feel that this hides bugs rather than exposing them: the author of the expression certainly wanted `6.0` and accidentally inserted an additional closing paren.

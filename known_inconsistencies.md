This library aims to faithfully emulate dBase expression evaluation as implemented by Codebase. However, there are areas where we deliberately diverge.

## Early parsing termination
This crate expects all expressions to be syntactically valid and returns an error if they are not. Codebase is more lenient with poorly-formed expressions.

Example: `(2.0)) * 3.0`
Codebase evaluates this expression as `2.0` while this crate errors due to the extra closing paren in the middle of the expression.

While it is possible to extend the grammar to ignore characters after an unmatched closing paren, we feel that this hides bugs rather than exposing them: the author of the expression certainly wanted `6.0` and accidentally inserted an additional closing paren.


## Number parsing
Codebase will parse a number from nothing but a sign (`-` or `+`) or nothing but a decimal place (`.`). So `-` evaluates to negative zero. This causes lots of weirdness (see the [Bestiary](bestiary.md)) including doing the opposite of what you want (`-1` and `--1` both evaluate to negative one, but `---1` is positive one), so we turn this into an error.

This crate now allows parsing a number from nothing but a `.` but evaluating with stacked negation should still be an error.

Codebase expressions which are not compliant should always result in parse errors.

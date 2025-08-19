This page documents _interesting_ dBase expressions which Codebase permits.

## Comments! (plz no)
`2.0   )) a comment!`


Codebase evaluates this to `2.0` because everything after an unmatched close paren is ignored.

Note that this crate produces a parse error instead, see [Known Inconsistencies](known_inconsistencies.md) for more

## Implicit zero digits
`.+.>.`

Evaluates to `.f.` because:
* A number may be written without digits _before_ the decimal place, e.g. `.17` = `0.17`
* A number may be written without digits _after_ the decimal place, e.g. `42.` = `42.0`
* These rules may be combined! `.` = `0.0`!

So this expression could be written as `0.0 + 0.0 > 0.0` which is false.

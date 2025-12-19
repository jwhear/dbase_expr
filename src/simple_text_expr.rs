/// Detects whether the expression is a simple text expression. See test cases
///  for a sense of what is "simple". This is an optimization for a common case
///  in Codebase expression evaluation and bypasses the normal parser.
pub fn is_simple_text_expr(expr: &str) -> bool {
    parse_simple_text_expr_core(expr, |_| {}).is_ok()
}

/// If [expr] is a simple text expression, return the concatenation of all
///  substrings.
/// ```
/// assert_eq!(
///   parse_simple_text_expr(r#"("hello")+("world")"#),
///   Some("helloworld".to_string())
/// );
/// ```
pub fn parse_simple_text_expr(expr: &str) -> Result<String, Error> {
    let mut result = String::new();
    parse_simple_text_expr_core(expr, |segment| {
        result.push_str(segment);
    })
    .map(|_| result)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    // The parser expected `("` but found something else
    ExpectedOpening,
    // The parser expected `")` but found something else
    ExpectedClosing,
    /// Only the `+` operator is supported
    InvalidOperator,
}

fn parse_simple_text_expr_core<F>(mut expr: &str, mut on_segment: F) -> Result<(), Error>
where
    F: FnMut(&str),
{
    // We'll consume bits of `expr` until it's empty or we hit an error case
    // Note that `strip_prefix` returns None if the str doesn't start with the
    //  prefix, so `expr.strip_prefix(x).ok_or(e)?` returns the error `e` if
    //  `expr` doesn't start with the `x`.
    loop {
        // Skip leading whitespace
        expr = expr.trim_ascii_start();

        // Expect ("
        expr = expr.strip_prefix("(\"").ok_or(Error::ExpectedOpening)?;

        // Extract string content until ")
        let (content, rest) = expr.split_once("\")").ok_or(Error::ExpectedClosing)?;
        on_segment(content);
        expr = rest;

        // Skip whitespace
        expr = expr.trim_ascii_start();

        if expr.is_empty() {
            // If we've consumed all the input, we're done
            break;
        } else {
            // If there's any continuation it must be '+'
            expr = expr.strip_prefix("+").ok_or(Error::InvalidOperator)?;
            // Note that the loop can't be refactored to a `while` because we
            //  must parse another expression (can't end with a '+')
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_simple_literal() {
        assert_eq!(
            parse_simple_text_expr(r#"("hello")"#),
            Ok("hello".to_string())
        );
        assert!(is_simple_text_expr(r#"("hello")"#));
    }

    #[test]
    fn test_simple_literal_with_spaces() {
        assert_eq!(
            parse_simple_text_expr(r#"("BERRY                   ")"#),
            Ok("BERRY                   ".to_string())
        );
    }

    #[test]
    fn test_empty_string() {
        assert_eq!(parse_simple_text_expr(r#"("")"#), Ok("".to_string()));
        assert!(is_simple_text_expr(r#"("")"#));
    }

    #[test]
    fn test_two_part_concat() {
        assert_eq!(
            parse_simple_text_expr(r#"("hello")+("world")"#),
            Ok("helloworld".to_string())
        );
        assert!(is_simple_text_expr(r#"("hello")+("world")"#));
    }

    #[test]
    fn test_three_part_concat() {
        assert_eq!(
            parse_simple_text_expr(r#"("a")+("b")+("c")"#),
            Ok("abc".to_string())
        );
    }

    #[test]
    fn test_concat_with_whitespace() {
        assert_eq!(
            parse_simple_text_expr(r#"("          ")+("                        ")"#),
            Ok("                                  ".to_string())
        );
    }

    #[test]
    fn test_concat_with_spaces_around_plus() {
        assert_eq!(
            parse_simple_text_expr(r#"("a") + ("b")"#),
            Ok("ab".to_string())
        );
        assert_eq!(
            parse_simple_text_expr(r#"("a")  +  ("b")"#),
            Ok("ab".to_string())
        );
    }

    #[test]
    fn test_leading_whitespace() {
        assert_eq!(
            parse_simple_text_expr(r#"  ("hello")"#),
            Ok("hello".to_string())
        );
        assert_eq!(
            parse_simple_text_expr(r#"   ("a")+("b")"#),
            Ok("ab".to_string())
        );
    }

    #[test]
    fn test_trailing_whitespace() {
        assert_eq!(
            parse_simple_text_expr(r#"("hello")  "#),
            Ok("hello".to_string())
        );
    }

    #[test]
    fn test_real_world_examples() {
        assert_eq!(
            parse_simple_text_expr(r#"("                        ")"#),
            Ok("                        ".to_string())
        );

        assert_eq!(
            parse_simple_text_expr(r#"("COIL_BERRY_26G_G50_41   ")+("PNSFL     ")"#),
            Ok("COIL_BERRY_26G_G50_41   PNSFL     ".to_string())
        );

        assert_eq!(
            parse_simple_text_expr(r#"("PH41COIL                ") + ("LF        ")"#),
            Ok("PH41COIL                LF        ".to_string())
        );

        assert_eq!(
            parse_simple_text_expr(r#"("         202385")+("S")"#),
            Ok("         202385S".to_string())
        );
    }

    #[test]
    fn test_not_simple_text_with_iif() {
        assert_eq!(
            parse_simple_text_expr(r#"("I58T7BZ2QO")+IIF(.F.,"T","F")"#),
            Err(Error::ExpectedOpening)
        );
        assert!(!is_simple_text_expr(r#"("I58T7BZ2QO")+IIF(.F.,"T","F")"#));
    }

    #[test]
    fn test_not_simple_text_missing_parens() {
        assert_eq!(
            parse_simple_text_expr(r#""hello""#),
            Err(Error::ExpectedOpening)
        );
        assert!(!is_simple_text_expr(r#""hello""#));
    }

    #[test]
    fn test_not_simple_text_no_opening_paren() {
        assert_eq!(
            parse_simple_text_expr(r#""hello")"#),
            Err(Error::ExpectedOpening)
        );
    }

    #[test]
    fn test_not_simple_text_no_closing_paren() {
        assert_eq!(
            parse_simple_text_expr(r#"("hello""#),
            Err(Error::ExpectedClosing)
        );
    }

    #[test]
    fn test_not_simple_text_incomplete_concat() {
        assert_eq!(
            parse_simple_text_expr(r#"("hello")+"#),
            Err(Error::ExpectedOpening)
        );
        assert_eq!(
            parse_simple_text_expr(r#"("hello")+("#),
            Err(Error::ExpectedOpening)
        );
    }

    #[test]
    fn test_not_simple_text_wrong_operator() {
        assert_eq!(
            parse_simple_text_expr(r#"("a")-("b")"#),
            Err(Error::InvalidOperator)
        );
        assert_eq!(
            parse_simple_text_expr(r#"("a")*("b")"#),
            Err(Error::InvalidOperator)
        );
    }

    #[test]
    fn test_not_simple_text_numbers() {
        assert_eq!(parse_simple_text_expr("0"), Err(Error::ExpectedOpening));
        assert_eq!(parse_simple_text_expr("123"), Err(Error::ExpectedOpening));
        assert!(!is_simple_text_expr("0"));
    }

    #[test]
    fn test_not_simple_text_str_function() {
        assert_eq!(
            parse_simple_text_expr("STR(-3,12,0)"),
            Err(Error::ExpectedOpening)
        );
        assert!(!is_simple_text_expr("STR(-3,12,0)"));
    }

    #[test]
    fn test_special_characters_in_string() {
        assert_eq!(
            parse_simple_text_expr(r#"("hello-world")"#),
            Ok("hello-world".to_string())
        );
        assert_eq!(
            parse_simple_text_expr(r#"("51000-100")"#),
            Ok("51000-100".to_string())
        );
        assert_eq!(
            parse_simple_text_expr(r#"("a_b_c")"#),
            Ok("a_b_c".to_string())
        );
    }

    #[test]
    fn test_empty_concat() {
        assert_eq!(parse_simple_text_expr(r#"("")+("")"#), Ok("".to_string()));
        assert_eq!(parse_simple_text_expr(r#"("a")+("")"#), Ok("a".to_string()));
    }

    #[test]
    fn test_multiple_segments() {
        let mut segments = Vec::new();
        let result = parse_simple_text_expr_core(r#"("a")+("b")+("c")"#, |seg| {
            segments.push(seg.to_string());
        });

        assert!(result.is_ok());
        assert_eq!(segments, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_edge_case_single_char() {
        assert_eq!(parse_simple_text_expr(r#"("x")"#), Ok("x".to_string()));
    }

    #[test]
    fn test_edge_case_many_parts() {
        assert_eq!(
            parse_simple_text_expr(r#"("1")+("2")+("3")+("4")+("5")"#),
            Ok("12345".to_string())
        );
    }
}

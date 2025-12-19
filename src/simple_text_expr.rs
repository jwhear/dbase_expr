fn parse_simple_text_expr_core<F>(expr: &str, mut on_segment: F) -> Result<(), ()>
where
    F: FnMut(&str),
{
    let bytes = expr.as_bytes();
    let mut i = 0;

    // Skip leading whitespace
    while i < bytes.len() && bytes[i].is_ascii_whitespace() {
        i += 1;
    }

    // Must start with ("
    if i + 1 >= bytes.len() || bytes[i] != b'(' || bytes[i + 1] != b'"' {
        return Err(());
    }

    loop {
        // Expect ("
        if i + 1 >= bytes.len() || bytes[i] != b'(' || bytes[i + 1] != b'"' {
            return Err(());
        }
        i += 2;

        // Extract string content until ")
        let start = i;
        while i < bytes.len() && !(bytes[i] == b'"' && i + 1 < bytes.len() && bytes[i + 1] == b')')
        {
            i += 1;
        }

        if i >= bytes.len() {
            return Err(());
        }

        on_segment(&expr[start..i]);

        i += 2; // Skip ")

        // Skip whitespace
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }

        // End of expression
        if i >= bytes.len() {
            return Ok(());
        }

        // Must be +
        if bytes[i] != b'+' {
            return Err(());
        }
        i += 1;

        // Skip whitespace after +
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }
    }
}

pub fn is_simple_text_expr(expr: &str) -> bool {
    parse_simple_text_expr_core(expr, |_| {}).is_ok()
}

pub fn parse_simple_text_expr(expr: &str) -> Option<String> {
    let mut result = String::new();
    parse_simple_text_expr_core(expr, |segment| {
        result.push_str(segment);
    })
    .ok()
    .map(|_| result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_literal() {
        assert_eq!(
            parse_simple_text_expr(r#"("hello")"#),
            Some("hello".to_string())
        );
        assert!(is_simple_text_expr(r#"("hello")"#));
    }

    #[test]
    fn test_simple_literal_with_spaces() {
        assert_eq!(
            parse_simple_text_expr(r#"("BERRY                   ")"#),
            Some("BERRY                   ".to_string())
        );
    }

    #[test]
    fn test_empty_string() {
        assert_eq!(parse_simple_text_expr(r#"("")"#), Some("".to_string()));
        assert!(is_simple_text_expr(r#"("")"#));
    }

    #[test]
    fn test_two_part_concat() {
        assert_eq!(
            parse_simple_text_expr(r#"("hello")+("world")"#),
            Some("helloworld".to_string())
        );
        assert!(is_simple_text_expr(r#"("hello")+("world")"#));
    }

    #[test]
    fn test_three_part_concat() {
        assert_eq!(
            parse_simple_text_expr(r#"("a")+("b")+("c")"#),
            Some("abc".to_string())
        );
    }

    #[test]
    fn test_concat_with_whitespace() {
        assert_eq!(
            parse_simple_text_expr(r#"("          ")+("                        ")"#),
            Some("                                  ".to_string())
        );
    }

    #[test]
    fn test_concat_with_spaces_around_plus() {
        assert_eq!(
            parse_simple_text_expr(r#"("a") + ("b")"#),
            Some("ab".to_string())
        );
        assert_eq!(
            parse_simple_text_expr(r#"("a")  +  ("b")"#),
            Some("ab".to_string())
        );
    }

    #[test]
    fn test_leading_whitespace() {
        assert_eq!(
            parse_simple_text_expr(r#"  ("hello")"#),
            Some("hello".to_string())
        );
        assert_eq!(
            parse_simple_text_expr(r#"   ("a")+("b")"#),
            Some("ab".to_string())
        );
    }

    #[test]
    fn test_trailing_whitespace() {
        assert_eq!(
            parse_simple_text_expr(r#"("hello")  "#),
            Some("hello".to_string())
        );
    }

    #[test]
    fn test_real_world_examples() {
        assert_eq!(
            parse_simple_text_expr(r#"("                        ")"#),
            Some("                        ".to_string())
        );

        assert_eq!(
            parse_simple_text_expr(r#"("COIL_BERRY_26G_G50_41   ")+("PNSFL     ")"#),
            Some("COIL_BERRY_26G_G50_41   PNSFL     ".to_string())
        );

        assert_eq!(
            parse_simple_text_expr(r#"("PH41COIL                ") + ("LF        ")"#),
            Some("PH41COIL                LF        ".to_string())
        );

        assert_eq!(
            parse_simple_text_expr(r#"("         202385")+("S")"#),
            Some("         202385S".to_string())
        );
    }

    #[test]
    fn test_not_simple_text_with_iif() {
        assert_eq!(
            parse_simple_text_expr(r#"("I58T7BZ2QO")+IIF(.F.,"T","F")"#),
            None
        );
        assert!(!is_simple_text_expr(r#"("I58T7BZ2QO")+IIF(.F.,"T","F")"#));
    }

    #[test]
    fn test_not_simple_text_missing_parens() {
        assert_eq!(parse_simple_text_expr(r#""hello""#), None);
        assert!(!is_simple_text_expr(r#""hello""#));
    }

    #[test]
    fn test_not_simple_text_no_opening_paren() {
        assert_eq!(parse_simple_text_expr(r#""hello")"#), None);
    }

    #[test]
    fn test_not_simple_text_no_closing_paren() {
        assert_eq!(parse_simple_text_expr(r#"("hello""#), None);
    }

    #[test]
    fn test_not_simple_text_incomplete_concat() {
        assert_eq!(parse_simple_text_expr(r#"("hello")+"#), None);
        assert_eq!(parse_simple_text_expr(r#"("hello")+("#), None);
    }

    #[test]
    fn test_not_simple_text_wrong_operator() {
        assert_eq!(parse_simple_text_expr(r#"("a")-("b")"#), None);
        assert_eq!(parse_simple_text_expr(r#"("a")*("b")"#), None);
    }

    #[test]
    fn test_not_simple_text_numbers() {
        assert_eq!(parse_simple_text_expr("0"), None);
        assert_eq!(parse_simple_text_expr("123"), None);
        assert!(!is_simple_text_expr("0"));
    }

    #[test]
    fn test_not_simple_text_str_function() {
        assert_eq!(parse_simple_text_expr("STR(-3,12,0)"), None);
        assert!(!is_simple_text_expr("STR(-3,12,0)"));
    }

    #[test]
    fn test_special_characters_in_string() {
        assert_eq!(
            parse_simple_text_expr(r#"("hello-world")"#),
            Some("hello-world".to_string())
        );
        assert_eq!(
            parse_simple_text_expr(r#"("51000-100")"#),
            Some("51000-100".to_string())
        );
        assert_eq!(
            parse_simple_text_expr(r#"("a_b_c")"#),
            Some("a_b_c".to_string())
        );
    }

    #[test]
    fn test_empty_concat() {
        assert_eq!(parse_simple_text_expr(r#"("")+("")"#), Some("".to_string()));
        assert_eq!(
            parse_simple_text_expr(r#"("a")+("")"#),
            Some("a".to_string())
        );
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
        assert_eq!(parse_simple_text_expr(r#"("x")"#), Some("x".to_string()));
    }

    #[test]
    fn test_edge_case_many_parts() {
        assert_eq!(
            parse_simple_text_expr(r#"("1")+("2")+("3")+("4")+("5")"#),
            Some("12345".to_string())
        );
    }
}

/// #Notes
/// `+` and `-` could be operators or the starts of numbers (`-.1`). This lexer
///  does not attempt to distinguish: they always become Plus and Minus tokens.
///  The parser should use its increased context to disambiguate these usages.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
    ParenLeft,
    ParenRight,
    Comma,
    Plus,
    Minus,
    Asterisk,
    ForwardSlash,
    DoubleAsterisk,
    Arrow, // ->, used in [db alias]->field_name
    Dollar,
    Equals,    // =
    NotEquals, // <>
    LT,        // <
    GT,        // >
    LTE,       // <=
    GTE,       // >=
    Number,
    Identifier,
    And,
    Or,
    Not,
    True,
    False,
    StringSingleQuote,
    StringDoubleQuote,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    ty: TokenType,

    // Byte indexes into the source
    start: usize,
    end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    UnterminatedStringLiteral(usize),
    UnexpectedCharacter(usize),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnterminatedStringLiteral(start) => {
                write!(f, "Unterminated string literal starting at {start}")
            }
            Self::UnexpectedCharacter(start) => {
                write!(f, "Unexpected character at {start}")
            }
        }
    }
}

pub struct Lexer<'input> {
    source: &'input [u8],
    current: usize,
}

impl<'input> Lexer<'input> {
    pub fn new(source: &'input [u8]) -> Self {
        Self { source, current: 0 }
    }

    pub fn is_empty(&self) -> bool {
        self.current >= self.source.len()
    }

    pub fn peek(&self) -> Option<u8> {
        if self.is_empty() {
            None
        } else {
            Some(self.peek_unchecked())
        }
    }

    pub fn peek_unchecked(&self) -> u8 {
        self.source[self.current]
    }

    pub fn pop(&mut self) -> Option<u8> {
        let res = self.peek();
        if res.is_some() {
            self.current += 1;
        }
        res
    }

    fn pop_unchecked(&mut self) -> u8 {
        let res = self.peek_unchecked();
        self.current += 1;
        res
    }

    /// If current starts with [prefix] (compared ASCII case-insensitive),
    ///  consume and return true. Otherwise, return false and leave current
    ///  unchanged.
    fn consume_ignore_ascii_case(&mut self, prefix: &[u8]) -> bool {
        let remainder = &self.source[self.current..];

        // We can't start with prefix if prefix is longer
        if prefix.len() > remainder.len() {
            return false;
        }

        // after the above check we can safely slice to prefix.len()
        let relevant = &remainder[..prefix.len()];
        let is_prefix = relevant.eq_ignore_ascii_case(prefix);

        if is_prefix {
            self.current += prefix.len();
        }
        is_prefix
    }

    fn consume_while(&mut self, predicate: impl Fn(u8) -> bool) {
        while let Some(c) = self.peek()
            && predicate(c)
        {
            self.current += 1;
        }
    }

    fn consume_whitespace(&mut self) {
        self.consume_while(|b| b == b' ' || b == b'\t');
    }

    fn consume_number(&mut self) {
        // Start with zero or more digits
        self.consume_while(|b| matches!(b, b'0'..=b'9'));

        // Optional dot
        if let Some(b'.') = self.peek() {
            self.current += 1; // consume '.'
            // Followed by zero or more digits
            self.consume_while(|b| matches!(b, b'0'..=b'9'));
        }
    }

    /// Returns the slice of the source that this token was lexed from.
    pub fn source_of(&self, token: &Token) -> &[u8] {
        &self.source[token.start..token.end]
    }

    /// Like [source_of] but omits the opening and closing quotes of string
    ///  literal tokens.
    pub fn contents(&self, token: &Token) -> &[u8] {
        let s = self.source_of(token);
        match token.ty {
            TokenType::StringSingleQuote | TokenType::StringDoubleQuote => &s[1..s.len() - 1],
            _ => &s,
        }
    }

    pub fn next_token(&mut self) -> Result<Option<Token>, Error> {
        self.consume_whitespace();

        if self.is_empty() {
            return Ok(None);
        }
        let start = self.current;

        // Convenience macro for returning a token from `start` to `self.current`
        // The match below will borrow self as mutable, so a simple closure won't
        //  do the trick.
        macro_rules! tok {
            ($name:ident) => {{
                Token {
                    ty: TokenType::$name,
                    start,
                    end: self.current,
                }
            }};
        }

        Ok(Some(match self.pop_unchecked() {
            b'(' => tok!(ParenLeft),
            b')' => tok!(ParenRight),
            b',' => tok!(Comma),
            b'/' => tok!(ForwardSlash),
            b'$' => tok!(Dollar),
            b'=' => tok!(Equals),

            // While +/- could be the start of a number, we treat them as
            //  operators and allow the parser to interpret them as unary or
            //  binary operators
            b'+' => tok!(Plus),
            b'-' => {
                // Arrow?
                if self.consume_ignore_ascii_case(b">") {
                    tok!(Arrow)
                } else {
                    tok!(Minus)
                }
            }
            b'*' => {
                // Exponentiation?
                if self.consume_ignore_ascii_case(b"*") {
                    tok!(DoubleAsterisk)
                } else {
                    tok!(Asterisk)
                }
            }
            b'<' => {
                if self.consume_ignore_ascii_case(b">") {
                    tok!(NotEquals)
                } else if self.consume_ignore_ascii_case(b"=") {
                    tok!(LTE)
                } else {
                    tok!(LT)
                }
            }
            b'>' => {
                if self.consume_ignore_ascii_case(b"=") {
                    tok!(GTE)
                } else {
                    tok!(GT)
                }
            }

            // Single and double-quoted strings
            // Note that Codebase doesn't support any escape sequences
            term if term == b'\'' || term == b'"' => {
                self.consume_while(|b| b != term);
                if self.is_empty() {
                    return Err(Error::UnterminatedStringLiteral(start));
                }

                // consume closing term
                self.current += 1;
                if term == b'"' {
                    tok!(StringDoubleQuote)
                } else {
                    tok!(StringSingleQuote)
                }
            }

            // Identifiers start with a-Z or underscore
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                self.consume_while(|b| match b {
                    b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' => true,
                    _ => false,
                });
                tok!(Identifier)
            }

            // Digits mean it's a number
            b'0'..b'9' => {
                self.consume_number();
                tok!(Number)
            }

            // A dot could be the start of a number, an operator, or a boolean literal
            b'.' => {
                // This macro expands a table to a an `if..if else..else` structure
                macro_rules! table_to_if_else {
                    ($default:block) => {
                        $default
                    };
                    ($cond:expr => $t:ident, $($rest:tt)*) => {
                        if $cond {
                            tok!($t)
                        } else {
                            table_to_if_else!($($rest)*)
                        }
                    };
                }
                table_to_if_else!(
                    self.consume_ignore_ascii_case(b"and.") => And,
                    self.consume_ignore_ascii_case(b"not.") => Not,
                    self.consume_ignore_ascii_case(b"or.") => Or,
                    self.consume_ignore_ascii_case(b"t.") => True,
                    self.consume_ignore_ascii_case(b"f.") => False,

                    // Default case: assume it's a number.
                    // Even a bare '.' is a valid number (0.0)
                    {
                        self.consume_number();
                        tok!(Number)
                    }
                )
            }
            _ => return Err(Error::UnexpectedCharacter(self.current)),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_basic() {
        //NOTE this test doesn't use the handy assert_toks macro because we're
        //  checking that the token boundaries are correct as well.
        //              0         1         2         3
        //              012345678901234567890123456789012
        let source = r#"'single' $ "double" (,/) .not..t."#;
        let mut lexer = Lexer::new(source.as_bytes());

        // 'single'
        let tok = lexer.next_token();
        assert_eq!(
            tok,
            Ok(Some(Token {
                ty: TokenType::StringSingleQuote,
                start: 0,
                end: 8
            }))
        );
        assert_eq!(lexer.contents(&tok.unwrap().unwrap()), b"single");

        // Dollar
        assert_eq!(
            lexer.next_token(),
            Ok(Some(Token {
                ty: TokenType::Dollar,
                start: 9,
                end: 10
            }))
        );
        // 'double'
        let tok = lexer.next_token();
        assert_eq!(
            tok,
            Ok(Some(Token {
                ty: TokenType::StringDoubleQuote,
                start: 11,
                end: 19
            }))
        );
        assert_eq!(lexer.contents(&tok.unwrap().unwrap()), b"double");

        // ParenLeft
        assert_eq!(
            lexer.next_token(),
            Ok(Some(Token {
                ty: TokenType::ParenLeft,
                start: 20,
                end: 21
            }))
        );
        // Comma
        assert_eq!(
            lexer.next_token(),
            Ok(Some(Token {
                ty: TokenType::Comma,
                start: 21,
                end: 22
            }))
        );
        // ForwardSlash
        assert_eq!(
            lexer.next_token(),
            Ok(Some(Token {
                ty: TokenType::ForwardSlash,
                start: 22,
                end: 23
            }))
        );
        // ParenRight
        assert_eq!(
            lexer.next_token(),
            Ok(Some(Token {
                ty: TokenType::ParenRight,
                start: 23,
                end: 24
            }))
        );
        // Not
        assert_eq!(
            lexer.next_token(),
            Ok(Some(Token {
                ty: TokenType::Not,
                start: 25,
                end: 30
            }))
        );
        // True
        assert_eq!(
            lexer.next_token(),
            Ok(Some(Token {
                ty: TokenType::True,
                start: 30,
                end: 33
            }))
        );
    }

    #[test]
    fn test_lex_numbers() {
        //NOTE this test doesn't use the handy assert_toks macro because we're
        //  checking that the token boundaries are correct as well.
        //             0         1
        //             01234567890123456
        let source = b"12.3 4+5. - .6009";
        let mut lexer = Lexer::new(source);
        assert_eq!(
            lexer.next_token(),
            Ok(Some(Token {
                ty: TokenType::Number,
                start: 0,
                end: 4,
            }))
        );
        assert_eq!(
            lexer.next_token(),
            Ok(Some(Token {
                ty: TokenType::Number,
                start: 5,
                end: 6,
            }))
        );
        assert_eq!(
            lexer.next_token(),
            Ok(Some(Token {
                ty: TokenType::Plus,
                start: 6,
                end: 7,
            }))
        );
        assert_eq!(
            lexer.next_token(),
            Ok(Some(Token {
                ty: TokenType::Number,
                start: 7,
                end: 9,
            }))
        );
        assert_eq!(
            lexer.next_token(),
            Ok(Some(Token {
                ty: TokenType::Minus,
                start: 10,
                end: 11,
            }))
        );
        assert_eq!(
            lexer.next_token(),
            Ok(Some(Token {
                ty: TokenType::Number,
                start: 12,
                end: 17,
            }))
        );
    }

    macro_rules! assert_tok {
        ($lex:ident, $tok_ty:ident) => {{
            let tok = $lex.next_token();
            assert!(
                matches!(
                    tok,
                    Ok(Some(Token {
                        ty: TokenType::$tok_ty,
                        ..
                    }))
                ),
                "Expected {}, got {tok:?}",
                stringify!($tok_ty)
            );
        }};
    }
    macro_rules! assert_toks {
        ($lex:ident, $tok_ty:ident) => {{
            assert_tok!($lex, $tok_ty)
        }};
        ($lex:ident, $tok_ty:ident, $($rest:tt)*) => {
            assert_tok!($lex, $tok_ty);
            assert_toks!($lex, $($rest)*)
        };
    }

    #[test]
    fn test_lex_arrow() {
        let source = b"-tbl->field";
        let mut lexer = Lexer::new(source);
        assert_toks!(lexer, Minus, Identifier, Arrow, Identifier);
    }

    #[test]
    fn test_lex_comparisons() {
        let source = b"= <> < > <= >=";
        let mut lexer = Lexer::new(source);
        assert_toks!(lexer, Equals, NotEquals, LT, GT, LTE, GTE);
    }
}

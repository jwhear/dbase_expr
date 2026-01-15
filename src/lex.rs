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
    DoubleAsterisk, // ** is exponentiation
    Arrow,          // ->, used in [db alias]->field_name
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
    pub ty: TokenType,

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

/// Assumes that [word] is eight ASCII characters packed into a u64 and
///  returns the lowercased equivalent.
#[inline]
fn lowercase_u64(word: u64) -> u64 {
    // Ok, this code is a bit obtuse but it's also branchless and just a handful
    //  of instructions.
    // Key insight is that a character `b` is in A..=Z if:
    //   (b + 0x1F) & ~(b + 0x05) & 0x20 != 0
    // Once we've identified the capitals we can lowercase them by setting the
    //  fifth bit ('A' = 65, 'a' = 97, difference = 32)
    const ADD_1F: u64 = 0x1F1F1F1F1F1F1F1F;
    const ADD_05: u64 = 0x0505050505050505;
    const FIFTH_BIT: u64 = 0x2020202020202020;

    let mask = word.wrapping_add(ADD_1F) & !word.wrapping_add(ADD_05) & FIFTH_BIT;
    word | mask
}

/// We generally call [Lexer::consume_ignore_ascii_case] with a string literal;
///  this macro effectively generates a check for that particular literal and
///  advances the [Lexer] if the source starts with it (case insensitive).
///
/// The cool thing is that all our literals are eight characters or less, so
///  this macro turns the check into an unaligned read, ANDing a mask, and an
///  equality check.
macro_rules! consume_literal {
    ($self:expr, $prefix:literal) => {{
        const LEN: usize = $prefix.len();
        // Lowercase prefix at compile time
        const PREFIX_LOWER_U64: u64 = {
            let mut bytes = [0u8; 8];
            let mut i = 0;
            while i < LEN {
                bytes[i] = $prefix[i].to_ascii_lowercase();
                i += 1;
            }
            u64::from_le_bytes(bytes)
        };
        // Mask for the actual length: e.g., 0x0000FFFF for len=2.
        const MASK: u64 = if LEN == 8 {
            u64::MAX
        } else {
            (1u64 << (LEN * 8)) - 1
        };

        if $self.remaining_len() < LEN {
            false
        } else {
            // Reading unaligned can be dangerous: this intrinsic makes it safe
            //  and it has a pretty negligible performance impact on modern chips
            let word = unsafe { std::ptr::read_unaligned($self.current_ptr() as *const u64) };
            let word = lowercase_u64(word);
            if (word & MASK) == PREFIX_LOWER_U64 {
                $self.current += LEN;
                true
            } else {
                false
            }
        }
    }};
}

/// This type simply holds a reference to the source bytes and an index, so it's
///  cheap to copy, making lookahead/rewind operations in the parser very easy.
#[derive(Clone)]
pub struct Lexer<'input> {
    source: &'input [u8],
    current: usize,
}

impl<'input> Lexer<'input> {
    pub fn new(source: &'input [u8]) -> Self {
        Self { source, current: 0 }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.current >= self.source.len()
    }

    #[inline]
    pub fn peek(&self) -> Option<u8> {
        if self.is_empty() {
            None
        } else {
            Some(self.peek_unchecked())
        }
    }

    #[inline]
    pub fn peek_unchecked(&self) -> u8 {
        self.source[self.current]
    }

    #[inline]
    pub fn peek_at(&self, at: usize) -> Option<u8> {
        let at = self.current + at;
        self.source.get(at).copied()
    }

    #[inline]
    pub fn pop(&mut self) -> Option<u8> {
        let res = self.peek();
        if res.is_some() {
            self.current += 1;
        }
        res
    }

    #[inline]
    fn pop_unchecked(&mut self) -> u8 {
        let res = self.peek_unchecked();
        self.current += 1;
        res
    }

    #[inline]
    fn current_ptr(&self) -> *const u8 {
        &self.source[self.current]
    }

    #[inline]
    pub fn remaining(&self) -> &[u8] {
        &self.source[self.current..]
    }

    #[inline]
    pub fn remaining_len(&self) -> usize {
        self.remaining().len()
    }

    /// If current starts with [prefix], consume it and return true.
    pub fn consume1(&mut self, prefix: u8) -> bool {
        if let Some(c) = self.peek()
            && c == prefix
        {
            self.current += 1;
            true
        } else {
            false
        }
    }

    /// If current starts with [prefix] (compared ASCII case-insensitive),
    ///  consume and return true. Otherwise, return false and leave current
    ///  unchanged.
    /// If you know [prefix] at compile time and it's eight characters or less,
    ///  use [consume_ignore_ascii_case_lit] instead.
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

    #[inline]
    fn consume_while(&mut self, predicate: impl Fn(u8) -> bool) {
        while let Some(c) = self.peek()
            && predicate(c)
        {
            self.current += 1;
        }
    }

    #[inline]
    fn consume_whitespace(&mut self) {
        self.consume_while(|b| b == b' ' || b == b'\t');
    }

    fn consume_number(&mut self) {
        // Start with zero or more digits
        self.consume_while(|b| b.is_ascii_digit());

        // Optional dot
        if let Some(b'.') = self.peek() {
            self.current += 1; // consume '.'
            // Followed by zero or more digits
            self.consume_while(|b| b.is_ascii_digit());
        }
    }

    /// Returns the slice of the source that this token was lexed from.
    #[inline]
    pub fn source_of(&self, token: &Token) -> &'input [u8] {
        &self.source[token.start..token.end]
    }

    /// Like [source_of] but omits the opening and closing quotes of string
    ///  literal tokens.
    #[inline]
    pub fn contents(&self, token: &Token) -> &'input [u8] {
        let s = self.source_of(token);
        match token.ty {
            TokenType::StringSingleQuote | TokenType::StringDoubleQuote => &s[1..s.len() - 1],
            _ => s,
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
            b'^' => tok!(DoubleAsterisk),
            b'#' => tok!(NotEquals),

            // While +/- could be the start of a number, we treat them as
            //  operators and allow the parser to interpret them as unary or
            //  binary operators
            b'+' => tok!(Plus),
            b'-' => {
                // Arrow?
                if self.consume1(b'>') {
                    tok!(Arrow)
                } else {
                    tok!(Minus)
                }
            }
            b'*' => {
                // Exponentiation?
                if self.consume1(b'*') {
                    tok!(DoubleAsterisk)
                } else {
                    tok!(Asterisk)
                }
            }
            b'<' => {
                if self.consume1(b'>') {
                    tok!(NotEquals)
                } else if self.consume1(b'=') {
                    tok!(LTE)
                } else {
                    tok!(LT)
                }
            }
            b'>' => {
                if self.consume1(b'=') {
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
                self.consume_while(|b| matches!(b, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'));
                tok!(Identifier)
            }

            // Digits mean it's a number
            b'0'..=b'9' => {
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
                    // We've already popped the leading '.'
                    consume_literal!(self, b"and.") => And,
                    consume_literal!(self, b"not.") => Not,
                    consume_literal!(self, b"or.") => Or,
                    consume_literal!(self, b"t.") => True,
                    consume_literal!(self, b"f.") => False,

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
    fn lex_basic() {
        //NOTE this test doesn't use the handy assert_toks macro because we're
        //  checking that the token boundaries are correct as well.
        //              0         1         2         3
        //              012345678901234567890123456789012
        let source = r#"'single' $ "double" (,/) .NOT..t."#;
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
    fn lex_numbers() {
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
    fn lex_arrow() {
        let source = b"-tbl->field";
        let mut lexer = Lexer::new(source);
        assert_toks!(lexer, Minus, Identifier, Arrow, Identifier);
    }

    #[test]
    fn lex_comparisons() {
        let source = b"= <> < > <= >=";
        let mut lexer = Lexer::new(source);
        assert_toks!(lexer, Equals, NotEquals, LT, GT, LTE, GTE);
    }

    #[test]
    fn degenerate_numbers() {
        let source = b".+.=."; // 0.0 + 0.0 = 0.0
        let mut lexer = Lexer::new(source);
        assert_toks!(lexer, Number, Plus, Number, Equals, Number);
    }

    #[test]
    fn test_lowercase_u8() {
        let source = u64::from_le_bytes(*b"Hello Z!");
        let result = lowercase_u64(source);
        let witness = u64::from_le_bytes(*b"hello z!");
        assert_eq!(witness, result);

        let source = u64::from_le_bytes(*b"aBcDvXyZ");
        let result = lowercase_u64(source);
        let witness = u64::from_le_bytes(*b"abcdvxyz");
        assert_eq!(witness, result);

        let source = u64::from_le_bytes(*b"@3-+/`&)");
        assert_eq!(source, lowercase_u64(source));
    }
}

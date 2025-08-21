#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    OpenParen,
    CloseParen,
    Comma,
    Arrow,
    LitTrue,
    LitFalse,
    OpAnd,
    OpOr,
    OpNot,
    OpExp,
    Plus,
    Minus,
    Asterisk,
    Exp, // Either ** or ^
    Slash,
    Hash,
    Dollar,
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
    LitNumber(String),
    LitString(String),
    Identifier(String),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    Unexpected(char),
    UnterminatedString,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unexpected(c) => write!(f, "Unexpected token '{c}'"),
            Self::UnterminatedString => write!(f, "Unterminated string"),
        }
    }
}
impl std::error::Error for Error {}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct Lexer<'input> {
    buf: Buffer<'input>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            buf: Buffer::new(input),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        // Consume whitespace
        self.buf.skip_while(char::is_ascii_whitespace);

        // We'll drop a checkpoint before calling pop so that we can slice from
        //  the current character
        let start = self.buf.checkpoint();
        match self.buf.pop() {
            None => None,
            Some((pos, c)) => {
                let single = |tok| Some(Ok((pos, tok, pos + 1)));
                let slice = |s: &str, tok| Some(Ok((pos, tok, pos + s.len())));
                match c {
                    '(' => single(Token::OpenParen),
                    ')' => single(Token::CloseParen),
                    ',' => single(Token::Comma),
                    '$' => single(Token::Dollar),
                    '#' => single(Token::Hash),
                    '+' => single(Token::Plus),
                    '-' => {
                        if self.buf.consume(">") {
                            slice("->", Token::Arrow)
                        } else {
                            single(Token::Minus)
                        }
                    }
                    '*' => {
                        if self.buf.consume("*") {
                            slice("**", Token::Exp)
                        } else {
                            single(Token::Asterisk)
                        }
                    }
                    '^' => single(Token::Exp),
                    '/' => single(Token::Slash),
                    '<' => {
                        if self.buf.consume("=") {
                            slice("<=", Token::Lte)
                        } else if self.buf.consume(">") {
                            slice("<>", Token::Neq)
                        } else {
                            single(Token::Lt)
                        }
                    }
                    '>' => {
                        if self.buf.consume("=") {
                            slice(">=", Token::Gte)
                        } else {
                            single(Token::Gt)
                        }
                    }
                    '=' => single(Token::Eq),
                    quote if quote == '"' || quote == '\'' => {
                        self.buf.skip_while(|&c| c != quote);
                        if self.buf.consume1(quote) {
                            let content = self.buf.from_checkpoint(&start);
                            // omit open and closing quotes
                            let content = content[1..content.len() - 1].to_string();
                            Some(Ok((pos, Token::LitString(content), self.buf.location)))
                        } else {
                            Some(Err(Error::UnterminatedString))
                        }
                    }
                    '.' => {
                        macro_rules! lookahead {
                            ($($pattern:expr => $token:expr),* $(,)?, else $fallback:block) => {
                                $(
                                    if self.buf.consume($pattern) {
                                        slice($pattern, $token)
                                    }
                                ) else *
                                else $fallback
                            }
                        }

                        // Could be an operator, logical literal, or number
                        lookahead![
                            "and." => Token::OpAnd,
                            "or." => Token::OpOr,
                            "not." => Token::OpNot,
                            "true." => Token::LitTrue,
                            "t." => Token::LitTrue,
                            "false." => Token::LitFalse,
                            "f." => Token::LitFalse,
                            else {
                                // Must be a number without leading digits (`.123`)
                                // Scan until we hit a non-digit
                                self.buf.skip_while(char::is_ascii_digit);
                                let content = self.buf.from_checkpoint(&start);
                                Some(Ok((pos, Token::LitNumber(content.to_string()), pos + content.len())))
                            }
                        ]
                    }

                    // Number?
                    c if c.is_ascii_digit() => {
                        // Scan until we hit a non-digit
                        self.buf.skip_while(char::is_ascii_digit);
                        // Is it a '.'?
                        if self.buf.consume(".") {
                            self.buf.skip_while(char::is_ascii_digit);
                        }
                        let content = self.buf.from_checkpoint(&start);
                        Some(Ok((
                            pos,
                            Token::LitNumber(content.to_string()),
                            pos + content.len(),
                        )))
                    }

                    // Identifier
                    c if c.is_ascii_alphabetic() => {
                        self.buf.skip_while(|c| {
                            c.is_ascii_alphabetic() || c.is_ascii_digit() || *c == '_'
                        });
                        let content = self.buf.from_checkpoint(&start).to_string();
                        Some(Ok((pos, Token::Identifier(content), self.buf.location)))
                    }
                    unexpected => Some(Err(Error::Unexpected(unexpected))),
                }
            }
        }
    }
}

// Peekable<CharIndices> almost gives us everything we need, but doesn't expose
//  CharIndices::as_str which we need for lookahead.
#[derive(Clone)]
struct Buffer<'input> {
    source: &'input str,

    // Offset of current start in `source`
    location: usize,
}

impl<'input> Buffer<'input> {
    pub fn new(source: &'input str) -> Self {
        Self {
            source,
            location: 0,
        }
    }

    pub fn is_not_empty(&self) -> bool {
        self.location < self.source.len()
    }

    pub fn is_empty(&self) -> bool {
        !self.is_not_empty()
    }

    pub fn remaining(&self) -> &str {
        &self.source[self.location..]
    }

    pub fn current(&self) -> Option<(usize, char)> {
        match self.is_empty() {
            true => None,
            false => Some((
                self.location,
                self.remaining().chars().next().expect("a character"),
            )),
        }
    }

    pub fn pop(&mut self) -> Option<(usize, char)> {
        let ret = self.current();
        if let Some((_, c)) = ret {
            self.location += c.len_utf8();
        }
        ret
    }

    /// Returns true if the buffer current starts with needle, case-insensitive
    pub fn matches(&self, needle: &str) -> bool {
        let rem = self.remaining();
        if rem.len() < needle.len() {
            return false;
        }
        // This slice is safe thanks to the check above
        rem[..needle.len()].eq_ignore_ascii_case(needle)
    }

    pub fn skip_while<P: Fn(&char) -> bool>(&mut self, pred: P) {
        while self.is_not_empty() && pred(&self.current().unwrap().1) {
            _ = self.pop();
        }
    }

    /// If buffer starts with `needle` (ignoring ASCII case), then the buffer is
    ///  advanced so that it no longer starts with `needle` and returns `true`.
    /// If buffer does not start with `needle`, it remains unchanged.
    pub fn consume(&mut self, needle: &str) -> bool {
        if self.matches(needle) {
            self.location += needle.len();
            true
        } else {
            false
        }
    }

    pub fn consume1(&mut self, needle: char) -> bool {
        if let Some((_, c)) = self.current()
            && c == needle
        {
            self.location += c.len_utf8();
            true
        } else {
            false
        }
    }

    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint(self.location)
    }

    /// Slices from checkpoint up to (but not including) the current location
    pub fn from_checkpoint(&self, chk: &Checkpoint) -> &str {
        &self.source[chk.0..self.location]
    }
}

struct Checkpoint(usize);

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_tokens(expr: &str, expected: &[Result<Token, Error>]) {
        let lexer = Lexer::new(expr);
        let actual: Vec<Result<Token, Error>> = lexer.map(|res| res.map(|(_, t, _)| t)).collect();
        assert_eq!(actual, expected);
    }

    #[test]
    fn numbers() {
        assert_tokens("123", &[Ok(Token::LitNumber("123".to_string()))]);
        assert_tokens("1.2", &[Ok(Token::LitNumber("1.2".to_string()))]);
        assert_tokens(".12", &[Ok(Token::LitNumber(".12".to_string()))]);
        assert_tokens("12.", &[Ok(Token::LitNumber("12.".to_string()))]);
    }

    #[test]
    fn double_quote_string_literal() {
        assert_tokens(
            r#" "Jim's place" "#,
            &[Ok(Token::LitString("Jim's place".to_string()))],
        );
    }

    #[test]
    fn single_quote_string_literal() {
        assert_tokens(
            r#" 'a "quotation" for you ' "#,
            &[Ok(Token::LitString("a \"quotation\" for you ".to_string()))],
        );
    }

    #[test]
    fn identifiers() {
        assert_tokens("aB17", &[Ok(Token::Identifier("aB17".to_string()))]);
        assert_tokens(
            "TBL_1->FIELD_1",
            &[
                Ok(Token::Identifier("TBL_1".to_string())),
                Ok(Token::Arrow),
                Ok(Token::Identifier("FIELD_1".to_string())),
            ],
        );
    }
}

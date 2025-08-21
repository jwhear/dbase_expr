#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    LitNumber,
    LitDoubleQuote,
    LitSingleQuote,
    Identifier,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    Unexpected(char),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unexpected(c) => write!(f, "Unexpected token '{c}'"),
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
        self.buf.skip_while(|(_, c)| c.is_whitespace());

        // This clone is not expensive, just copying a pointer and a usize
        //let start = self.buf.clone();
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
                                // Must be a number without leading digits
                                todo!()
                            }
                        ]
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

    pub fn skip_while<P: Fn((usize, char)) -> bool>(&mut self, pred: P) {
        while self.is_not_empty() && pred(self.current().unwrap()) {
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
}

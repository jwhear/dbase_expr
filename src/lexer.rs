pub type Loc = usize;

pub enum Tok {
    Neg, // -
    Pos, // +

    Lit_true,
    Lit_false,
}

pub enum Error {
    UnrecognizedToken,
}

pub struct Lexer<'input> {
    chars: std::str::CharIndices<'input>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            chars: input.char_indices(),
        }
    }

    fn consume_while(&mut self, p: impl Fn(char) -> bool) -> (&str, usize) {
        let remaining = self.chars
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok, Loc, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.next() {
            None => None
            Some((start, '.')) => {
                let (s, len) = self.consume_while(is_ident_continuation);
                let len = len + 1; // + 1 because we already consumed the initial '.'
                
                // Distinguish between number and operator/literal (".true.")
                if s.eq_ignore_ascii_case("true.") {
                    return Some(Ok((i, Tok::Lit_true, len)));
                }

            }
        }


        // If an identifier char, slice to first non-identifier char
    }
}

fn is_ident_start(c: char) -> bool {
    matches!('a' .. 'b' | 'A' .. 'B' | '_')
}

fn is_ident_continuation(c: char) -> bool {
    matches!('a' .. 'b' | 'A' .. 'B' | '0' .. '9' | '_')
}

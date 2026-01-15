use crate::codebase_functions::CodebaseFunction;
use crate::lex::{Error as LexerError, Lexer, Token, TokenType};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Contain,
    And,
    Or,
    Exp,
}

impl TryFrom<Token> for BinaryOp {
    type Error = Error;
    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.ty {
            TokenType::Plus => Ok(BinaryOp::Add),
            TokenType::Minus => Ok(BinaryOp::Sub),
            TokenType::Asterisk => Ok(BinaryOp::Mul),
            TokenType::ForwardSlash => Ok(BinaryOp::Div),
            TokenType::Equals => Ok(BinaryOp::Eq),
            TokenType::NotEquals => Ok(BinaryOp::Ne),
            TokenType::LT => Ok(BinaryOp::Lt),
            TokenType::LTE => Ok(BinaryOp::Le),
            TokenType::GT => Ok(BinaryOp::Gt),
            TokenType::GTE => Ok(BinaryOp::Ge),
            TokenType::Dollar => Ok(BinaryOp::Contain),
            TokenType::And => Ok(BinaryOp::And),
            TokenType::Or => Ok(BinaryOp::Or),
            TokenType::DoubleAsterisk => Ok(BinaryOp::Exp),
            _ => Err(Error::UnexpectedToken(value)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConcatOp {
    Add,
    Sub,
}

impl ConcatOp {
    pub fn get_op(&self) -> &BinaryOp {
        match self {
            ConcatOp::Add => &BinaryOp::Add,
            ConcatOp::Sub => &BinaryOp::Sub,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'input> {
    BoolLiteral(bool),
    NumberLiteral(&'input [u8]),
    StringLiteral(&'input [u8]),
    Field {
        alias: Option<&'input [u8]>,
        name: &'input [u8],
    },
    FunctionCall {
        name: CodebaseFunction,
        args: ArgList,
    },
    BinaryOperator(ExpressionId, BinaryOp, ExpressionId),
    UnaryOperator(UnaryOp, ExpressionId),
    /// Lengthy concatenation is a common pattern `a+b+c+d+chr(34)+...`
    /// These sequences result in a deep tree of BinaryOps that can cause the
    ///  translate and evaluation code to blow out their stacks. We can simplify
    ///  to a single op and an arg list.
    Sequence(ArgList, ConcatOp),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ExpressionId(usize);

/// No Codebase function takes an unlimited number of arguments.
/// The biggest is DATETIME which takes up to six.
/// But Sequence (an optimization) effectively takes an an unlimited number.
/// Note: when parsing a list we can utilize a single scratch Vec like so:
///   stack frame 1:
///     parse call expression
///     loop and recurse to parse arguments
///     stack frame 2:
///       parse arguments
///       push_args(drain from scratch buf)
///       scratch buf is now "reset" to where it was before we recursed
///     parse more arguments...
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ArgList {
    start: usize,
    len: usize,
}

pub struct ParseTree<'input> {
    /// All expressions are stored in a flat list. References are via ExpressionId.
    expressions: Vec<Expression<'input>>,
    /// All argument lists are stored flat packed
    arg_lists: Vec<ExpressionId>,
}

impl<'input> ParseTree<'input> {
    pub fn push_expr(&mut self, expr: Expression<'input>) -> ExpressionId {
        let id = ExpressionId(self.expressions.len());
        self.expressions.push(expr);
        id
    }

    pub fn push_args(&mut self, ids: impl ExactSizeIterator<Item = ExpressionId>) -> ArgList {
        let start = self.arg_lists.len();
        let len = ids.len();
        self.arg_lists.extend(ids);
        ArgList { start, len }
    }

    #[inline]
    pub fn get_expr_unchecked(&self, id: ExpressionId) -> &Expression<'input> {
        &self.expressions[id.0]
    }

    #[inline]
    pub fn get_expr(&self, id: ExpressionId) -> Option<&Expression<'input>> {
        self.expressions.get(id.0)
    }

    #[inline]
    pub fn get_args(&self, list: &ArgList) -> &[ExpressionId] {
        &self.arg_lists[list.start..list.start + list.len]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    /// Returned when the input is empty (or just whitespace)
    NoExpression,
    Lexical(LexerError),
    MissingCloseParen,
    UnexpectedToken(Token),
    UnexpectedEof,
    Other(String),
}

impl From<LexerError> for Error {
    fn from(value: LexerError) -> Self {
        Self::Lexical(value)
    }
}

pub fn parse<'input>(input: &'input str) -> Result<(ParseTree<'input>, ExpressionId), Error> {
    let mut lexer = Lexer::new(input.as_bytes());
    let mut arg_scratch = Vec::with_capacity(100);
    let mut pt = ParseTree::<'input> {
        expressions: Vec::with_capacity(32),
        arg_lists: Vec::with_capacity(32),
    };

    let root_exp = parse_binary_op(&mut lexer, &mut pt, &mut arg_scratch, 0)?;
    let root = pt.push_expr(root_exp);
    Ok((pt, root))
}

fn parse_binary_op<'input>(
    lexer: &mut Lexer<'input>,
    tree: &mut ParseTree<'input>,
    scratch: &mut Vec<ExpressionId>,
    min_binding_power: u8,
) -> Result<Expression<'input>, Error> {
    let lhs = lexer.next_token()?.ok_or(Error::NoExpression)?;
    let mut lhs = match lhs {
        // Open paren: parse the internal expression and expect a closing paren
        Token {
            ty: TokenType::ParenLeft,
            ..
        } => {
            let lhs = parse_binary_op(lexer, tree, scratch, 0)?;
            if let Ok(Some(tok)) = lexer.next_token()
                && tok.ty != TokenType::ParenRight
            {
                return Err(Error::MissingCloseParen);
            }
            Ok(lhs)
        }
        // Prefix '-'
        Token {
            ty: TokenType::Minus,
            ..
        } => {
            let ((), pow) = prefix_binding(TokenType::Minus).unwrap();
            let rhs = parse_binary_op(lexer, tree, scratch, pow)?;
            let rhs = tree.push_expr(rhs);
            Ok(Expression::UnaryOperator(UnaryOp::Neg, rhs))
        }
        // Prefix '.NOT.'
        Token {
            ty: TokenType::Not, ..
        } => {
            let ((), pow) = prefix_binding(TokenType::Not).unwrap();
            let rhs = parse_binary_op(lexer, tree, scratch, pow)?;
            let rhs = tree.push_expr(rhs);
            Ok(Expression::UnaryOperator(UnaryOp::Not, rhs))
        }
        // Numbers, strings, and boolean literals
        tok if is_literal(&tok) => parse_literal(lexer, &tok),
        // Either a field reference or a function call
        tok if tok.ty == TokenType::Identifier => {
            // If next token is a left paren, it's a function call
            let mut peeker = lexer.clone();
            if let Ok(Some(Token {
                ty: TokenType::ParenLeft,
                ..
            })) = peeker.next_token()
            {
                parse_fn_call(lexer, tree, scratch, &tok)
            } else {
                // otherwise a field reference
                parse_field_ref(lexer, &tok)
            }
        }
        unexpected => Err(Error::UnexpectedToken(unexpected)),
    }?;

    // now that we have our left side, expect a series of operators or EOF
    loop {
        let mut peeker = lexer.clone();
        let op = match peeker.next_token()? {
            None => break,
            Some(op) => op,
        };

        //TODO if op is the same as previous, make it a sequence

        if let Some((l_pow, r_pow)) = infix_binding(op.ty) {
            if l_pow < min_binding_power {
                break;
            }
            // We've only peeked the op token, consume it now
            _ = lexer.next_token()?;

            let lhs_id = tree.push_expr(lhs);
            let rhs = parse_binary_op(lexer, tree, scratch, r_pow)?;
            let rhs_id = tree.push_expr(rhs);
            lhs = Expression::BinaryOperator(lhs_id, op.try_into()?, rhs_id);
            continue;
        }

        break;
    }

    Ok(lhs)
}

fn is_literal(token: &Token) -> bool {
    matches!(
        token.ty,
        TokenType::Number
            | TokenType::StringSingleQuote
            | TokenType::StringDoubleQuote
            | TokenType::True
            | TokenType::False
    )
}

fn parse_literal<'input>(
    lexer: &Lexer<'input>,
    token: &Token,
) -> Result<Expression<'input>, Error> {
    match token.ty {
        TokenType::True => Ok(Expression::BoolLiteral(true)),
        TokenType::False => Ok(Expression::BoolLiteral(false)),
        TokenType::Number => Ok(Expression::NumberLiteral(lexer.contents(token))),
        TokenType::StringSingleQuote | TokenType::StringDoubleQuote => {
            Ok(Expression::StringLiteral(lexer.contents(token)))
        }
        _ => Err(Error::Other(format!(
            "Unexpected token in parse_literal: {:?}",
            token.ty
        ))),
    }
}

// Handles `field` and `table->field`
fn parse_field_ref<'input>(
    lexer: &mut Lexer<'input>,
    token: &Token,
) -> Result<Expression<'input>, Error> {
    // Peek for an Arrow
    let mut peeker = lexer.clone();
    if let Ok(Some(Token {
        ty: TokenType::Arrow,
        ..
    })) = peeker.next_token()
    {
        _ = lexer.next_token()?;
        let field = lexer.next_token()?.ok_or(Error::UnexpectedEof)?;

        if field.ty == TokenType::Identifier {
            Ok(Expression::Field {
                alias: Some(lexer.contents(token)),
                name: lexer.contents(&field),
            })
        } else {
            Err(Error::UnexpectedToken(field))
        }
    } else {
        Ok(Expression::Field {
            alias: None,
            name: lexer.contents(&token),
        })
    }
}

fn parse_fn_call<'input>(
    lexer: &mut Lexer<'input>,
    tree: &mut ParseTree<'input>,
    scratch: &mut Vec<ExpressionId>,
    name_token: &Token,
) -> Result<Expression<'input>, Error> {
    let scratch_start = scratch.len();

    // Expect an open paren
    let mut t = lexer.next_token()?.ok_or(Error::UnexpectedEof)?;
    if t.ty != TokenType::ParenLeft {
        return Err(Error::UnexpectedToken(t));
    }

    // Some number of arguments
    let mut first = true;
    loop {
        t = lexer.next_token()?.ok_or(Error::UnexpectedEof)?;
        if t.ty == TokenType::ParenRight {
            break;
        }

        if !first {
            // Expect a comma
            if t.ty != TokenType::Comma {
                return Err(Error::UnexpectedToken(t));
            }
        } else {
            first = false;
        }

        // Expect an argument
        let arg = parse_binary_op(lexer, tree, scratch, 0)?;
        let arg_id = tree.push_expr(arg);
        scratch.push(arg_id);
    }

    let name = unsafe { std::str::from_utf8_unchecked(lexer.contents(name_token)) }
        .parse()
        .map_err(|s| Error::Other(s))?;
    let args = tree.push_args(scratch.drain(scratch_start..scratch.len()));

    Ok(Expression::FunctionCall { name, args })
}

// NOTE prefix_binding and infix_binding specify the "binding power" of the
//  various prefix and infix operators. Binding power is a more intuitive
//  version of "precedence": higher binding power means the operator binds
//  more tightly.
fn prefix_binding(ty: TokenType) -> Option<((), u8)> {
    match ty {
        TokenType::Plus | TokenType::Minus => Some(((), 90)),
        TokenType::Not => Some(((), 30)),
        _ => None,
    }
}

fn infix_binding(ty: TokenType) -> Option<(u8, u8)> {
    match ty {
        TokenType::Plus | TokenType::Minus => Some((50, 51)),
        TokenType::Asterisk | TokenType::ForwardSlash => Some((60, 61)),
        TokenType::DoubleAsterisk => Some((70, 71)),
        TokenType::Equals
        | TokenType::NotEquals
        | TokenType::LT
        | TokenType::LTE
        | TokenType::GT
        | TokenType::GTE
        | TokenType::Dollar => Some((40, 41)),
        TokenType::And => Some((20, 21)),
        TokenType::Or => Some((10, 11)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic1() {
        let (tree, root) = parse(r#"(1 + -2.0) <> 3."#).expect("a valid parse");
        let root = tree.get_expr(root).expect("a root expression");
        if let Expression::BinaryOperator(lhs, BinaryOp::Ne, rhs) = root {
            let lhs = tree.get_expr(*lhs).expect("lhs");
            let rhs = tree.get_expr(*rhs).expect("rhs");

            assert_eq!(*rhs, Expression::NumberLiteral(b"3."));

            if let Expression::BinaryOperator(lhs, BinaryOp::Add, rhs) = lhs {
                let lhs = tree.get_expr(*lhs).expect("lhs");
                let rhs = tree.get_expr(*rhs).expect("rhs");

                assert_eq!(*lhs, Expression::NumberLiteral(b"1"));

                if let Expression::UnaryOperator(UnaryOp::Neg, rhs) = rhs {
                    let rhs = tree.get_expr(*rhs).expect("rhs");
                    assert_eq!(*rhs, Expression::NumberLiteral(b"2.0"));
                }
            }
        } else {
            panic!("Expected a Ne root");
        }
    }
    #[test]
    fn basic2() {
        let (tree, root) = parse(r#"'Hello' + (" " + "World")"#).expect("a valid parse");
        let root = tree.get_expr(root).expect("a root expression");
        if let Expression::BinaryOperator(lhs, BinaryOp::Add, rhs) = root {
            let lhs = tree.get_expr(*lhs).expect("lhs");
            let rhs = tree.get_expr(*rhs).expect("rhs");

            assert_eq!(*lhs, Expression::StringLiteral(b"Hello"));

            if let Expression::BinaryOperator(lhs, BinaryOp::Add, rhs) = rhs {
                let lhs = tree.get_expr(*lhs).expect("lhs");
                let rhs = tree.get_expr(*rhs).expect("rhs");

                assert_eq!(*lhs, Expression::StringLiteral(b" "));
                assert_eq!(*rhs, Expression::StringLiteral(b"World"));
            }
        } else {
            panic!("Expected a Ne root");
        }
    }
    #[test]
    fn basic3() {
        let (tree, root) = parse(r#"'Hello ' + (F_NAME + CUST->L_NAME)"#).expect("a valid parse");
        let root = tree.get_expr(root).expect("a root expression");
        if let Expression::BinaryOperator(lhs, BinaryOp::Add, rhs) = root {
            let lhs = tree.get_expr(*lhs).expect("lhs");
            let rhs = tree.get_expr(*rhs).expect("rhs");

            assert_eq!(*lhs, Expression::StringLiteral(b"Hello "));

            if let Expression::BinaryOperator(lhs, BinaryOp::Add, rhs) = rhs {
                let lhs = tree.get_expr(*lhs).expect("lhs");
                let rhs = tree.get_expr(*rhs).expect("rhs");

                assert_eq!(
                    *lhs,
                    Expression::Field {
                        alias: None,
                        name: b"F_NAME"
                    }
                );
                assert_eq!(
                    *rhs,
                    Expression::Field {
                        alias: Some(b"CUST"),
                        name: b"L_NAME"
                    }
                );
            }
        } else {
            panic!("Expected a Ne root");
        }
    }
    #[test]
    fn fn_calls() {
        let (tree, root) = parse(r#"CTOD(TRIM("a date?"), TEST)"#).expect("a valid parse");
        let root = tree.get_expr(root).expect("a root expression");
        if let Expression::FunctionCall {
            name: CodebaseFunction::CTOD,
            args,
        } = root
        {
            let args = tree.get_args(args);
            assert_eq!(args.len(), 2);
            let first = tree.get_expr(args[0]).expect("first");
            let second = tree.get_expr(args[1]).expect("second");

            assert_eq!(
                *second,
                Expression::Field {
                    alias: None,
                    name: b"TEST"
                }
            );

            if let Expression::FunctionCall {
                name: CodebaseFunction::TRIM,
                args,
            } = first
            {
                let args = tree.get_args(args);
                assert_eq!(args.len(), 1);
                let first = tree.get_expr(args[0]).expect("first");

                assert_eq!(*first, Expression::StringLiteral(b"a date?"));
            }
        } else {
            panic!("Expected a Ne root");
        }
    }
}

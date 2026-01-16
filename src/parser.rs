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
pub enum UnaryOp {
    Not,
    Neg,
}
impl TryFrom<Token> for UnaryOp {
    type Error = Error;
    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.ty {
            TokenType::Minus => Ok(UnaryOp::Neg),
            TokenType::Not => Ok(UnaryOp::Not),
            _ => Err(Error::UnexpectedToken(value)),
        }
    }
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
    Sequence(ArgList, BinaryOp),
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

/// Instead of generating a tree of references or smart pointers (Rc), we'll
///  pack all expressions into a flat array and have them refer to each other by
///  an id (which is simply an index into that array).
///
/// Argument lists require a bit of special handling because we don't want
///  FunctionCall and ConcatOp to actually carry a Vec and own the subexpressions.
/// To handle these, all arguments get parsed as Expressions and stored in
///  [expressions], then their ids are stored in [arg_lists]. A particular argument
///  list is contiguous within [arg_lists]. For example, when this expression is
///  parsed:
///     fn_a(fn_b(1), 2)
///
/// This will get parsed into an expressions list:
///        id=0,                              id=1,     id=2,                              id=3
///  [Number(1), FunctionCall(fn_b, ArgList(0, 1)), Number(2), FunctionCall(fn_a, ArgList(1, 2))]
///
/// The two ArgLists reference spans of arg_lists:
///  [ExpressionId(0), ExpressionId(1), Expression(2)]
///
/// The So fn_a's ArgList (1,2) is the span at arg_lists[1..1+2], that is, ExpressionId's 1 and 2.
/// These in turn map to FunctionCall(fn_b) and Number(2), which are indeed its two arguments.
pub struct ParseTree<'input> {
    /// All expressions are stored in a flat list. References are via ExpressionId.
    expressions: Vec<Expression<'input>>,
    /// All argument lists are stored packed contiguously
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

    /// Get the ExpressionIds representing a particular argument list
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
    UnknownFunction(Vec<u8>),
    Other(String),
}

impl From<LexerError> for Error {
    fn from(value: LexerError) -> Self {
        Self::Lexical(value)
    }
}

pub fn parse<'input>(input: &'input str) -> Result<(ParseTree<'input>, Expression<'input>), Error> {
    let mut lexer = Lexer::new(input.as_bytes());
    let mut arg_scratch = Vec::with_capacity(100);
    let mut pt = ParseTree::<'input> {
        expressions: Vec::with_capacity(32),
        arg_lists: Vec::with_capacity(32),
    };

    let root = parse_binary_op(&mut lexer, &mut pt, &mut arg_scratch, 0)?;
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
        // Prefix '-' or '.NOT.'
        prefix if prefix.ty == TokenType::Minus || prefix.ty == TokenType::Not => {
            let ((), pow) = prefix_binding(prefix.ty).unwrap();
            let rhs = parse_binary_op(lexer, tree, scratch, pow)?;
            let rhs = tree.push_expr(rhs);
            Ok(Expression::UnaryOperator(prefix.try_into()?, rhs))
        }

        // Numbers, strings, and boolean literals
        tok if is_literal(&tok) => parse_literal(lexer, &tok),

        // Either a field reference or a function call
        tok if tok.ty == TokenType::Identifier => {
            // If next token is a left paren, it's a function call
            if let Ok(Some(Token {
                ty: TokenType::ParenLeft,
                ..
            })) = lexer.peek_token()
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
        let op = match lexer.peek_token()? {
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
    let Ok(Some(Token {
        ty: TokenType::Arrow,
        ..
    })) = lexer.peek_token()
    else {
        // If there's no Arrow then it's just a plain Field reference
        return Ok(Expression::Field {
            alias: None,
            name: lexer.contents(&token),
        });
    };

    let alias = Some(lexer.contents(token));
    // We've only peeked the Arrow, consume it now
    _ = lexer.next_token();
    let name = lexer.next_token()?.ok_or(Error::UnexpectedEof)?;

    if name.ty == TokenType::Identifier {
        Ok(Expression::Field {
            alias,
            name: lexer.contents(&name),
        })
    } else {
        Err(Error::UnexpectedToken(name))
    }
}

fn parse_fn_call<'input>(
    lexer: &mut Lexer<'input>,
    tree: &mut ParseTree<'input>,
    scratch: &mut Vec<ExpressionId>,
    name_token: &Token,
) -> Result<Expression<'input>, Error> {
    // We'll use the scratch buffer to accumulate the ExpressionIds of the args
    // At the end we'll drain them into the ParseTree. We do need to track where
    //  we're starting because we might be recursed multiple levels and prior
    //  stack frames are also using this buffer so it might not be empty.
    let scratch_start = scratch.len();

    // We've already popped the function name [name_token], so expect a ParenLeft
    if !lexer.consume(TokenType::ParenLeft)? {
        return Err(Error::UnexpectedEof);
    }

    // Zero or more arguments
    let mut first = true;
    loop {
        // Peek to see if we're getting a right paren to end the list
        let t = lexer.peek_token()?.ok_or(Error::UnexpectedEof)?;
        if t.ty == TokenType::ParenRight {
            // consume the ), we only peeked it
            _ = lexer.next_token();
            break;
        }

        // If not closing the list, it must be another argument.
        // If this isn't the first argument, expect a comment
        if !first {
            if !lexer.consume(TokenType::Comma)? {
                return Err(Error::UnexpectedToken(t));
            };
        } else {
            first = false;
        }

        // For the actual argument we'll leave the token(s) in the lexer and
        //  let parse_binary_op do the work. This is why we only peeked above.
        let arg = parse_binary_op(lexer, tree, scratch, 0)?;
        let arg_id = tree.push_expr(arg);
        scratch.push(arg_id);
    }

    // Get the name and map it into a CodebaseFunction
    let name = lexer.contents(name_token);
    let name = name
        .try_into()
        .map_err(|()| Error::UnknownFunction(name.to_vec()))?;
    let args = tree.push_args(scratch.drain(scratch_start..scratch.len()));

    Ok(Expression::FunctionCall { name, args })
}

// NOTE prefix_binding and infix_binding specify the "binding power" of the
//  various prefix and infix operators. Binding power is a more intuitive
//  version of "precedence": higher binding power means the operator binds
//  more tightly. So multiplication has a higher binding power than addition.
fn prefix_binding(ty: TokenType) -> Option<((), u8)> {
    match ty {
        TokenType::Plus | TokenType::Minus => Some(((), 90)),
        TokenType::Not => Some(((), 30)),
        _ => None,
    }
}

// NOTE for infix bindings we specify a left and right side of the operator,
//  this slight assymetry prevents us from getting stuck on ties and allows
//  us to change the associativity of operators, a feature not particularly
//  needed by dbase expressions.
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
        let Expression::BinaryOperator(lhs, BinaryOp::Ne, rhs) = root else {
            panic!("Expected a Ne, got a {root:?}")
        };

        let lhs = tree.get_expr(lhs).expect("lhs");
        let rhs = tree.get_expr(rhs).expect("rhs");

        assert_eq!(*rhs, Expression::NumberLiteral(b"3."));

        let Expression::BinaryOperator(lhs, BinaryOp::Add, rhs) = lhs else {
            panic!("Expected an Add, got a {lhs:?}")
        };

        let lhs = tree.get_expr(*lhs).expect("lhs");
        let rhs = tree.get_expr(*rhs).expect("rhs");

        assert_eq!(*lhs, Expression::NumberLiteral(b"1"));

        let Expression::UnaryOperator(UnaryOp::Neg, rhs) = rhs else {
            panic!("Expected a Neg, got a {rhs:?}")
        };
        let rhs = tree.get_expr(*rhs).expect("rhs");
        assert_eq!(*rhs, Expression::NumberLiteral(b"2.0"));
    }
    #[test]
    fn basic2() {
        let (tree, root) = parse(r#"'Hello' + (" " + "World")"#).expect("a valid parse");
        let Expression::BinaryOperator(lhs, BinaryOp::Add, rhs) = root else {
            panic!("Expected a Add, got a {root:?}")
        };

        let lhs = tree.get_expr(lhs).expect("lhs");
        let rhs = tree.get_expr(rhs).expect("rhs");

        assert_eq!(*lhs, Expression::StringLiteral(b"Hello"));

        let Expression::BinaryOperator(lhs, BinaryOp::Add, rhs) = rhs else {
            panic!("Expected an Add, got a {rhs:?}")
        };
        let lhs = tree.get_expr(*lhs).expect("lhs");
        let rhs = tree.get_expr(*rhs).expect("rhs");

        assert_eq!(*lhs, Expression::StringLiteral(b" "));
        assert_eq!(*rhs, Expression::StringLiteral(b"World"));
    }
    #[test]
    fn basic3() {
        let (tree, root) = parse(r#"'Hello ' + (F_NAME + CUST->L_NAME)"#).expect("a valid parse");
        let Expression::BinaryOperator(lhs, BinaryOp::Add, rhs) = root else {
            panic!("Expected a Add, got a {root:?}")
        };
        let lhs = tree.get_expr(lhs).expect("lhs");
        let rhs = tree.get_expr(rhs).expect("rhs");

        assert_eq!(*lhs, Expression::StringLiteral(b"Hello "));

        let Expression::BinaryOperator(lhs, BinaryOp::Add, rhs) = rhs else {
            panic!("Expected a Add, got {rhs:?}")
        };
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
    #[test]
    fn logical() {
        let (tree, root) = parse(r#"(.t. = .NOT..f.) .OR. .t."#).expect("a valid parse");
        let Expression::BinaryOperator(lhs, BinaryOp::Or, rhs) = root else {
            panic!("Expected an OR, got a {root:?}");
        };
        let lhs = tree.get_expr(lhs).expect("lhs");
        let rhs = tree.get_expr(rhs).expect("rhs");

        assert_eq!(*rhs, Expression::BoolLiteral(true));

        let Expression::BinaryOperator(lhs, BinaryOp::Eq, rhs) = lhs else {
            panic!("Expected a Eq, got a {lhs:?}");
        };
        let lhs = tree.get_expr(*lhs).expect("lhs");
        let rhs = tree.get_expr(*rhs).expect("rhs");

        assert_eq!(*lhs, Expression::BoolLiteral(true));
        let Expression::UnaryOperator(UnaryOp::Not, rhs) = rhs else {
            panic!("Expected a NOT")
        };
        let rhs = tree.get_expr(*rhs).expect("rhs");
        assert_eq!(*rhs, Expression::BoolLiteral(false));
    }
    #[test]
    fn fn_calls() {
        let (tree, root) = parse(r#"CTOD(TRIM("a date?"), TEST)"#).expect("a valid parse");
        let Expression::FunctionCall {
            name: CodebaseFunction::CTOD,
            args,
        } = root
        else {
            panic!("Expected a FunctionCall, got a {root:?}")
        };
        let args = tree.get_args(&args);
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

        let Expression::FunctionCall {
            name: CodebaseFunction::TRIM,
            args,
        } = first
        else {
            panic!("Expected a FunctionCall, got a {first:?}")
        };
        let args = tree.get_args(args);
        assert_eq!(args.len(), 1);
        let first = tree.get_expr(args[0]).expect("first");

        assert_eq!(*first, Expression::StringLiteral(b"a date?"));
    }
}

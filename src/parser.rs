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

    pub fn print_tree(
        &self,
        e: &Expression,
        f: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        match e {
            Expression::BoolLiteral(b) => write!(f, "{b}"),
            Expression::NumberLiteral(s) => {
                if let Ok(as_str) = std::str::from_utf8(s) {
                    write!(f, "{as_str}")
                } else {
                    write!(f, "Number({s:?})")
                }
            }
            Expression::StringLiteral(s) => {
                if let Ok(as_str) = std::str::from_utf8(s) {
                    write!(f, "\"{as_str}\"")
                } else {
                    write!(f, "String({s:?})")
                }
            }
            Expression::Field { alias: None, name } => {
                if let Ok(name) = std::str::from_utf8(name) {
                    write!(f, "{name}")
                } else {
                    write!(f, "Field({name:?})")
                }
            }
            Expression::Field {
                alias: Some(alias),
                name,
            } => {
                if let Ok(alias) = std::str::from_utf8(alias)
                    && let Ok(name) = std::str::from_utf8(name)
                {
                    write!(f, "{alias}->{name}")
                } else {
                    write!(f, "Field({alias:?}, {name:?})")
                }
            }
            Expression::UnaryOperator(UnaryOp::Not, child) => {
                write!(f, "(NOT ")?;
                self.print_tree(self.get_expr_unchecked(*child), f)?;
                write!(f, ")")
            }
            Expression::UnaryOperator(UnaryOp::Neg, child) => {
                write!(f, "(-")?;
                self.print_tree(self.get_expr_unchecked(*child), f)?;
                write!(f, ")")
            }
            Expression::BinaryOperator(l, op, r) => {
                write!(f, "(")?;
                self.print_tree(self.get_expr_unchecked(*l), f)?;
                write!(
                    f,
                    "{}",
                    match op {
                        BinaryOp::Add => "+",
                        BinaryOp::Sub => "-",
                        BinaryOp::Mul => "*",
                        BinaryOp::Div => "/",
                        BinaryOp::Exp => "^",
                        BinaryOp::Eq => "=",
                        BinaryOp::Ne => "<>",
                        BinaryOp::Lt => "<",
                        BinaryOp::Le => "<=",
                        BinaryOp::Gt => ">",
                        BinaryOp::Ge => ">=",
                        BinaryOp::Contain => "$",
                        BinaryOp::Or => " OR ",
                        BinaryOp::And => " AND ",
                    }
                )?;
                self.print_tree(self.get_expr_unchecked(*r), f)?;
                write!(f, ")")
            }
            Expression::FunctionCall { name, args } => {
                write!(f, "{name}(")?;
                let mut first = true;
                for arg in self.get_args(args) {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    self.print_tree(self.get_expr_unchecked(*arg), f)?;
                }
                write!(f, ")")
            }
            Expression::Sequence(args, op) => {
                write!(f, "({}", if *op == BinaryOp::Add { "+" } else { "-" })?;
                for arg in self.get_args(args) {
                    write!(f, " ")?;
                    self.print_tree(self.get_expr_unchecked(*arg), f)?;
                }
                write!(f, ")")
            }
        }
    }
}

pub struct TreePrinter<'input>(pub ParseTree<'input>, pub Expression<'input>);

impl<'input> std::fmt::Display for TreePrinter<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.print_tree(&self.1, f)
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

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoExpression => write!(f, "Empty input"),
            Self::Lexical(l) => write!(f, "Lexical error: {l}"),
            Self::MissingCloseParen => write!(f, "Missing closing parenthesis"),
            Self::UnexpectedToken(t) => write!(f, "Unexpected token, got {t:?}"),
            Self::UnexpectedEof => write!(f, "Unexpected end of input"),
            Self::UnknownFunction(name) => {
                if let Ok(name) = std::str::from_utf8(name) {
                    write!(f, "Unknown function '{name}'")
                } else {
                    write!(f, "Unknown function: {name:?}")
                }
            }
            Self::Other(msg) => write!(f, "{msg}"),
        }
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

    // Make sure we've completely parsed the input
    if let Ok(Some(tok)) = lexer.next_token() {
        Err(Error::UnexpectedToken(tok))
    } else {
        Ok((pt, root))
    }
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
        let op_tok = match lexer.peek_token()? {
            None => break,
            Some(op) => op,
        };

        let Some((l_pow, r_pow)) = infix_binding(op_tok.ty) else {
            break;
        };

        if l_pow < min_binding_power {
            break;
        }

        let op: BinaryOp = op_tok.try_into()?;

        // Consume the operator token
        _ = lexer.next_token()?;

        // Special handling for Add/Sub sequences
        let seq_binding_power = infix_binding(TokenType::Plus).unwrap().0;
        if matches!(op, BinaryOp::Add | BinaryOp::Sub) && min_binding_power <= seq_binding_power {
            let seq_op = op;
            let scratch_start = scratch.len();
            scratch.push(tree.push_expr(lhs));

            // Parse the initial RHS for this operator
            let mut rhs = parse_binary_op(lexer, tree, scratch, r_pow)?;
            scratch.push(tree.push_expr(rhs));

            // Collect additional RHS if the next operator matches exactly
            // (same op, sufficient binding power)
            loop {
                let next_op_tok = match lexer.peek_token()? {
                    None => break,
                    Some(t) => t,
                };

                let Some((next_l_pow, next_r_pow)) = infix_binding(next_op_tok.ty) else {
                    break;
                };

                if next_l_pow < min_binding_power || next_op_tok.ty != op_tok.ty {
                    break;
                }

                // Only continue the sequence if it's the exact same op
                let next_op: BinaryOp = next_op_tok.try_into()?;
                if next_op != seq_op {
                    break;
                }
                _ = lexer.next_token()?;

                // Parse the next RHS
                rhs = parse_binary_op(lexer, tree, scratch, next_r_pow)?;
                scratch.push(tree.push_expr(rhs));
            }

            // If just the two operands, use a BinaryOperator
            if let [l_id, r_id] = scratch[scratch_start..] {
                lhs = Expression::BinaryOperator(l_id, seq_op, r_id);
                scratch.truncate(scratch_start); // clean up my usage
            } else {
                let arg_list = tree.push_args(scratch.drain(scratch_start..));
                lhs = Expression::Sequence(arg_list, seq_op);
            }
        } else {
            // Standard binary operator handling for non-Add/Sub ops
            let lhs_id = tree.push_expr(lhs);
            let rhs = parse_binary_op(lexer, tree, scratch, r_pow)?;
            let rhs_id = tree.push_expr(rhs);
            lhs = Expression::BinaryOperator(lhs_id, op, rhs_id);
        }
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
        scratch.push(tree.push_expr(arg));
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
    #[test]
    fn sequence() {
        let (tree, root) = parse(r#"a+b+c+d*e+f"#).expect("a valid parse");
        let Expression::Sequence(args, BinaryOp::Add) = root else {
            panic!("Expect a Sequence, got a {root:?}")
        };
        let args = tree.get_args(&args);
        assert_eq!(args.len(), 5); // a, b, c, (d+e), f

        assert_eq!(
            tree.get_expr(args[0]).expect("arg"),
            &Expression::Field {
                alias: None,
                name: b"a"
            }
        );
        assert_eq!(
            tree.get_expr(args[1]).expect("arg"),
            &Expression::Field {
                alias: None,
                name: b"b"
            }
        );
        assert_eq!(
            tree.get_expr(args[2]).expect("arg"),
            &Expression::Field {
                alias: None,
                name: b"c"
            }
        );
        assert_eq!(
            tree.get_expr(args[4]).expect("arg"),
            &Expression::Field {
                alias: None,
                name: b"f"
            }
        );

        // Arg 3 is a multiply
        let arg3 = tree.get_expr(args[3]).expect("arg 3");
        let Expression::BinaryOperator(lhs, BinaryOp::Mul, rhs) = arg3 else {
            panic!("Expected a Mul, got a {arg3:?}")
        };
        let lhs = tree.get_expr(*lhs).expect("lhs");
        let rhs = tree.get_expr(*rhs).expect("rhs");
        assert_eq!(
            *lhs,
            Expression::Field {
                alias: None,
                name: b"d"
            }
        );
        assert_eq!(
            *rhs,
            Expression::Field {
                alias: None,
                name: b"e"
            }
        );
    }
}

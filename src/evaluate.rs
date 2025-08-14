use core::str;
use std::fmt::Debug;

use chrono::Datelike;
use chrono::NaiveDate;

use crate::ast::{BinaryOp, Expression, UnaryOp};

use crate::codebase_functions::CodebaseFunction as F;

#[derive(Clone, PartialEq)]
pub enum Value {
    Str(String, usize),
    Memo(String),
    Bool(bool),
    Number(f64),
    Date(Option<NaiveDate>),
    Blob(Vec<u8>),
    Null,
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Str(s, len) => write!(f, "String: '{}', Length: {}", s.replace('\'', "''"), len),
            Value::Memo(s) => write!(f, "Memo: '{}'", s.replace('\'', "''")),
            Value::Bool(b) => write!(f, "Boolean: {}", if *b { ".T." } else { ".F." }),
            Value::Number(n) => write!(f, "Number: {}", n),
            Value::Date(d) => write!(
                f,
                "Date: {}",
                d.map_or_else(|| "NULL".to_string(), |f| f.format("%Y-%m-%d").to_string())
            ),
            Value::Blob(b) => write!(f, "BLOB({:?})", b),
            Value::Null => write!(f, ".NULL."),
        }
    }
}

pub type FieldValueGetter<'a> = &'a dyn Fn(&str) -> Option<Value>;

pub fn evaluate(expr: &Expression, get: FieldValueGetter) -> Result<Value, String> {
    #[derive(Debug)]
    enum EvalState<'a> {
        Expr(&'a Expression),
        Unary {
            op: UnaryOp,
        },
        BinaryLeft {
            op: BinaryOp,
            rhs: &'a Expression,
        },
        BinaryRight {
            op: BinaryOp,
            left: Value,
        },
        Function {
            name: &'a F,
            args: std::slice::Iter<'a, Box<Expression>>,
            collected: Vec<Value>,
            total: usize,
        },
    }

    let mut stack: Vec<EvalState> = vec![EvalState::Expr(expr)];
    let mut results: Vec<Value> = vec![];

    while let Some(state) = stack.pop() {
        match state {
            EvalState::Expr(e) => match e {
                Expression::BoolLiteral(b) => results.push(Value::Bool(*b)),

                Expression::NumberLiteral(s) => results.push(
                    s.parse::<f64>()
                        .map(Value::Number)
                        .map_err(|e| e.to_string())?,
                ),

                Expression::SingleQuoteStringLiteral(s)
                | Expression::DoubleQuoteStringLiteral(s) => {
                    results.push(Value::Str(s.clone(), s.len()))
                }

                Expression::Field { name, .. } => match get(name) {
                    Some(Value::Str(s, len)) => {
                        let padded = if s.len() < len {
                            let mut padded = s.to_string();
                            padded.extend(std::iter::repeat_n(' ', len - s.len()));
                            padded
                        } else {
                            s.chars().take(len).collect()
                        };
                        results.push(Value::Str(padded, len))
                    }
                    Some(v) => results.push(v),
                    None => return Err(format!("Field '{}' not found in row", name)),
                },

                Expression::UnaryOperator(op, expr) => {
                    stack.push(EvalState::Unary { op: *op });
                    stack.push(EvalState::Expr(expr));
                }

                Expression::BinaryOperator(lhs, op, rhs) => {
                    stack.push(EvalState::BinaryLeft { op: *op, rhs });
                    stack.push(EvalState::Expr(lhs));
                }
                Expression::Sequence(exprs, op) => {
                    // Evaluate the whole expression and push it to the stack
                    let mut accum = evaluate(&exprs[0], get)?;
                    for e in &exprs[1..] {
                        let e = evaluate(e, get)?;
                        accum = eval_binary_op(op.get_op(), accum, e)?;
                    }
                    results.push(accum);
                }

                Expression::FunctionCall { name, args } => {
                    stack.push(EvalState::Function {
                        name,
                        args: args.iter(),
                        collected: vec![],
                        total: args.len(),
                    });
                }
            },

            EvalState::Unary { op } => {
                let val = results.pop().unwrap();
                let result = match (op, val) {
                    (UnaryOp::Not, Value::Bool(b)) => Value::Bool(!b),
                    (UnaryOp::Neg, Value::Number(n)) => Value::Number(-n),
                    _ => return Err("Invalid unary operation".to_string()),
                };
                results.push(result);
            }

            EvalState::BinaryLeft { op, rhs } => {
                let left = results.pop().unwrap();
                stack.push(EvalState::BinaryRight { op, left });
                stack.push(EvalState::Expr(rhs));
            }

            EvalState::BinaryRight { op, left } => {
                let right = results.pop().unwrap();
                results.push(eval_binary_op(&op, left, right)?);
            }

            EvalState::Function {
                name,
                mut args,
                mut collected,
                total,
            } => {
                if let Some(next) = args.next() {
                    // Push back function call with updated state
                    stack.push(EvalState::Function {
                        name,
                        args,
                        collected,
                        total,
                    });
                    stack.push(EvalState::Expr(next));
                } else {
                    // All args evaluated, now apply function
                    for _ in 0..total {
                        collected.push(results.pop().unwrap());
                    }
                    collected.reverse(); // restore original order

                    let result = eval_function(name, &collected, get)?;
                    results.push(result);
                }
            }
        }
    }

    assert_eq!(results.len(), 1);
    Ok(results.pop().unwrap())
}

fn eval_function(name: &F, args: &[Value], get: FieldValueGetter) -> Result<Value, String> {
    match name {
        F::LTRIM => match args {
            [Value::Str(s, len)] => Ok(Value::Str(s.trim_start().to_string(), *len)),
            [Value::Memo(s)] => Ok(Value::Memo(s.trim_start().to_string())),
            _ => Err("LTRIM expects a single string argument".to_string()),
        },

        F::TRIM | F::RTRIM => match args {
            [Value::Str(s, len)] => Ok(Value::Str(s.trim_end().to_string(), *len)),
            [Value::Memo(s)] => Ok(Value::Memo(s.trim_end().to_string())),
            _ => Err("RTRIM expects a single string argument".to_string()),
        },

        F::ALLTRIM => match args {
            [Value::Str(s, len)] => Ok(Value::Str(s.trim().to_string(), *len)),
            [Value::Memo(s)] => Ok(Value::Memo(s.trim().to_string())),
            _ => Err("ALLTRIM expects a single string argument".to_string()),
        },

        F::CHR => match args {
            [Value::Number(n)] => {
                let ch = (*n as u8) as char;
                Ok(Value::Str(ch.to_string(), 1))
            }
            _ => Err("CHR expects a single numeric argument".to_string()),
        },

        F::CTOD | F::STOD => match args {
            [Value::Str(s, _len)] => {
                if s.trim().is_empty() {
                    Ok(Value::Date(None))
                } else {
                    let fmt = if name == &F::CTOD {
                        "%m/%d/%y"
                    } else {
                        "%Y%m%d"
                    };
                    chrono::NaiveDate::parse_from_str(s, fmt)
                        .map(|d| Value::Date(Some(d)))
                        .map_err(|e| format!("Date parse error: {}", e))
                }
            }
            _ => Err(format!("{:?} expects a single string argument", name)),
        },

        F::DTOC | F::DTOS => match args {
            [Value::Date(d)] => {
                let fmt = if name == &F::DTOC {
                    "%m/%d/%y"
                } else {
                    "%Y%m%d"
                };
                let text = match d {
                    Some(date) => date.format(fmt).to_string(),
                    None => "".to_string(),
                };
                let len = if name == &F::DTOC { 10 } else { 8 };
                Ok(Value::Str(text, len))
            }
            [Value::Null] => {
                let len: usize = if name == &F::DTOC { 10 } else { 8 };
                Ok(Value::Str("".to_string(), len))
            }
            _ => Err("DTOC expects a date argument".to_string()),
        },

        F::DAY | F::MONTH | F::YEAR => match args {
            [Value::Date(d)] => match d {
                Some(date) => {
                    let result = match name {
                        F::DAY => date.day() as f64,
                        F::MONTH => date.month() as f64,
                        F::YEAR => date.year() as f64,
                        _ => unreachable!(),
                    };
                    Ok(Value::Number(result))
                }
                None => Ok(Value::Number(0.0)),
            },
            _ => Err(format!("{:?} expects a date argument", name)),
        },

        F::LEFT => match args {
            [Value::Str(s, _) | Value::Memo(s), Value::Number(n)] => {
                let n = *n as usize;
                Ok(Value::Str(s.chars().take(n).collect(), n))
            }
            [Value::Number(v), Value::Number(n)] => {
                let n = *n as usize;
                Ok(Value::Str(v.to_string().chars().take(n).collect(), n))
            }
            _ => Err("LEFT expects (string, number) or (number, number)".to_string()),
        },

        F::RIGHT => match args {
            [Value::Str(s, _) | Value::Memo(s), Value::Number(n)] => {
                Ok(Value::Str(right_str_n(s, *n), *n as usize))
            }
            [Value::Number(v), Value::Number(n)] => {
                Ok(Value::Str(right_str_n(&v.to_string(), *n), *n as usize))
            }
            _ => Err("RIGHT expects (string, number) or (number, number)".to_string()),
        },

        F::SUBSTR => match args {
            [
                Value::Str(s, _) | Value::Memo(s),
                Value::Number(start),
                Value::Number(len),
            ] => {
                let start = (*start as usize).saturating_sub(1);
                let len = *len as usize;
                let substr: String = s.chars().skip(start).take(len).collect();
                Ok(Value::Str(substr, len))
            }
            _ => Err("SUBSTR expects (string, start, length)".to_string()),
        },

        F::UPPER => match args {
            [Value::Str(s, len)] => Ok(Value::Str(s.to_uppercase(), *len)),
            [Value::Memo(s)] => Ok(Value::Memo(s.to_uppercase())),
            _ => Err("UPPER expects a single string argument".to_string()),
        },

        F::STR => match args {
            [Value::Number(n), Value::Number(len), Value::Number(dec)] => {
                let fmt = format!(
                    "{:width$.prec$}",
                    n,
                    width = *len as usize,
                    prec = *dec as usize
                );
                Ok(Value::Str(fmt.trim_end().to_string(), *len as usize))
            }
            _ => Err("STR expects (number, len, dec)".to_string()),
        },

        F::VAL => match args {
            [Value::Str(s, _) | Value::Memo(s)] => match s.trim().parse::<f64>() {
                Ok(v) => Ok(Value::Number(v)),
                Err(_) => {
                    if s.trim().chars().all(|c| c == 'F' || c == 'f') {
                        Ok(Value::Number(0.0)) // these are placeholders for float, we'll just use 0.0
                    } else {
                        Err(format!("VAL could not parse '{}' to a numeric value", s))
                    }
                }
            },
            _ => Err("VAL expects a string".to_string()),
        },

        F::DATE => Ok(Value::Date(Some(chrono::Local::now().naive_local().date()))),

        F::IIF => match args {
            [Value::Bool(cond), when_true, when_false] => Ok(if *cond {
                when_true.clone()
            } else {
                when_false.clone()
            }),
            _ => Err("IIF expects (boolean, true, false)".to_string()),
        },

        // DELETED() => __deleted
        F::DELETED => Ok(get("__deleted").unwrap_or(Value::Bool(false))),

        F::RECNO => Ok(get("RECNO5").unwrap_or(Value::Number(0.0))),

        F::Unknown(unsupported) => Err(format!("Unsupported function: {}", unsupported)),
    }
}

fn right_str_n(s: &str, n: f64) -> String {
    let n = n as usize;
    if n >= s.len() {
        s.to_string()
    } else {
        let start = s
            .char_indices()
            .rev()
            .nth(n - 1)
            .expect("already checked n against len")
            .0;

        s[start..].to_string()
    }
}

use Value::*;

fn eval_binary_op(op: &BinaryOp, left: Value, right: Value) -> Result<Value, String> {
    // Helper function to truncate a string without risking cutting a multi-byte character in half
    fn slice(s: &str, len: usize) -> &str {
        let char_count = len.min(s.chars().count());
        match s.char_indices().nth(char_count) {
            Some((i, _)) => &s[..i],
            None => s,
        }
    }
    match op {
        BinaryOp::Add => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (Value::Date(d), Value::Number(n)) => {
                match d {
                    Some(d) => {
                        // Add n days to date
                        let ndays = n as i64;
                        d.checked_add_signed(chrono::Duration::days(ndays))
                            .map(|d| Value::Date(Some(d)))
                            .ok_or("Date addition overflow".to_string())
                    }
                    None => Ok(Value::Date(None)), //null + n = null
                }
            }
            (Value::Number(n), Value::Date(d)) => {
                match d {
                    Some(d) => {
                        let ndays = n as i64;
                        d.checked_add_signed(chrono::Duration::days(ndays))
                            .map(|d| Value::Date(Some(d)))
                            .ok_or("Date addition overflow".to_string())
                    }
                    None => Ok(Value::Date(None)), //n + null = null
                }
            }
            (Value::Str(a, len_a), Value::Str(b, len_b)) => {
                let mut result = a.clone();
                result.push_str(&b);
                Ok(Value::Str(result, len_a + len_b))
            }
            (Value::Memo(a), Value::Memo(b) | Value::Str(b, _)) => {
                let mut result = a.clone();
                result.push_str(&b);
                Ok(Value::Memo(result))
            }
            _ => Err("Add: incompatible types".to_string()),
        },
        BinaryOp::Sub => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            (Value::Date(d), Value::Number(n)) => {
                match d {
                    Some(d) => {
                        // Subtract n days from date
                        let ndays = n as i64;
                        d.checked_sub_signed(chrono::Duration::days(ndays))
                            .map(|d| Value::Date(Some(d)))
                            .ok_or("Date subtraction overflow".to_string())
                    }
                    None => Ok(Value::Date(None)), //null - n = null
                }
            }
            (Value::Date(Some(d1)), Value::Date(Some(d2))) => {
                // Difference in days as float
                let duration = d1.signed_duration_since(d2);
                Ok(Value::Number(duration.num_days() as f64))
            }
            _ => Err("Sub: incompatible types".to_string()),
        },
        BinaryOp::Mul => match (left, right) {
            (Number(a), Number(b)) => Ok(Number(a * b)),
            _ => Err("Mul: incompatible types".to_string()),
        },
        BinaryOp::Div => match (left, right) {
            (Number(_), Number(0.0)) => Ok(Number(f64::NAN)),
            (Number(a), Number(b)) => Ok(Number(a / b)),
            _ => Err("Div: incompatible types".to_string()),
        },

        BinaryOp::Exp => match (left, right) {
            (Number(a), Number(b)) => Ok(Number(a.powf(b))),
            _ => Err("Exp: incompatible types".to_string()),
        },

        BinaryOp::Eq => Ok(Bool(match (left, right) {
            (Str(l, _) | Memo(l), Str(r, _) | Memo(r)) => l.starts_with(&r),
            (l, r) => l == r,
        })),
        BinaryOp::Ne => Ok(Bool(match (left, right) {
            (Str(l, _) | Memo(l), Str(r, _) | Memo(r)) => !l.starts_with(&r),
            (l, r) => l != r,
        })),
        BinaryOp::Lt => match (left, right) {
            (Number(a), Number(b)) => Ok(Bool(a < b)),
            (Str(a, _) | Memo(a), Str(b, len)) => Ok(Bool(slice(&a, len) < slice(&b, a.len()))),
            (Str(a, _) | Memo(a), Memo(b)) => Ok(Bool(a.as_str() < slice(&b, a.len()))),
            (Date(a), Date(b)) => Ok(Bool(a < b)),
            _ => Err("Lt: incompatible types".to_string()),
        },

        BinaryOp::Le => match (left, right) {
            (Number(a), Number(b)) => Ok(Bool(a <= b)),
            (Str(a, _) | Memo(a), Str(b, len)) => Ok(Bool(slice(&a, len) <= slice(&b, a.len()))),
            (Str(a, _) | Memo(a), Memo(b)) => Ok(Bool(a.as_str() <= slice(&b, a.len()))),
            (Date(a), Date(b)) => Ok(Bool(a <= b)),
            _ => Err("Le: incompatible types".to_string()),
        },

        BinaryOp::Gt => match (left, right) {
            (Number(a), Number(b)) => Ok(Bool(a > b)),
            (Str(a, _) | Memo(a), Str(b, len)) => Ok(Bool(slice(&a, len) > slice(&b, a.len()))),
            (Str(a, _) | Memo(a), Memo(b)) => Ok(Bool(a.as_str() > slice(&b, a.len()))),
            (Date(a), Date(b)) => Ok(Bool(a > b)),
            _ => Err("Gt: incompatible types".to_string()),
        },

        BinaryOp::Ge => match (left, right) {
            (Number(a), Number(b)) => Ok(Bool(a >= b)),
            (Str(a, _) | Memo(a), Str(b, len)) => Ok(Bool(slice(&a, len) >= slice(&b, a.len()))),
            (Str(a, _) | Memo(a), Memo(b)) => Ok(Bool(a.as_str() >= slice(&b, a.len()))),
            (Date(a), Date(b)) => Ok(Bool(a >= b)),
            _ => Err("Ge: incompatible types".to_string()),
        },

        BinaryOp::Contain => match (left, right) {
            (Str(needle, _) | Memo(needle), Str(haystack, _) | Memo(haystack)) => {
                Ok(Bool(haystack.contains(&needle)))
            }
            _ => Err("Contain: requires string operands".to_string()),
        },

        BinaryOp::And => {
            let left_bool = match left {
                Value::Bool(b) => b,
                Value::Number(n) => n != 0.0,
                _ => return Err("And: expected boolean or numeric operands".to_string()),
            };
            let right_bool = match right {
                Value::Bool(b) => b,
                Value::Number(n) => n != 0.0,
                _ => return Err("And: expected boolean or numeric operands".to_string()),
            };
            Ok(Value::Bool(left_bool && right_bool))
        }

        BinaryOp::Or => {
            let left_bool = match left {
                Value::Bool(b) => b,
                Value::Number(n) => n != 0.0,
                _ => return Err("Or: expected boolean or numeric operands".to_string()),
            };
            let right_bool = match right {
                Value::Bool(b) => b,
                Value::Number(n) => n != 0.0,
                _ => return Err("Or: expected boolean or numeric operands".to_string()),
            };
            Ok(Value::Bool(left_bool || right_bool))
        }
    }
}

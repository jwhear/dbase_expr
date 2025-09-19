use std::borrow::Cow;
use std::fmt::Debug;

use chrono::Datelike;
use chrono::NaiveDate;

use crate::ast::{BinaryOp, Expression, UnaryOp};

use crate::codebase_functions::CodebaseFunction as F;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    FieldNotFound(String),
    InvalidUnaryOp(UnaryOp),
    InvalidArguments(F, String),
    DateParseError(String),
    FloatParseError(String),
    DateAdditionOverflow,
    DateSubtractionOverflow,
    IncompatibleBinaryOp(BinaryOp, String),
    UnknownFunction(String),
    Other(String),
}

#[derive(Clone, PartialEq)]
pub enum Value {
    Str(String, usize),
    Memo(String),
    Bool(bool),
    Number(f64),
    Date(Option<NaiveDate>),
    DateParseError(String), // this is used to indicate a date parsing error but to match codebase, we treat it as julian day zero for equations
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
            Value::DateParseError(s) => write!(f, "DateParseError: {}", s),
            Value::Blob(b) => write!(f, "BLOB({:?})", b),
            Value::Null => write!(f, ".NULL."),
        }
    }
}

pub type FieldValueGetter<'a> = &'a dyn Fn(&str) -> Option<Value>;
pub type CustomFunctions<'a> = &'a dyn Fn(&str) -> Option<Expression>;

pub fn evaluate(
    expr: &Expression,
    get: FieldValueGetter,
    custom_functions: CustomFunctions,
) -> Result<Value, Error> {
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

                Expression::NumberLiteral(s) if s == "." => results.push(Value::Number(0.0)),
                Expression::NumberLiteral(s) => results.push(
                    s.parse::<f64>()
                        .map(Value::Number)
                        .map_err(|e| Error::FloatParseError(e.to_string()))?,
                ),

                Expression::StringLiteral(s) => results.push(Value::Str(s.clone(), s.len())),

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
                    None => return Err(Error::FieldNotFound(name.to_string())),
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
                    let mut accum = evaluate(&exprs[0], get, custom_functions)?;
                    for e in &exprs[1..] {
                        let e = evaluate(e, get, custom_functions)?;
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
                    _ => return Err(Error::InvalidUnaryOp(op)),
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

                    let result = eval_function(name, &collected, get, custom_functions)?;
                    results.push(result);
                }
            }
        }
    }

    assert_eq!(results.len(), 1);

    match results.pop().unwrap() {
        Value::DateParseError(err) => Err(Error::DateParseError(err)), // we allow DateParseError as a value for use within the expressions but it should not be passed back to the caller
        r => Ok(r),
    }
}

fn eval_function(
    name: &F,
    args: &[Value],
    get: FieldValueGetter,
    custom_functions: CustomFunctions,
) -> Result<Value, Error> {
    match name {
        F::LTRIM => match args {
            [Value::Str(s, len)] => Ok(Value::Str(s.trim_start().to_string(), *len)),
            [Value::Memo(s)] => Ok(Value::Memo(s.trim_start().to_string())),
            _ => Err(Error::InvalidArguments(
                name.clone(),
                "LTRIM expects a single string argument".to_string(),
            )),
        },

        F::TRIM | F::RTRIM => match args {
            [Value::Str(s, len)] => Ok(Value::Str(s.trim_end().to_string(), *len)),
            [Value::Memo(s)] => Ok(Value::Memo(s.trim_end().to_string())),
            _ => Err(Error::InvalidArguments(
                name.clone(),
                "RTRIM expects a single string argument".to_string(),
            )),
        },

        F::ALLTRIM => match args {
            [Value::Str(s, len)] => Ok(Value::Str(s.trim().to_string(), *len)),
            [Value::Memo(s)] => Ok(Value::Memo(s.trim().to_string())),
            _ => Err(Error::InvalidArguments(
                name.clone(),
                "ALLTRIM expects a single string argument".to_string(),
            )),
        },

        F::CHR => match args {
            [Value::Number(n)] => {
                let ch = (*n as u8) as char;
                Ok(Value::Str(ch.to_string(), 1))
            }
            _ => Err(Error::InvalidArguments(
                name.clone(),
                "CHR expects a single numeric argument".to_string(),
            )),
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
                    match chrono::NaiveDate::parse_from_str(s, fmt) {
                        Ok(date) => Ok(Value::Date(Some(date))),
                        Err(_) => Ok(Value::DateParseError(s.to_string())),
                    }
                }
            }
            _ => Err(Error::InvalidArguments(
                name.clone(),
                "CTOD expects a single string argument".to_string(),
            )),
        },

        F::DTOC | F::DTOS => match args {
            [Value::Date(d)] => {
                let (fmt, len) = if name == &F::DTOC {
                    ("%m/%d/%y", 10)
                } else {
                    ("%Y%m%d", 8)
                };
                let text = match d {
                    Some(date) => date.format(fmt).to_string(),
                    None => " ".repeat(len),
                };
                let len = if name == &F::DTOC { 10 } else { 8 };
                Ok(Value::Str(text, len))
            }
            [Value::Null] => {
                let len: usize = if name == &F::DTOC { 10 } else { 8 };
                Ok(Value::Str("".to_string(), len))
            }
            _ => Err(Error::InvalidArguments(
                name.clone(),
                "DTOC expects a date argument".to_string(),
            )),
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
            _ => Err(Error::InvalidArguments(
                name.clone(),
                format!("{:?} expects a date argument", name),
            )),
        },

        F::EMPTY => match args {
            [Value::Str(s, _) | Value::Memo(s)] => Ok(Value::Bool(s.trim().is_empty())),
            _ => Err(Error::InvalidArguments(
                name.clone(),
                "EMPTY expects a single string argument".to_string(),
            )),
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
            _ => Err(Error::InvalidArguments(
                name.clone(),
                "LEFT expects (string, number) or (number, number)".to_string(),
            )),
        },

        F::PADL => match args {
            [Value::Str(s, _) | Value::Memo(s), Value::Number(n)] => {
                let n = *n as usize;
                let mut padded = s.chars().take(n).collect::<String>();
                if padded.len() < n {
                    padded.extend(std::iter::repeat_n(' ', n - padded.len()));
                }
                Ok(Value::Str(padded, n))
            }
            _ => Err(Error::InvalidArguments(
                name.clone(),
                "PADL expects (string, number)".to_string(),
            )),
        },

        F::RIGHT => match args {
            [Value::Str(s, _) | Value::Memo(s), Value::Number(n)] => {
                Ok(Value::Str(right_str_n(s, *n), *n as usize))
            }
            [Value::Number(v), Value::Number(n)] => {
                Ok(Value::Str(right_str_n(&v.to_string(), *n), *n as usize))
            }
            _ => Err(Error::InvalidArguments(
                name.clone(),
                "RIGHT expects (string, number) or (number, number)".to_string(),
            )),
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
            _ => Err(Error::InvalidArguments(
                name.clone(),
                "SUBSTR expects (string, start, length)".to_string(),
            )),
        },

        F::UPPER => match args {
            [Value::Str(s, len)] => Ok(Value::Str(s.to_uppercase(), *len)),
            [Value::Memo(s)] => Ok(Value::Memo(s.to_uppercase())),
            _ => Err(Error::InvalidArguments(
                name.clone(),
                "UPPER expects a single string argument".to_string(),
            )),
        },

        F::STR => match args {
            [Value::Number(n)] => {
                let fmt = format!("{:width$.prec$}", n, width = 10, prec = 0); // DBASE defaults: https://www.dbase.com/downloads/dBLLanguageReference2.6.pdf
                Ok(Value::Str(fmt.trim_end().to_string(), 10))
            }
            [Value::Number(n), Value::Number(len), Value::Number(dec)] => {
                let len = (*len).max(0.0) as usize;
                let mut dec = (*dec).max(0.0) as usize;
                dec = dec.min(15);

                if len == 0 {
                    return Ok(Value::Str(String::new(), 0));
                }

                if len <= dec + 1 {
                    dec = (len.saturating_sub(2)).max(0); //to allow space for the '.', something like 2,1 doesn't make sense since there would be no space for the leading 0 so codebase just removes the dec
                }

                let fmt = format!("{:width$.prec$}", n, width = len, prec = dec);
                Ok(Value::Str(
                    if fmt.len() > len {
                        "*".repeat(len)
                    } else {
                        let text = fmt.trim_end().to_string();
                        format!("{:>width$}", text, width = len)
                    },
                    len,
                ))
            }
            _ => Err(Error::InvalidArguments(
                name.clone(),
                "STR expects (number, len, dec)".to_string(),
            )),
        },

        F::VAL => match args {
            [Value::Str(s, _) | Value::Memo(s)] => match s.trim().parse::<f64>() {
                Ok(v) => Ok(Value::Number(v)),
                Err(_) => {
                    if s.trim().chars().all(|c| c == 'F' || c == 'f') {
                        Ok(Value::Number(0.0)) // these are placeholders for float, we'll just use 0.0
                    } else {
                        Err(Error::InvalidArguments(
                            name.clone(),
                            format!("VAL could not parse '{}' to a numeric value", s),
                        ))
                    }
                }
            },
            _ => Err(Error::InvalidArguments(
                name.clone(),
                "VAL expects a string".to_string(),
            )),
        },

        F::DATE => Ok(Value::Date(Some(chrono::Local::now().naive_local().date()))),

        F::IIF => match args {
            [Value::Bool(cond), when_true, when_false] => {
                let chosen = if *cond { when_true } else { when_false };
                let value = match (when_true, when_false) {
                    (Value::Str(str_true, len_true), Value::Str(str_false, len_false)) => {
                        let max_len = *len_true.max(len_false); //get the max of the two because the length shouldn't depend on the values
                        let mut str_value = if *cond { str_true } else { str_false }.clone();
                        if str_value.len() < max_len {
                            str_value.extend(std::iter::repeat_n(' ', max_len - str_value.len()));
                        }
                        Value::Str(str_value, max_len)
                    }
                    _ => chosen.clone(),
                };
                Ok(value)
            }
            _ => Err(Error::InvalidArguments(
                name.clone(),
                "IIF expects (boolean, true, false)".to_string(),
            )),
        },

        // DELETED() => __deleted
        F::DELETED => Ok(get("__deleted").unwrap_or(Value::Bool(false))),

        F::RECNO => Ok(get("RECNO5").unwrap_or(Value::Number(0.0))),

        F::Unknown(unknown) => match custom_functions(unknown) {
            Some(v) => evaluate(&v, get, custom_functions),
            None => Err(Error::UnknownFunction(unknown.clone())),
        },
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

fn eval_binary_op(op: &BinaryOp, left: Value, right: Value) -> Result<Value, Error> {
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
                            .ok_or(Error::DateAdditionOverflow)
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
                            .ok_or(Error::DateAdditionOverflow)
                    }
                    None => Ok(Value::Date(None)), //n + null = null
                }
            }
            (Value::Str(a, len_a), Value::Str(b, len_b)) => {
                let mut result = a.clone();
                result.push_str(&b);
                Ok(Value::Str(result, len_a + len_b))
            }
            (Value::Str(a, _) | Value::Memo(a), Value::Str(b, _) | Value::Memo(b)) => {
                let mut result = a.clone();
                result.push_str(&b);
                Ok(Value::Memo(result))
            }
            (l, r) => Err(Error::IncompatibleBinaryOp(
                BinaryOp::Add,
                format!("Incompatible types: {:?} and {:?}", l, r),
            )),
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
                            .ok_or(Error::DateSubtractionOverflow)
                    }
                    None => Ok(Value::Date(None)), //null - n = null
                }
            }
            (Value::Date(Some(d1)), Value::Date(Some(d2))) => {
                // Difference in days as float
                let duration = d1.signed_duration_since(d2);
                Ok(Value::Number(duration.num_days() as f64))
            }
            (Value::Date(Some(d1)), Value::DateParseError(_)) => {
                // Difference in days as float
                Ok(Value::Number(days_since_jd0(d1) as f64))
            }
            (Value::DateParseError(_), Value::Date(Some(d1))) => {
                // Difference in days as float
                Ok(Value::Number(-days_since_jd0(d1) as f64))
            }
            (Value::DateParseError(_), Value::DateParseError(_)) => Ok(Value::Number(0.0)),
            _ => Err(Error::IncompatibleBinaryOp(
                BinaryOp::Sub,
                "Incompatible types".to_string(),
            )),
        },
        BinaryOp::Mul => match (left, right) {
            (Number(a), Number(b)) => Ok(Number(a * b)),
            _ => Err(Error::IncompatibleBinaryOp(
                BinaryOp::Mul,
                "Incompatible types".to_string(),
            )),
        },
        BinaryOp::Div => match (left, right) {
            (Number(_), Number(0.0)) => Ok(Number(f64::NAN)),
            (Number(a), Number(b)) => Ok(Number(a / b)),
            _ => Err(Error::IncompatibleBinaryOp(
                BinaryOp::Div,
                "Incompatible types".to_string(),
            )),
        },

        BinaryOp::Exp => match (left, right) {
            (Number(a), Number(b)) => Ok(Number(a.powf(b))),
            _ => Err(Error::IncompatibleBinaryOp(
                BinaryOp::Exp,
                "Incompatible types".to_string(),
            )),
        },

        BinaryOp::Eq | BinaryOp::Ne => Ok(Bool(match (left, right) {
            (Str(a, _), Memo(b) | Str(b, _)) => cmp_str(&a, &b, op),
            (l, r) => match op {
                BinaryOp::Eq => l == r,
                BinaryOp::Ne => l != r,
                _ => unreachable!(),
            },
        })),

        BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => match (left, right) {
            (Number(a), Number(b)) => Ok(Bool(cmp(a, b, op))),
            (Str(a, _) | Memo(a), Str(b, _) | Memo(b)) => Ok(Bool(cmp_str(&a, &b, op))),
            (Date(a), Date(b)) => Ok(Bool(cmp(&a, &b, op))),
            _ => Err(Error::IncompatibleBinaryOp(
                *op,
                "Incompatible types".to_string(),
            )),
        },

        BinaryOp::Contain => match (left, right) {
            (Str(needle, _) | Memo(needle), Str(haystack, _) | Memo(haystack)) => {
                Ok(Bool(haystack.contains(&needle)))
            }
            _ => Err(Error::IncompatibleBinaryOp(
                BinaryOp::Contain,
                "Contain requires string operands".to_string(),
            )),
        },

        BinaryOp::And => {
            let left_bool = match left {
                Value::Bool(b) => b,
                _ => {
                    return Err(Error::IncompatibleBinaryOp(
                        BinaryOp::And,
                        "Expected boolean operands".to_string(),
                    ));
                }
            };
            let right_bool = match right {
                Value::Bool(b) => b,
                _ => {
                    return Err(Error::IncompatibleBinaryOp(
                        BinaryOp::And,
                        "Expected boolean operands".to_string(),
                    ));
                }
            };
            Ok(Value::Bool(left_bool && right_bool))
        }

        BinaryOp::Or => {
            let left_bool = match left {
                Value::Bool(b) => b,
                Value::Number(n) => n != 0.0,
                _ => {
                    return Err(Error::IncompatibleBinaryOp(
                        BinaryOp::Or,
                        "Expected boolean or numeric operands".to_string(),
                    ));
                }
            };
            let right_bool = match right {
                Value::Bool(b) => b,
                Value::Number(n) => n != 0.0,
                _ => {
                    return Err(Error::IncompatibleBinaryOp(
                        BinaryOp::Or,
                        "Expected boolean or numeric operands".to_string(),
                    ));
                }
            };
            Ok(Value::Bool(left_bool || right_bool))
        }
    }
}

fn cmp<T: PartialOrd + PartialEq>(left: T, right: T, op: &BinaryOp) -> bool {
    match op {
        BinaryOp::Eq => left == right,
        BinaryOp::Ne => left != right,
        BinaryOp::Lt => left < right,
        BinaryOp::Le => left <= right,
        BinaryOp::Gt => left > right,
        BinaryOp::Ge => left >= right,
        op => panic!("Unexpected comparison operator: {:?}", op),
    }
}
fn cmp_str(a: &str, b: &str, op: &BinaryOp) -> bool {
    let b_adjusted = with_len(b, a.len());
    cmp(a, &*b_adjusted, op)
}
fn with_len<'s>(s: &'s str, len: usize) -> Cow<'s, str> {
    let char_count = len.min(s.chars().count());
    match s.char_indices().nth(char_count) {
        Some((i, _)) => Cow::Borrowed(&s[..i]), //slice down the string if it's too long
        None => {
            //pad it if it's too short
            if s.chars().count() < len {
                let mut padded = s.to_string();
                padded.extend(std::iter::repeat_n(' ', len - padded.chars().count()));
                Cow::Owned(padded)
            } else {
                Cow::Borrowed(s)
            }
        }
    }
}

pub fn julian_day(year: i32, month: u32, day: u32) -> i32 {
    // Julian day calculation (Fliegel & Van Flandern algorithm)
    let a = (14 - month as i32) / 12;
    let y = year + 4800 - a;
    let m = month as i32 + 12 * a - 3;
    day as i32 + ((153 * m + 2) / 5) + 365 * y + y / 4 - y / 100 + y / 400 - 32045
}

fn days_since_jd0(date: NaiveDate) -> i32 {
    julian_day(date.year(), date.month(), date.day()) + 1
}

#[cfg(test)]
mod tests {
    use super::*;

    const TRUE: Result<Value, Error> = Result::Ok(Value::Bool(true));

    fn eval(expr: &str) -> Result<Value, Error> {
        use crate::{ast, grammar::ExprParser};
        let parser = ExprParser::new();
        let value_lookup = |_: &str| -> Option<Value> { None };
        let custom_functions = |_: &str| -> Option<Expression> { None };
        match parser.parse(expr) {
            Ok(t) => {
                let t = ast::simplify(*t);
                evaluate(&t, &value_lookup, &custom_functions)
            }
            Err(e) => Err(Error::Other(format!("{e}"))),
        }
    }

    // Asserts if the expression does not produce an error
    fn assert_any_err(expr: &str) {
        if let Ok(v) = eval(expr) {
            panic!("Expected an error, got {v:?}");
        }
    }

    #[test]
    fn optional_digits() {
        // Trailing digits NOT optional
        assert_any_err("1. + 2 = 3.00");

        // Leading digits optional
        assert_eq!(eval(".1 + 0.1 = 000.2"), TRUE);

        // All digits optional!
        // 0.0 + 0.0 = 0.0
        //assert_eq!(eval(".+.=."), TRUE);
    }

    // This does not pass due to numeric precision issues with f64
    //TODO: implement using decimal type and uncomment; KOB-78
    #[test]
    fn precision() {
        assert_eq!(eval(".1 + 0.2"), Ok(Value::Number(0.3)));
        assert_eq!(eval(".1 + 0.2 = 000.3"), TRUE);
    }

    #[test]
    fn multiple_signs() {
        assert_eq!(eval("-2"), Ok(Value::Number(-2.0)));
        // Not allowed to stack signs; Codebase actually allows this but only
        //  because it's doing something very unexpected
        assert_any_err("--2"); // CB evals this to -2
        assert_any_err("---2"); // CB evals this to 2
        assert_any_err("-+-2"); // CB evals this to -2
    }

    #[test]
    fn implicit_number() {
        // Codebase evaluates these to 0, now an error
        assert_any_err("-");
        assert_any_err("+");
    }

    #[test]
    fn ambiguous_dots() {
        // This is fine because a decimal place requires trailing digits
        assert_eq!(eval("1=1.and.1=1"), TRUE);
    }

    #[test]
    fn addition_or_sign() {
        assert_eq!(eval("7+1=8"), TRUE);
    }
}

use std::fmt::Debug;

use chrono::Datelike;
use chrono::NaiveDate;

use crate::ast::{BinaryOp, Expression, UnaryOp};

#[derive(Clone, PartialEq)]
pub enum Value {
    Str(String, usize),
    Bool(bool),
    Number(f64),
    Date(NaiveDate),
    Blob(Vec<u8>),
    Null,
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Str(s, len) => write!(f, "String: '{}', Length: {}", s.replace('\'', "''"), len),
            Value::Bool(b) => write!(f, "Boolean: {}", if *b { ".T." } else { ".F." }),
            Value::Number(n) => write!(f, "Number: {}", n),
            Value::Date(d) => write!(f, "Date: {}", d.format("%Y-%m-%d")),
            Value::Blob(b) => write!(f, "BLOB({:?})", b),
            Value::Null => write!(f, ".NULL."),
        }
    }
}

pub type FieldValueGetter<'a> = &'a dyn Fn(&str) -> Option<Value>;

pub fn evaluate(expr: &Expression, get: FieldValueGetter) -> Result<Value, String> {
    match expr {
        Expression::BoolLiteral(b) => Ok(Value::Bool(*b)),

        Expression::NumberLiteral(s) => s
            .parse::<f64>()
            .map(Value::Number)
            .map_err(|e| e.to_string()),

        Expression::SingleQuoteStringLiteral(s) | Expression::DoubleQuoteStringLiteral(s) => {
            Ok(Value::Str(s.clone(), s.len()))
        }

        Expression::Field { name, .. } => match get(name) {
            Some(Value::Str(s, len)) => {
                let padded = if s.len() < len {
                    let mut padded = s.to_string();
                    padded.extend(std::iter::repeat(' ').take(len - s.len()));
                    padded
                } else {
                    s.chars().take(len).collect()
                };
                Ok(Value::Str(padded, len))
            }
            Some(v) => Ok(v),
            None => Err(format!("Field '{}' not found in row", name)),
        },

        Expression::UnaryOperator(op, expr) => {
            let v = evaluate(expr, get)?;
            match (op, v) {
                (UnaryOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
                (UnaryOp::Neg, Value::Number(n)) => Ok(Value::Number(-n)),
                _ => Err("Invalid unary operation".to_string()),
            }
        }

        Expression::BinaryOperator(lhs, op, rhs) => {
            let left = evaluate(lhs, get)?;
            let right = evaluate(rhs, get)?;
            eval_binary_op(op, left, right)
        }

        Expression::FunctionCall { name, args } => {
            let name_upper = name.to_uppercase();

            let eval_args: Result<Vec<Value>, String> =
                args.iter().map(|arg| evaluate(arg, get)).collect();

            let args = eval_args?;

            match name_upper.as_str() {
                "LTRIM" => match &args[..] {
                    [Value::Str(s, len)] => Ok(Value::Str(s.trim_end().to_string(), *len)),
                    _ => Err("LTRIM expects a single string argument".to_string()),
                },

                "RTRIM" => match &args[..] {
                    //TODO
                    [Value::Str(s, len)] => Ok(Value::Str(s.trim_start().to_string(), *len)),
                    _ => Err("RTRIM expects a single string argument".to_string()),
                },

                "ALLTRIM" => match &args[..] {
                    //TODO
                    [Value::Str(s, len)] => {
                        Ok(Value::Str(s.trim_start().trim_end().to_string(), *len))
                    }
                    _ => Err("ALLTRIM expects a single string argument".to_string()),
                },

                "CHR" => match &args[..] {
                    [Value::Number(n)] => {
                        let ch = (*n as u8) as char;
                        Ok(Value::Str(ch.to_string(), 1))
                    }
                    _ => Err("CHR expects a single numeric argument".to_string()),
                },

                "CTOD" | "STOD" => match &args[..] {
                    [Value::Str(s, _len)] => {
                        let fmt = if name_upper == "CTOD" {
                            "%m/%d/%y"
                        } else {
                            "%Y%m%d"
                        };
                        chrono::NaiveDate::parse_from_str(s, fmt)
                            .map(Value::Date)
                            .map_err(|e| format!("Date parse error: {}", e))
                    }
                    _ => Err(format!("{} expects a single string argument", name)),
                },

                "DTOC" => match &args[..] {
                    [Value::Date(d)] => {
                        let (fmt, len) = if args.len() == 2 {
                            ("%Y%m%d", 8)
                        } else {
                            ("%m/%d/%y", 10)
                        };
                        Ok(Value::Str(d.format(fmt).to_string(), len))
                    }
                    _ => Err("DTOC expects a date argument".to_string()),
                },

                "DAY" | "MONTH" | "YEAR" => match &args[..] {
                    [Value::Date(d)] => {
                        let result = match name_upper.as_str() {
                            "DAY" => d.day() as f64,
                            "MONTH" => d.month() as f64,
                            "YEAR" => d.year() as f64,
                            _ => unreachable!(),
                        };
                        Ok(Value::Number(result))
                    }
                    _ => Err(format!("{} expects a date argument", name_upper)),
                },

                "LEFT" => match &args[..] {
                    [Value::Str(s, _len), Value::Number(n)] => {
                        let n = *n as usize;
                        Ok(Value::Str(s.chars().take(n).collect(), n))
                    }
                    [Value::Number(v), Value::Number(n)] => {
                        let n = *n as usize;
                        Ok(Value::Str(v.to_string().chars().take(n).collect(), n))
                    }
                    _ => Err("LEFT expects (string, number) or (number, number)".to_string()),
                },

                "RIGHT" => match &args[..] {
                    [Value::Str(s, _len), Value::Number(n)] => {
                        Ok(Value::Str(right_str_n(&s, *n), *n as usize))
                    }
                    [Value::Number(v), Value::Number(n)] => {
                        Ok(Value::Str(right_str_n(&v.to_string(), *n), *n as usize))
                    }
                    _ => Err("RIGHT expects (string, number) or (number, number)".to_string()),
                },

                "SUBSTR" => match &args[..] {
                    [
                        Value::Str(s, _len),
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

                "UPPER" => match &args[..] {
                    [Value::Str(s, len)] => Ok(Value::Str(s.to_uppercase(), *len)),
                    _ => Err("UPPER expects a single string argument".to_string()),
                },

                "STR" => match &args[..] {
                    [Value::Number(n), Value::Number(len), Value::Number(dec)] => {
                        let fmt = format!(
                            "{:width$.prec$}",
                            n,
                            width = *len as usize,
                            prec = *dec as usize
                        );
                        Ok(Value::Str(fmt.trim().to_string(), *len as usize))
                    }
                    _ => Err("STR expects (number, len, dec)".to_string()),
                },

                "VAL" => match &args[..] {
                    [Value::Str(s, _len)] => s
                        .parse::<f64>()
                        .map(Value::Number)
                        .map_err(|e| e.to_string()),
                    _ => Err("VAL expects a string".to_string()),
                },

                "DATE" => Ok(Value::Date(chrono::Local::now().naive_local().date())),

                "IIF" => match &args[..] {
                    [Value::Bool(cond), when_true, when_false] => Ok(if *cond {
                        when_true.clone()
                    } else {
                        when_false.clone()
                    }),
                    _ => Err("IIF expects (boolean, true, false)".to_string()),
                },

                // DELETED() => __deleted
                "DELETED" => Ok(get("__deleted").unwrap_or(Value::Bool(false))),

                _ => Err(format!("Unsupported function: {}", name)),
            }
        }
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
    match op {
        BinaryOp::Add => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (Value::Date(d), Value::Number(n)) => {
                // Add n days to date
                let ndays = n as i64;
                d.checked_add_signed(chrono::Duration::days(ndays))
                    .map(Value::Date)
                    .ok_or("Date addition overflow".to_string())
            }
            (Value::Number(n), Value::Date(d)) => {
                let ndays = n as i64;
                d.checked_add_signed(chrono::Duration::days(ndays))
                    .map(Value::Date)
                    .ok_or("Date addition overflow".to_string())
            }
            (Value::Str(a, len_a), Value::Str(b, len_b)) => {
                let mut result = a.clone();
                result.push_str(&b);
                Ok(Value::Str(result, len_a + len_b))
            }
            _ => Err("Add: incompatible types".to_string()),
        },
        BinaryOp::Sub => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            (Value::Date(d), Value::Number(n)) => {
                let ndays = n as i64;
                d.checked_sub_signed(chrono::Duration::days(ndays))
                    .map(Value::Date)
                    .ok_or("Date subtraction overflow".to_string())
            }
            (Value::Date(d1), Value::Date(d2)) => {
                // difference in days as float
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
            (Number(_), Number(b)) if b == 0.0 => Err("Division by zero".to_string()),
            (Number(a), Number(b)) => Ok(Number(a / b)),
            _ => Err("Div: incompatible types".to_string()),
        },

        BinaryOp::Exp => match (left, right) {
            (Number(a), Number(b)) => Ok(Number(a.powf(b))),
            _ => Err("Exp: incompatible types".to_string()),
        },

        BinaryOp::Eq => Ok(Bool(left == right)),
        BinaryOp::Ne => Ok(Bool(left != right)),
        BinaryOp::Lt => match (left, right) {
            (Number(a), Number(b)) => Ok(Bool(a < b)),
            (Str(a, _), Str(b, _)) => Ok(Bool(a < b)),
            (Date(a), Date(b)) => Ok(Bool(a < b)),
            _ => Err("Lt: incompatible types".to_string()),
        },
        BinaryOp::Le => match (left, right) {
            (Number(a), Number(b)) => Ok(Bool(a <= b)),
            (Str(a, _), Str(b, _)) => Ok(Bool(a <= b)),
            (Date(a), Date(b)) => Ok(Bool(a <= b)),
            _ => Err("Le: incompatible types".to_string()),
        },
        BinaryOp::Gt => match (left, right) {
            (Number(a), Number(b)) => Ok(Bool(a > b)),
            (Str(a, _), Str(b, _)) => Ok(Bool(a > b)),
            (Date(a), Date(b)) => Ok(Bool(a > b)),
            _ => Err("Gt: incompatible types".to_string()),
        },
        BinaryOp::Ge => match (left, right) {
            (Number(a), Number(b)) => Ok(Bool(a >= b)),
            (Str(a, _), Str(b, _)) => Ok(Bool(a >= b)),
            (Date(a), Date(b)) => Ok(Bool(a >= b)),
            _ => Err("Ge: incompatible types".to_string()),
        },

        BinaryOp::Contain => match (left, right) {
            (Str(needle, _), Str(haystack, _)) => Ok(Bool(haystack.contains(&needle))),
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

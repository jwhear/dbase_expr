use chrono::Datelike;
use chrono::NaiveDate;

use crate::translate::{BinaryOp, Expression, UnaryOp};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Str(String),
    Bool(bool),
    Number(f64),
    Date(NaiveDate),
    Null,
}

pub type RowGetter<'a> = &'a dyn Fn(&str) -> Option<Value>;

impl Expression {
    pub fn evaluate(&self, get: RowGetter) -> Result<Value, String> {
        match self {
            Expression::BoolLiteral(b) => Ok(Value::Bool(*b)),

            Expression::NumberLiteral(s) => s
                .parse::<f64>()
                .map(Value::Number)
                .map_err(|e| e.to_string()),

            Expression::SingleQuoteStringLiteral(s) => Ok(Value::Str(s.clone())),

            Expression::Field { name, .. } => {
                get(name).ok_or_else(|| format!("Field '{}' not found in row", name))
            }

            Expression::UnaryOperator(op, expr) => {
                let v = expr.evaluate(get)?;
                match (op, v) {
                    (UnaryOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
                    (UnaryOp::Neg, Value::Number(n)) => Ok(Value::Number(-n)),
                    _ => Err("Invalid unary operation".to_string()),
                }
            }

            Expression::BinaryOperator(lhs, op, rhs) => {
                let left = lhs.evaluate(get)?;
                let right = rhs.evaluate(get)?;
                eval_binary_op(op, left, right)
            }

            Expression::Iif {
                cond,
                when_true,
                when_false,
            } => match cond.evaluate(get)? {
                Value::Bool(true) => when_true.evaluate(get),
                Value::Bool(false) => when_false.evaluate(get),
                _ => Err("IIF condition must be boolean".to_string()),
            },

            Expression::FunctionCall { name, args } => {
                let name_upper = name.to_uppercase();

                let eval_args: Result<Vec<Value>, String> =
                    args.iter().map(|arg| arg.evaluate(get)).collect();

                let args = eval_args?;

                match name_upper.as_str() {
                    "LTRIM" => match &args[..] {
                        [Value::Str(s)] => Ok(Value::Str(s.trim_end().to_string())),
                        _ => Err("LTRIM expects a single string argument".to_string()),
                    },

                    "RTRIM" => match &args[..] {
                        //TODO
                        [Value::Str(s)] => Ok(Value::Str(s.trim_start().to_string())),
                        _ => Err("RTRIM expects a single string argument".to_string()),
                    },

                    "ALLTRIM" => match &args[..] {
                        //TODO
                        [Value::Str(s)] => Ok(Value::Str(s.trim_start().trim_end().to_string())),
                        _ => Err("ALLTRIM expects a single string argument".to_string()),
                    },

                    "CHR" => match &args[..] {
                        [Value::Number(n)] => {
                            let ch = (*n as u8) as char;
                            Ok(Value::Str(ch.to_string()))
                        }
                        _ => Err("CHR expects a single numeric argument".to_string()),
                    },

                    "CTOD" | "STOD" => match &args[..] {
                        [Value::Str(s)] => {
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
                            let fmt = if args.len() == 2 {
                                "%Y%m%d"
                            } else {
                                "%m/%d/%y"
                            };
                            Ok(Value::Str(d.format(fmt).to_string()))
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
                        [Value::Str(s), Value::Number(n)] => {
                            let n = *n as usize;
                            Ok(Value::Str(s.chars().take(n).collect()))
                        }
                        _ => Err("LEFT expects (string, number)".to_string()),
                    },

                    "RIGHT" => match &args[..] {
                        [Value::Str(s), Value::Number(n)] => {
                            let n = *n as usize;
                            Ok(Value::Str(
                                s.chars()
                                    .rev()
                                    .take(n)
                                    .collect::<Vec<_>>()
                                    .into_iter()
                                    .rev()
                                    .collect(),
                            ))
                        }
                        _ => Err("RIGHT expects (string, number)".to_string()),
                    },

                    "SUBSTR" => match &args[..] {
                        [Value::Str(s), Value::Number(start), Value::Number(len)] => {
                            let start = (*start as usize).saturating_sub(1);
                            let len = *len as usize;
                            let substr: String = s.chars().skip(start).take(len).collect();
                            Ok(Value::Str(substr))
                        }
                        _ => Err("SUBSTR expects (string, start, length)".to_string()),
                    },

                    "UPPER" => match &args[..] {
                        [Value::Str(s)] => Ok(Value::Str(s.to_uppercase())),
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
                            Ok(Value::Str(fmt.trim().to_string()))
                        }
                        _ => Err("STR expects (number, len, dec)".to_string()),
                    },

                    "VAL" => match &args[..] {
                        [Value::Str(s)] => s
                            .parse::<f64>()
                            .map(Value::Number)
                            .map_err(|e| e.to_string()),
                        _ => Err("VAL expects a string".to_string()),
                    },

                    "DATE" => Ok(Value::Date(chrono::Local::now().naive_local().date())),

                    _ => Err(format!("Unsupported function: {}", name)),
                }
            }

            Expression::BareFunctionCall(name) => {
                if name == "CURRENT_DATE" {
                    Ok(Value::Date(chrono::Local::now().naive_local().date()))
                } else {
                    Err(format!("Unknown bare function: {}", name))
                }
            }

            Expression::Cast(expr, target) => {
                let val = expr.evaluate(get)?;
                eval_cast(val, target)
            }
        }
    }
}

fn eval_binary_op(op: &BinaryOp, left: Value, right: Value) -> Result<Value, String> {
    use Value::*;
    match op {
        BinaryOp::Add => match (left, right) {
            (Number(a), Number(b)) => Ok(Number(a + b)),
            (Str(a), Str(b)) => Ok(Str(a + &b)),
            _ => Err("Invalid Add operands".to_string()),
        },
        BinaryOp::Sub => match (left, right) {
            (Number(a), Number(b)) => Ok(Number(a - b)),
            _ => Err("Invalid Sub operands".to_string()),
        },
        BinaryOp::Eq => Ok(Value::Bool(left == right)),
        BinaryOp::Ne => Ok(Value::Bool(left != right)),
        BinaryOp::Concat => match (left, right) {
            (Str(a), Str(b)) => Ok(Str(a + &b)),
            _ => Err("Concat expects strings".to_string()),
        },
        BinaryOp::And => match (left, right) {
            (Bool(a), Bool(b)) => Ok(Bool(a && b)),
            _ => Err("And expects booleans".to_string()),
        },
        BinaryOp::Or => match (left, right) {
            (Bool(a), Bool(b)) => Ok(Bool(a || b)),
            _ => Err("Or expects booleans".to_string()),
        },
        BinaryOp::Lt => match (left, right) {
            (Number(a), Number(b)) => Ok(Bool(a < b)),
            (Str(a), Str(b)) => Ok(Bool(a < b)),
            _ => Err("Lt expects comparable types".to_string()),
        },
        BinaryOp::Like => match (left, right) {
            (Str(s), Str(pat)) => {
                let re_str = pat.replace('%', ".*").replace('_', ".");
                let re = regex::Regex::new(&format!("^{}$", re_str)).map_err(|e| e.to_string())?;
                Ok(Bool(re.is_match(&s)))
            }
            _ => Err("LIKE expects string operands".to_string()),
        },
        _ => Err("Binary operator not implemented".to_string()),
    }
}

fn eval_cast(val: Value, target: &str) -> Result<Value, String> {
    match (val, target) {
        (Value::Str(s), "Number") => s
            .parse::<f64>()
            .map(Value::Number)
            .map_err(|e| e.to_string()),
        (Value::Number(n), "Str") => Ok(Value::Str(n.to_string())),
        (Value::Bool(b), "Str") => Ok(Value::Str(b.to_string())),
        (Value::Date(d), "Str") => Ok(Value::Str(d.format("%Y%m%d").to_string())),
        _ => Err(format!("Cannot cast to {}", target)),
    }
}

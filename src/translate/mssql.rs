use crate::{
    ast,
    codebase_functions::CodebaseFunction as F,
    translate::{
        Error, ExprRef, Expression, FieldType, Result, TranslationContext, expr_ref, ok,
        postgres::{self, get_all_args, get_arg, translate as default_translate, wrong_type},
    },
};

pub struct MssqlTranslator<F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
{
    pub field_lookup: F,
}

impl<F> TranslationContext for MssqlTranslator<F>
where
    F: Fn(Option<&str>, &str) -> std::result::Result<(String, FieldType), String>,
{
    fn lookup_field(
        &self,
        alias: Option<&str>,
        field: &str,
    ) -> std::result::Result<(String, FieldType), String> {
        (self.field_lookup)(alias, field)
    }

    fn translate(&self, source: &ast::Expression) -> Result {
        default_translate(source, self)
    }

    fn translate_fn_call(
        &self,
        name: &crate::codebase_functions::CodebaseFunction,
        args: &[Box<ast::Expression>],
    ) -> std::result::Result<(ExprRef, FieldType), Error> {
        translate_fn_call(name, args, self)
    }

    fn translate_binary_op(
        &self,
        l: &ast::Expression,
        op: &ast::BinaryOp,
        r: &ast::Expression,
    ) -> Result {
        translate_binary_op(self, l, op, r)
    }
}

pub fn translate_binary_op<T: TranslationContext>(
    cx: &T,
    l: &ast::Expression,
    op: &ast::BinaryOp,
    r: &ast::Expression,
) -> Result {
    let (translated_l, ty_l) = cx.translate(l)?;
    let (translated_r, ty_r) = cx.translate(r)?;

    match (op, ty_l, ty_r) {
        // For date + integer, SQL Server requires DATEADD function
        (
            ast::BinaryOp::Add,
            FieldType::Date,
            FieldType::Integer | FieldType::Double | FieldType::Float | FieldType::Numeric { .. },
        ) => ok(
            Expression::FunctionCall {
                name: "DATEADD".to_string(),
                args: vec![
                    expr_ref(Expression::BareFunctionCall("day".to_string())),
                    translated_r,
                    translated_l,
                ],
            },
            FieldType::Date,
        ),
        // For date - integer, SQL Server requires DATEADD with negative number
        (
            ast::BinaryOp::Sub,
            FieldType::Date,
            FieldType::Integer | FieldType::Double | FieldType::Float | FieldType::Numeric { .. },
        ) => ok(
            Expression::FunctionCall {
                name: "DATEADD".to_string(),
                args: vec![
                    expr_ref(Expression::BareFunctionCall("day".to_string())),
                    expr_ref(Expression::UnaryOperator(
                        crate::translate::UnaryOp::Neg,
                        translated_r,
                    )),
                    translated_l,
                ],
            },
            FieldType::Date,
        ),
        // For date - date, SQL Server requires DATEDIFF function
        (ast::BinaryOp::Sub, FieldType::Date, FieldType::Date) => ok(
            Expression::FunctionCall {
                name: "DATEDIFF".to_string(),
                args: vec![
                    expr_ref(Expression::BareFunctionCall("day".to_string())),
                    translated_r,
                    translated_l,
                ],
            },
            FieldType::Numeric { len: 99, dec: 0 },
        ),
        // For all other operations, delegate to postgres implementation
        _ => postgres::translate_binary_op(cx, l, op, r),
    }
}

pub fn translate_fn_call(
    name: &F,
    args: &[Box<ast::Expression>],
    cx: &impl TranslationContext,
) -> std::result::Result<(ExprRef, FieldType), Error> {
    let arg = |index: usize| get_arg(index, args, cx, name);
    let all_args = || get_all_args(args, cx);
    let wrong_type = |index| wrong_type(index, name, args);

    // These are only the ones that are different from Postgres, everything else falls through to postgres
    match name {
        // In SQL Server, CHR is called CHAR
        F::CHR => ok(
            Expression::FunctionCall {
                name: "CHAR".to_string(),
                args: all_args()?,
            },
            FieldType::Character(1),
        ),

        // CTOD(x) => CONVERT(date, x, 101) -- format 101 is MM/DD/YY
        F::CTOD => ok(
            Expression::FunctionCall {
                name: "CONVERT".into(),
                args: vec![
                    expr_ref(Expression::BareFunctionCall("date".to_string())),
                    arg(0)??.0,
                    expr_ref(Expression::NumberLiteral("101".to_string())), // MM/DD/YY format
                ],
            },
            FieldType::Date,
        ),

        // DATE() => CAST(GETDATE() AS date)
        F::DATE => ok(
            Expression::Cast(
                expr_ref(Expression::BareFunctionCall("GETDATE()".to_string())),
                "date",
            ),
            FieldType::Date,
        ),

        // DAY(x) => DAY(x) -- SQL Server has this function
        F::DAY => ok(
            Expression::FunctionCall {
                name: "DAY".into(),
                args: vec![arg(0)??.0],
            },
            FieldType::Double,
        ),

        // DTOC(x) => FORMAT(x, 'MM/dd/yy') or FORMAT(x, 'yyyyMMdd')
        F::DTOC => {
            if args.len() == 2 {
                // Equivalent to DTOS
                ok(
                    Expression::FunctionCall {
                        name: "FORMAT".into(),
                        args: vec![arg(0)??.0, expr_ref("yyyyMMdd".into())],
                    },
                    FieldType::Character(8),
                )
            } else {
                ok(
                    Expression::FunctionCall {
                        name: "FORMAT".into(),
                        args: vec![arg(0)??.0, expr_ref("MM/dd/yy".into())],
                    },
                    FieldType::Character(8),
                )
            }
        }

        F::DTOS => ok(
            Expression::FunctionCall {
                name: "FORMAT".into(),
                args: vec![arg(0)??.0, expr_ref("yyyyMMdd".into())],
            },
            FieldType::Character(8),
        ),

        // MONTH(x) => MONTH(x) -- SQL Server has this function
        F::MONTH => ok(
            Expression::FunctionCall {
                name: "MONTH".into(),
                args: vec![arg(0)??.0],
            },
            FieldType::Double,
        ),

        // RIGHT(x, n) => RIGHT(x, n) -- SQL Server has this function
        F::RIGHT => {
            let (x, ty) = arg(0)??;
            let n = match &*arg(1)??.0.borrow() {
                Expression::NumberLiteral(v) => v.parse::<u32>().map_err(|_| wrong_type(1)),
                _ => Err(wrong_type(1)),
            }?;
            let out_ty = match ty {
                FieldType::Character(len) if len > n => FieldType::Character(len - n),
                FieldType::Character(_) => FieldType::Character(1), // Minimum length
                _ => FieldType::Memo,
            };
            ok(
                Expression::FunctionCall {
                    name: "RIGHT".into(),
                    args: vec![x, arg(1)??.0],
                },
                out_ty,
            )
        }

        // STOD(x) => CONVERT(date, x, 112) -- format 112 is YYYYMMDD
        F::STOD => ok(
            Expression::FunctionCall {
                name: "CONVERT".to_string(),
                args: vec![
                    expr_ref(Expression::BareFunctionCall("date".to_string())),
                    arg(0)??.0,
                    expr_ref(Expression::NumberLiteral("112".to_string())), // YYYYMMDD format
                ],
            },
            FieldType::Date,
        ),

        // STR(num, len, dec) => STR(num, len, dec) with padding
        F::STR => {
            let len: i64 = match &*arg(1)??.0.borrow() {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(1)),
                _ => Err(wrong_type(1)),
            }?;
            let dec: i64 = match &*arg(2)??.0.borrow() {
                Expression::NumberLiteral(v) => v.parse().map_err(|_| wrong_type(2)),
                _ => Err(wrong_type(2)),
            }?;

            // SQL Server STR function: STR(float_expression, length, decimal)
            ok(
                Expression::FunctionCall {
                    name: "STR".to_string(),
                    args: vec![
                        arg(0)??.0,
                        expr_ref(Expression::NumberLiteral(len.to_string())),
                        expr_ref(Expression::NumberLiteral(dec.to_string())),
                    ],
                },
                FieldType::Character(len as u32),
            )
        }

        // VAL(x) => CAST(x as float)
        F::VAL => ok(
            Expression::Cast(arg(0)??.0, "float"),
            FieldType::Numeric { len: 0, dec: 0 },
        ),

        // YEAR(x) => YEAR(x) -- SQL Server has this function
        F::YEAR => ok(
            Expression::FunctionCall {
                name: "YEAR".into(),
                args: vec![arg(0)??.0],
            },
            FieldType::Double,
        ),

        // For all other functions, delegate to Postgres implementation
        other => postgres::translate_fn_call(other, args, cx),
    }
}

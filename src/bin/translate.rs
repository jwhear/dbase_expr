use clap::{Parser, ValueEnum};
use dbase_expr::{
    parser::{Depth, ParseTree, parse_into_tree},
    to_sql::{self, Printer, PrinterConfig, PrinterContext},
    translate::{self, FieldType, TranslationContext},
};
use std::{borrow::Cow, collections::HashMap, path::PathBuf, time::Instant};

#[derive(Default, Debug, Clone, Copy, ValueEnum)]
enum Target {
    #[default]
    Postgres,
    Sqlite,
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Field lookup information. This must be a CSV in the following format:
    ///   tbl, field, type, len, dec, <other fields ignored>
    fields: PathBuf,

    /// Which SQL backend to target
    #[arg(long, value_enum, default_value = "postgres")]
    target: Target,
}

// Expects a field defs CSV as an argument, then reads expressions from stdin
pub fn main() {
    let cli = Cli::parse();

    // Load up the field type data
    let field_info =
        read_field_info_from_csv(csv::Reader::from_path(&cli.fields).expect("a fields file"));

    let mut expressions = csv::Reader::from_reader(std::io::stdin());
    let mut t = Translator {
        field_info,
        current_table: "".into(),
    };

    let cx = match cli.target {
        Target::Postgres => to_sql::PostgresPrinterContext {}.box_clone(),
        Target::Sqlite => to_sql::SqlitePrinterContext { pad_strings: false }.box_clone(),
    };

    fn tr<T: TranslationContext>(
        expr: &str,
        translator: &T,
        printer_cx: Box<dyn PrinterContext>,
    ) -> String {
        let mut tree = ParseTree::new();
        parse_into_tree(expr, &mut tree, Depth::default()).expect("valid parse");
        let (sqltree, root_type) = translator.translate(&tree).expect("translated");

        format!(
            "{root_type:?} {}",
            Printer::new(
                sqltree,
                PrinterConfig {
                    context: printer_cx
                }
            )
        )
    }

    let start = Instant::now();
    let mut num_exprs = 0;
    for record in expressions.records() {
        let record = record.expect("a valid CSV row");

        let table = record.get(0).expect("a table name");
        // 1: ignore tag name
        let tag_expression = record.get(2).expect("an expression");
        let tag_filter = record.get(3).expect("a filter");

        t.current_table = table.to_owned();

        if !tag_expression.is_empty() {
            println!("{}", tr(tag_expression, &t, cx.clone()));
            num_exprs += 1;
        }

        if !tag_filter.is_empty() {
            println!("{}", tr(tag_filter, &t, cx.clone()));
            num_exprs += 1;
        }
    }

    let elapsed = Instant::now() - start;
    println!(
        "Translated {num_exprs} expressions in {}ms",
        elapsed.as_millis()
    );
}

#[derive(Debug, Hash, PartialEq, Eq)]
struct FieldKey {
    table: String,
    field: String,
}

fn read_field_info_from_csv<R: std::io::Read>(
    mut csv: csv::Reader<R>,
) -> HashMap<FieldKey, FieldType> {
    let mut field_info = std::collections::HashMap::new();
    for result in csv.records() {
        let record = result.expect("a valid CSV row");

        // Raw
        let table = record.get(0).expect("table name");
        let field = record.get(1).expect("field name");
        let typ = record.get(2).expect("type");
        let len = record.get(3).expect("len");
        let dec = record.get(4).expect("dec");

        // Parsed
        let table = table.to_uppercase();
        let field = field.to_uppercase();
        let len = len.parse().expect("a valid len");
        let dec = dec.parse().expect("a valid dec");

        // Go from an integer (67)
        let typ: u32 = typ.parse().expect("a type integer");
        // to a character ('C')
        let typ = char::from_u32(typ).expect("a type character");
        // to a FieldType with length information
        let typ = match typ {
            'C' => FieldType::Character(len),
            'Z' => FieldType::CharacterBinary(len),
            'Y' => FieldType::Currency,
            'T' => FieldType::DateTime,
            'D' => FieldType::Date,
            'B' => FieldType::Double,
            'F' => FieldType::Float,
            'G' => FieldType::General,
            'I' => FieldType::Integer,
            'L' => FieldType::Logical,
            'M' => FieldType::Memo,
            'X' => FieldType::MemoBinary,
            'N' => FieldType::Numeric { len, dec },
            x => panic!("unknown field type {x}"),
        };

        field_info.insert(FieldKey { table, field }, typ);
    }
    field_info
}

struct Translator {
    field_info: HashMap<FieldKey, FieldType>,
    current_table: String,
}

impl TranslationContext for Translator {
    fn lookup_field<'field_lookup>(
        &'field_lookup self,
        alias: Option<&str>,
        field: &str,
    ) -> std::result::Result<(Cow<'field_lookup, str>, FieldType), String> {
        let table = alias.unwrap_or(&self.current_table).to_owned();
        let field = field.to_uppercase();
        let key = FieldKey {
            table: table.clone(),
            field: field.clone(),
        };

        if field.eq_ignore_ascii_case("__deleted") {
            return Ok((Cow::from("__deleted"), FieldType::Logical));
        }
        if field.eq_ignore_ascii_case("recno5") {
            return Ok((Cow::from("RECNO5"), FieldType::Integer));
        }

        self.field_info
            .get(&key)
            .map(|ft| (Cow::from(field.clone()), *ft))
            .ok_or_else(|| format!("Unknown field: {table}->{field}"))
    }

    fn translate_expr<'field_lookup, 'parse>(
        &'field_lookup self,
        source: &'parse dbase_expr::parser::Expression,
        in_tree: &'parse ParseTree,
        out_tree: &mut translate::SQLTree<'field_lookup, 'parse>,
    ) -> translate::ExpResult<'field_lookup, 'parse> {
        translate::sqlite::translate_expr(source, in_tree, out_tree, self)
    }

    fn translate_fn_call<'field_lookup, 'parse>(
        &'field_lookup self,
        name: &'parse dbase_expr::codebase_functions::CodebaseFunction,
        args: &'parse [dbase_expr::parser::ExpressionId],
        in_tree: &'parse ParseTree,
        out_tree: &mut translate::SQLTree<'field_lookup, 'parse>,
    ) -> translate::ExpResult<'field_lookup, 'parse> {
        translate::sqlite::translate_fn_call(name, args, in_tree, out_tree, self)
    }

    fn translate_binary_op<'field_lookup, 'parse>(
        &'field_lookup self,
        l: &'parse dbase_expr::parser::Expression,
        op: &'parse dbase_expr::parser::BinaryOp,
        r: &'parse dbase_expr::parser::Expression,
        in_tree: &'parse ParseTree,
        out_tree: &mut translate::SQLTree<'field_lookup, 'parse>,
    ) -> translate::ExpResult<'field_lookup, 'parse> {
        translate::sqlite::translate_binary_op(l, op, r, in_tree, out_tree, self)
    }
}

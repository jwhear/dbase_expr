use dbase_expr::parser::{TreePrinter, parse};

fn main() {
    for line in std::io::stdin().lines() {
        let line = line.expect("a line");
        let now = std::time::Instant::now();
        let res = parse(&line);
        print!("[in {}μs] ", now.elapsed().as_micros());
        match res {
            Err(e) => println!("Error parsing input: {e}"),
            Ok(tree) => match tree.get_root_id() {
                None => println!("Empty tree"),
                Some(id) => println!("{}", TreePrinter(tree, id)),
            },
        }
    }
}

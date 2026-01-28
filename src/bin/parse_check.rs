use dbase_expr::parser::{TreePrinter, parse};

fn main() {
    for line in std::io::stdin().lines() {
        let line = line.expect("a line");
        let now = std::time::Instant::now();
        let res = parse(&line);
        print!("[in {}μs] ", now.elapsed().as_micros());
        match res {
            Err(e) => println!("Error parsing input: {e}"),
            Ok((tree, root)) => println!("{}", TreePrinter(tree, root)),
        }
    }
}

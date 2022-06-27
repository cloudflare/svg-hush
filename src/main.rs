use std::fs::File;
use std::io::{BufReader, BufWriter};
use svg_hush::*;

fn main() {
    let input = std::env::args().nth(1);
    let input = input.as_deref().unwrap_or("tests/test.xml");
    let file = File::open(input).unwrap();

    let filter = Filter::new();
    match filter.filter(BufReader::new(file), BufWriter::new(std::io::stdout())) {
        Ok(()) => {},
        Err(e) => {
            eprintln!("Can't parse {input}: {e:?}");
        }
    }
}

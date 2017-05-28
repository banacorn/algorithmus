use std::fs::File;
use std::io::{Result, BufReader};
use std::io::prelude::*;

extern crate rosalind;
use rosalind::problems;

fn read_input(filepath: &str) -> Result<String> {
    let file = File::open(filepath)?;
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;
    Ok(String::from(contents.trim()))
}

fn main() {
    let input = read_input("inputs/revc").unwrap();
    println!("{}", problems::revc::run(&input));
}

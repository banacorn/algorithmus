extern crate long;

use std::io::prelude::*;
use std::fs::File;

use long::fasta::*;

fn main() {

    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();
    let input_str = buffer.trim();

    println!("{:?}", parse_fasta(input_str));
}

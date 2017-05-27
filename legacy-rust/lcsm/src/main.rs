use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;
use std::io::Result;

fn read_input() -> Result<String> {
    let file = File::open("input")?;
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;
    Ok(contents)
}

fn main() {
    let input = read_input().unwrap();



    println!["{}", input];
}

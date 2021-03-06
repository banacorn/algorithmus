use std::fs::File;
use std::io::{Result, BufReader};
use std::io::prelude::*;

pub fn read_input(filepath: &str) -> Result<String> {
    let file = File::open(filepath)?;
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents)?;
    Ok(String::from(contents.trim()))
}

pub fn write_answer(filepath: &str, answer: &str) {
    let mut file = File::create(filepath).unwrap();
    file.write_all(answer.as_bytes()).unwrap();
}

use std::io::prelude::*;
use std::fs::File;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();
    let string: &str = buffer.trim();

    let transcripted: String = string.chars().map(transcript).collect();

    println!("{}", transcripted);
}

fn transcript(c: char) -> char {
    match c {
        'T' => 'U',
        others => others
    }
}

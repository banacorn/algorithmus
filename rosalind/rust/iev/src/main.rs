use std::io::prelude::*;
use std::fs::File;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let input: Vec<f32> = buffer.trim().split_whitespace().map(|x| x.parse().unwrap()).collect();

    let offspring = input[0] * 2.0 + input[1] * 2.0 + input[2] * 2.0 + input[3] * 1.5 + input[4] * 1.0;
    println!("{}", offspring);
}

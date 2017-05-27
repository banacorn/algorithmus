use std::io::prelude::*;
use std::fs::File;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let sequences: Vec<&str> = buffer.trim().lines().collect();
    println!("{:?}", distance(sequences[0], sequences[1]));

}

fn distance(a: &str, b: &str) -> usize {
    let mut count = 0;
    for (x, y) in a.chars().zip(b.chars()) {
        if x != y {
            count = count + 1;
        }
    }
    count
}

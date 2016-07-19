use std::io::prelude::*;
use std::fs::File;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();

    file.read_to_string(&mut buffer).unwrap();
    let string: &str = buffer.trim();

    let mut a = 0;
    let mut c = 0;
    let mut t = 0;
    let mut g = 0;

    for x in string.chars() {
        match x {
            'A' => a = a + 1,
            'C' => c = c + 1,
            'G' => g = g + 1,
            'T' => t = t + 1,
            _   => ()
        }
    }

    println!("{} {} {} {}", a, c, g, t);
}

extern crate prtm;

use std::io::prelude::*;
use std::fs::File;

use prtm::bio::*;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let aa_vec: Vec<AA> = parse_aa_sequence(buffer.trim());
    let mass: f64 = aa_vec
        .into_iter()
        .map(monoisotopic_mass)
        .fold(0.0, |acc, n| acc + n);
    println!("{}", mass);
}

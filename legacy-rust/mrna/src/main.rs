extern crate mrna;

use std::io::prelude::*;
use std::fs::File;

use mrna::bio::*;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let aa_vec: Vec<AA> = parse_aa_sequence(buffer.trim());
    let count: usize = aa_vec
        .into_iter()
        .map(codon_num)
        .fold(1, |acc, n| acc * n % 1000000);
    println!("{}", count);
}

fn codon_num(aa: AA) -> usize {
    match aa {
        AA::Phe => 2,
        AA::Leu => 6,
        AA::Ile => 3,
        AA::Met => 1,
        AA::Val => 4,
        AA::Ser => 6,
        AA::Pro => 4,
        AA::Thr => 4,
        AA::Ala => 4,
        AA::Tyr => 2,
        AA::Stop => 3,
        AA::His => 2,
        AA::Gln => 2,
        AA::Asn => 2,
        AA::Lys => 2,
        AA::Asp => 2,
        AA::Glu => 2,
        AA::Cys => 2,
        AA::Trp => 1,
        AA::Arg => 6,
        AA::Gly => 4
    }
}

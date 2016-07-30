extern crate splc;

use std::io::prelude::*;
use std::fs::File;
use splc::bio::fasta::*;
use splc::bio::dna::DNA;
// use orf::bio::aa::*;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let fastas: Vec<Fasta<DNA>> = parse_fastas(buffer.trim());
    let fasta = &fastas[0];
    let introns = &fastas[1..];
    println!("{:?}", fasta);

    for aa in fasta.aa() {
        println!("{:?}", aa);
    }

}

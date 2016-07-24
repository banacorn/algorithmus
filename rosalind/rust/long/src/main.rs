extern crate long;

use std::io::prelude::*;
use std::fs::File;

use long::fasta::*;
use long::contig::*;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();
    let input_str = buffer.trim();

    let reads: Vec<Contig> = parse_fasta(input_str)
        .into_iter()
        .map(|x| Contig::new(x.content))
        .collect();

    for x in &reads {
        println!("{:?}", x);
    }


    // pop the last out, and overlap it with the rest of the vector
    // while reads.len() != 1 {
    //     reads.pop();
    //     println!("{:?}", reads);
    // }
    // println!("{:?}", reads);


}

extern crate cons;

use std::io::prelude::*;
use std::fs::File;
use cons::bio::*;
use std::cmp::max;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let fastas = parse_fastas(buffer.trim());

    let length = fastas[0].content.len();

    // initialize table
    let mut a_vec: Vec<usize> = Vec::new();
    let mut c_vec: Vec<usize> = Vec::new();
    let mut g_vec: Vec<usize> = Vec::new();
    let mut t_vec: Vec<usize> = Vec::new();
    // fill them with zeros
    a_vec.resize(length, 0);
    c_vec.resize(length, 0);
    g_vec.resize(length, 0);
    t_vec.resize(length, 0);

    for f in fastas.into_iter() {
        for (j, c) in f.content.chars().enumerate() {
            match c {
                'A' => a_vec[j] = a_vec[j] + 1,
                'C' => c_vec[j] = c_vec[j] + 1,
                'G' => g_vec[j] = g_vec[j] + 1,
                'T' => t_vec[j] = t_vec[j] + 1,
                _   => {}
            }
        }
    }

    // find the concensus string
    let mut concensus = String::new();

    for i in 0 .. length {
        let maximum = max(max(a_vec[i], c_vec[i]), max(g_vec[i], t_vec[i]));
        if maximum == a_vec[i] {
            concensus = concensus + "A";
        } else if maximum == c_vec[i] {
            concensus = concensus + "C";
        } else if maximum == g_vec[i] {
            concensus = concensus + "G";
        } else if maximum == t_vec[i] {
            concensus = concensus + "T";
        }
    }
    println!("{}", concensus);
    print!("A: ");
    for i in a_vec {
        print!("{} ", i);
    }
    print!("\nC: ");
    for i in c_vec {
        print!("{} ", i);
    }
    print!("\nG: ");
    for i in g_vec {
        print!("{} ", i);
    }
    print!("\nT: ");
    for i in t_vec {
        print!("{} ", i);
    }
    print!("\n");
}

extern crate num_traits;
extern crate num;

use std::io::prelude::*;
use std::fs::File;
use std::collections::VecDeque;
use num::bigint::{BigInt, ToBigInt};
use num_traits::{Zero, One};

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let mut iter = buffer.trim().split_whitespace();
    let n: i64 = iter.next().unwrap().parse().unwrap();
    let m: i64 = iter.next().unwrap().parse().unwrap();

    // populate
    let mut population: VecDeque<BigInt> = VecDeque::new();
    for _ in 0 .. m-1 {
        population.push_front(Zero::zero());
    }
    population.push_front(One::one());

    // breed
    for _ in 0..n-1 {
        bread(&mut population);
    }

    // sum
    println!("{}", population.iter().fold(0.to_bigint().unwrap(), |acc, x| acc + x));
}

fn bread(population: &mut VecDeque<BigInt>) {
    let mut newborn = Zero::zero();
    for (i, n) in &mut population.iter().enumerate() {
        if i != 0 {
            newborn = newborn + n;
        }
    }

    population.push_front(newborn);
    population.pop_back();
    // println!("{:?}", population);
}

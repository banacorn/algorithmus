extern crate num_traits;
extern crate num;

use std::io::prelude::*;
use std::fs::File;
use num::bigint::{BigInt};
use num_traits::{Zero, One};
use num::FromPrimitive;
use num_traits::checked_pow;
use num::ToPrimitive;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let mut iter = buffer.trim().split_whitespace();
    let k: usize = iter.next().unwrap().parse().unwrap();
    let n: usize = iter.next().unwrap().parse().unwrap();

    let mut sum: BigInt = Zero::zero();
    let total = generation_size(k);
    for i in n..2usize.pow(k as u32) + 1 {
        sum = sum.checked_add(&exact(k, i)).unwrap();
    }
    let sumx10000: BigInt = sum.checked_mul(&FromPrimitive::from_usize(10000).unwrap()).unwrap();
    let resultx10000: BigInt = sumx10000.checked_div(&total).unwrap();
    println!("{}", resultx10000.to_f32().unwrap() / 10000.0);
}

fn factorial(n: usize) -> BigInt {
    match n {
        0 => One::one(),
        n => {
            let n_: BigInt = FromPrimitive::from_usize(n).unwrap();
            n_.checked_mul(&factorial(n - 1)).unwrap()
        }
    }
}

fn choice(m: usize, n: usize) -> BigInt {
    // let n_fac: BigInt = factorialFromPrimitive::from_usize(n).unwrap();
    factorial(m).checked_div(&factorial(m - n).checked_mul(&factorial(n)).unwrap()).unwrap()
}

fn exact(g: usize, n: usize) -> BigInt {
    let size = 2usize.pow(g as u32);

    let base = FromPrimitive::from_usize(3).unwrap();
    let population: BigInt = checked_pow(base, size - n).unwrap();
    let combination: BigInt = choice(size, n);
    let result: BigInt = population.checked_mul(&combination).unwrap();
    result
}

fn generation_size(g: usize) -> BigInt {
    let four: BigInt = FromPrimitive::from_usize(4).unwrap();
    checked_pow(four, 2usize.pow(g as u32)).unwrap()
}

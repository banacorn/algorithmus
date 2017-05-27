use std::io::prelude::*;
use std::fs::File;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let mut iter = buffer.trim().split_whitespace();
    let k: usize = iter.next().unwrap().parse().unwrap();
    let m: usize = iter.next().unwrap().parse().unwrap();
    let n: usize = iter.next().unwrap().parse().unwrap();

    // k - k
    let population = k + m + n;
    let combinations = population * (population - 1);
    let kk = k * (k - 1);
    let mm = m * (m - 1);
    let km = k * m * 2;
    let kn = k * n * 2;
    let mn = m * n * 2;

    println!("{}", (kk * 4 + km * 4 + mm * 3 + mn * 2 + kn * 4) as f32 / (combinations * 4) as f32);
}

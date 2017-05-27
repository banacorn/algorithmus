use std::io::prelude::*;
use std::fs::File;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let string = buffer.trim();

    let mut iter = string.split_whitespace();
    let k: u64 = iter.next().unwrap().parse().unwrap();
    let n: u64 = iter.next().unwrap().parse().unwrap();
    println!("{}", fib(k, n));
}

fn fib(k: u64, n: u64) -> u64 {
    let mut vec: Vec<u64> = Vec::new();
    vec.push(1);
    vec.push(1);

    for i in 2..k {
        let grandparent = vec[(i - 2) as usize];
        let parent      = vec[(i - 1) as usize];
        vec.push(grandparent * n + parent);
    }
    let last: &u64 = vec.last().unwrap();
    return *last;
}

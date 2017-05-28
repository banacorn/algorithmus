extern crate rosalind;
// use rosalind::bio::*;
use rosalind::problems;
use rosalind::util;

fn main() {
    let input = util::read_input("inputs/iprb").unwrap();
    println!("{}", problems::iprb::run(&input));
}

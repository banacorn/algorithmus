extern crate rosalind;
use rosalind::problems;
use rosalind::util;

fn main() {
    let input = util::read_input("inputs/revc").unwrap();
    println!("{}", problems::revc::run(&input));
}

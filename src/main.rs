extern crate rosalind;
// use rosalind::bio::*;
use rosalind::problems;
use rosalind::util;

fn main() {
    let input = util::read_input("inputs/prot").unwrap();
    let answer = problems::prot::run(&input);
    println!("{}", answer);
    // util::write_answer("answers/prot", &answer);
}

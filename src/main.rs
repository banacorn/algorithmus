extern crate rosalind;
// use rosalind::bio::*;
use rosalind::problems;
use rosalind::util;

fn main() {
    let input = util::read_input("inputs/fib").unwrap();
    let answer = problems::fib::run(&input);
    println!("{}", answer);
    util::write_answer("answers/fib", &answer);
}

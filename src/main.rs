extern crate rosalind;
// use rosalind::bio::*;
use rosalind::problems;
use rosalind::util;

fn main() {
    let input = util::read_input("inputs/gc").unwrap();
    let answer = problems::gc::run(&input);
    println!("{}", answer);
    // util::write_answer("answers/gc", &answer);
}

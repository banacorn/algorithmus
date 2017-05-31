extern crate rosalind;
// use rosalind::bio::*;
use rosalind::problems;
use rosalind::util;

fn main() {
    let input = util::read_input("inputs/subs").unwrap();
    let answer = problems::subs::run(&input);
    println!("{}", answer);
    util::write_answer("answers/subs", &answer);
}

extern crate rosalind;

use rosalind::util;
use rosalind::problems;

#[test]
fn dna() {
    let input = util::read_input("inputs/dna").unwrap();
    let answer = util::read_input("answers/dna").unwrap();
    assert_eq!(problems::dna::run(&input), answer);
}

#[test]
fn rna() {
    let input = util::read_input("inputs/rna").unwrap();
    let answer = util::read_input("answers/rna").unwrap();
    assert_eq!(problems::rna::run(&input), answer);
}

#[test]
fn revc() {
    let input = util::read_input("inputs/revc").unwrap();
    let answer = util::read_input("answers/revc").unwrap();
    assert_eq!(problems::revc::run(&input), answer);
}
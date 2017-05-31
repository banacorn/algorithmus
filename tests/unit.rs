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

#[test]
fn iprb() {
    let input = util::read_input("inputs/iprb").unwrap();
    let answer = util::read_input("answers/iprb").unwrap();
    assert_eq!(problems::iprb::run(&input), answer);
}

#[test]
fn fib() {
    let input = util::read_input("inputs/fib").unwrap();
    let answer = util::read_input("answers/fib").unwrap();
    assert_eq!(problems::fib::run(&input), answer);
}

#[test]
fn gc() {
    let input = util::read_input("inputs/gc").unwrap();
    let answer = util::read_input("answers/gc").unwrap();
    assert_eq!(problems::gc::run(&input), answer);
}

#[test]
fn prot() {
    let input = util::read_input("inputs/prot").unwrap();
    let answer = util::read_input("answers/prot").unwrap();
    assert_eq!(problems::prot::run(&input), answer);
}

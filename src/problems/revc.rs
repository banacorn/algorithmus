use super::super::bio::*;

pub fn run(input: &str) -> String {
    parse_dna(input)
        .into_iter()
        .map(complement)
        .rev()
        .collect::<DNA>()
        .to_string()
}

fn complement(nt: Deoxyribonucleotide)-> Deoxyribonucleotide {
    match nt {
        Deoxyribonucleotide::A => Deoxyribonucleotide::T,
        Deoxyribonucleotide::C => Deoxyribonucleotide::G,
        Deoxyribonucleotide::G => Deoxyribonucleotide::C,
        Deoxyribonucleotide::T => Deoxyribonucleotide::A,
    }
}

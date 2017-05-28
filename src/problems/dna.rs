use super::super::bio::{Deoxyribonucleotide, parse_dna};

pub fn run(input: &str) -> String {
    let mut a = 0;
    let mut c = 0;
    let mut g = 0;
    let mut t = 0;

    for x in &parse_dna(input).0 {
        match x {
            &Deoxyribonucleotide::A => a = a + 1,
            &Deoxyribonucleotide::C => c = c + 1,
            &Deoxyribonucleotide::G => g = g + 1,
            &Deoxyribonucleotide::T => t = t + 1,
        }
    }

    format!("{} {} {} {}", a, c, g, t)
}

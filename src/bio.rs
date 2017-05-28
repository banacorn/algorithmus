pub enum Riboucleotide {
    A, C, G, U
}

pub enum Deoxyribonucleotide {
    A, C, G, T
}

#[allow(dead_code)]
type RNA = Vec<Riboucleotide>;

#[allow(dead_code)]
type DNA = Vec<Deoxyribonucleotide>;

pub fn parse_rna(input: &str) -> RNA {
    input.chars().map(|c: char| -> Riboucleotide {
        match c {
            'A' => Riboucleotide::A,
            'C' => Riboucleotide::C,
            'G' => Riboucleotide::G,
            'U' => Riboucleotide::U,
            _   => Riboucleotide::U,
        }
    }).collect()
}

pub fn parse_dna(input: &str) -> DNA {
    input.chars().map(|c: char| -> Deoxyribonucleotide {
        match c {
            'A' => Deoxyribonucleotide::A,
            'C' => Deoxyribonucleotide::C,
            'G' => Deoxyribonucleotide::G,
            'T' => Deoxyribonucleotide::T,
            _   => Deoxyribonucleotide::T,
        }
    }).collect()
}

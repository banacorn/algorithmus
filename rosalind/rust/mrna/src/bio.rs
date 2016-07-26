use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(dead_code)]
pub enum RNA {
    U, C, A, G
}

pub fn parse_rna_sequence(s: &str) -> Vec<RNA> {
    let mut vec = Vec::new();
    for c in s.chars() {
        match c {
            'U' => vec.push(RNA::U),
            'C' => vec.push(RNA::C),
            'A' => vec.push(RNA::A),
            'G' => vec.push(RNA::G),
            _   => {}
        }
    }
    vec
}

#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(dead_code)]
pub enum AA {
    Phe, Leu, Ile, Met, Val,
    Ser, Pro, Thr, Ala, Tyr,
    Stop, His, Gln, Asn, Lys,
    Asp, Glu, Cys, Trp, Arg, Gly
}

pub fn parse_aa_sequence(s: &str) -> Vec<AA> {
    let mut vec = Vec::new();
    for c in s.chars() {
        match c {
            'F' => vec.push(AA::Phe),
            'L' => vec.push(AA::Leu),
            'I' => vec.push(AA::Ile),
            'M' => vec.push(AA::Met),
            'V' => vec.push(AA::Val),
            'S' => vec.push(AA::Ser),
            'P' => vec.push(AA::Pro),
            'T' => vec.push(AA::Thr),
            'A' => vec.push(AA::Ala),
            'Y' => vec.push(AA::Tyr),
            'H' => vec.push(AA::His),
            'Q' => vec.push(AA::Gln),
            'N' => vec.push(AA::Asn),
            'K' => vec.push(AA::Lys),
            'D' => vec.push(AA::Asp),
            'E' => vec.push(AA::Glu),
            'C' => vec.push(AA::Cys),
            'W' => vec.push(AA::Trp),
            'R' => vec.push(AA::Arg),
            'G' => vec.push(AA::Gly),
            _ => vec.push(AA::Stop)
        }
    }
    vec.push(AA::Stop);
    vec
}

impl fmt::Display for AA {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &AA::Phe => write!(f, "F"),
            &AA::Leu => write!(f, "L"),
            &AA::Ile => write!(f, "I"),
            &AA::Met => write!(f, "M"),
            &AA::Val => write!(f, "V"),
            &AA::Ser => write!(f, "S"),
            &AA::Pro => write!(f, "P"),
            &AA::Thr => write!(f, "T"),
            &AA::Ala => write!(f, "A"),
            &AA::Tyr => write!(f, "Y"),
            &AA::Stop => write!(f, ""),
            &AA::His => write!(f, "H"),
            &AA::Gln => write!(f, "Q"),
            &AA::Asn => write!(f, "N"),
            &AA::Lys => write!(f, "K"),
            &AA::Asp => write!(f, "D"),
            &AA::Glu => write!(f, "E"),
            &AA::Cys => write!(f, "C"),
            &AA::Trp => write!(f, "W"),
            &AA::Arg => write!(f, "R"),
            &AA::Gly => write!(f, "G")
        }
    }
}

pub fn transcribe(chunk: &[RNA]) -> AA {
    match (&chunk[0], &chunk[1], &chunk[2]) {
        (&RNA::U, &RNA::U, &RNA::U) => AA::Phe,
        (&RNA::U, &RNA::U, &RNA::C) => AA::Phe,
        (&RNA::U, &RNA::U, &RNA::A) => AA::Leu,
        (&RNA::U, &RNA::U, &RNA::G) => AA::Leu,
        (&RNA::U, &RNA::C, &RNA::U) => AA::Ser,
        (&RNA::U, &RNA::C, &RNA::C) => AA::Ser,
        (&RNA::U, &RNA::C, &RNA::A) => AA::Ser,
        (&RNA::U, &RNA::C, &RNA::G) => AA::Ser,
        (&RNA::U, &RNA::A, &RNA::U) => AA::Tyr,
        (&RNA::U, &RNA::A, &RNA::C) => AA::Tyr,
        (&RNA::U, &RNA::A, &RNA::A) => AA::Stop,
        (&RNA::U, &RNA::A, &RNA::G) => AA::Stop,
        (&RNA::U, &RNA::G, &RNA::U) => AA::Cys,
        (&RNA::U, &RNA::G, &RNA::C) => AA::Cys,
        (&RNA::U, &RNA::G, &RNA::A) => AA::Stop,
        (&RNA::U, &RNA::G, &RNA::G) => AA::Trp,
        (&RNA::C, &RNA::U, &RNA::U) => AA::Leu,
        (&RNA::C, &RNA::U, &RNA::C) => AA::Leu,
        (&RNA::C, &RNA::U, &RNA::A) => AA::Leu,
        (&RNA::C, &RNA::U, &RNA::G) => AA::Leu,
        (&RNA::C, &RNA::C, &RNA::U) => AA::Pro,
        (&RNA::C, &RNA::C, &RNA::C) => AA::Pro,
        (&RNA::C, &RNA::C, &RNA::A) => AA::Pro,
        (&RNA::C, &RNA::C, &RNA::G) => AA::Pro,
        (&RNA::C, &RNA::A, &RNA::U) => AA::His,
        (&RNA::C, &RNA::A, &RNA::C) => AA::His,
        (&RNA::C, &RNA::A, &RNA::A) => AA::Gln,
        (&RNA::C, &RNA::A, &RNA::G) => AA::Gln,
        (&RNA::C, &RNA::G, &RNA::U) => AA::Arg,
        (&RNA::C, &RNA::G, &RNA::C) => AA::Arg,
        (&RNA::C, &RNA::G, &RNA::A) => AA::Arg,
        (&RNA::C, &RNA::G, &RNA::G) => AA::Arg,
        (&RNA::A, &RNA::U, &RNA::U) => AA::Ile,
        (&RNA::A, &RNA::U, &RNA::C) => AA::Ile,
        (&RNA::A, &RNA::U, &RNA::A) => AA::Ile,
        (&RNA::A, &RNA::U, &RNA::G) => AA::Met,
        (&RNA::A, &RNA::C, &RNA::U) => AA::Thr,
        (&RNA::A, &RNA::C, &RNA::C) => AA::Thr,
        (&RNA::A, &RNA::C, &RNA::A) => AA::Thr,
        (&RNA::A, &RNA::C, &RNA::G) => AA::Thr,
        (&RNA::A, &RNA::A, &RNA::U) => AA::Asn,
        (&RNA::A, &RNA::A, &RNA::C) => AA::Asn,
        (&RNA::A, &RNA::A, &RNA::A) => AA::Lys,
        (&RNA::A, &RNA::A, &RNA::G) => AA::Lys,
        (&RNA::A, &RNA::G, &RNA::U) => AA::Ser,
        (&RNA::A, &RNA::G, &RNA::C) => AA::Ser,
        (&RNA::A, &RNA::G, &RNA::A) => AA::Arg,
        (&RNA::A, &RNA::G, &RNA::G) => AA::Arg,
        (&RNA::G, &RNA::U, &RNA::U) => AA::Val,
        (&RNA::G, &RNA::U, &RNA::C) => AA::Val,
        (&RNA::G, &RNA::U, &RNA::A) => AA::Val,
        (&RNA::G, &RNA::U, &RNA::G) => AA::Val,
        (&RNA::G, &RNA::C, &RNA::U) => AA::Ala,
        (&RNA::G, &RNA::C, &RNA::C) => AA::Ala,
        (&RNA::G, &RNA::C, &RNA::A) => AA::Ala,
        (&RNA::G, &RNA::C, &RNA::G) => AA::Ala,
        (&RNA::G, &RNA::A, &RNA::U) => AA::Asp,
        (&RNA::G, &RNA::A, &RNA::C) => AA::Asp,
        (&RNA::G, &RNA::A, &RNA::A) => AA::Glu,
        (&RNA::G, &RNA::A, &RNA::G) => AA::Glu,
        (&RNA::G, &RNA::G, &RNA::U) => AA::Gly,
        (&RNA::G, &RNA::G, &RNA::C) => AA::Gly,
        (&RNA::G, &RNA::G, &RNA::A) => AA::Gly,
        (&RNA::G, &RNA::G, &RNA::G) => AA::Gly
    }
}

use std::fmt;
use std::iter::FromIterator;
use std::slice::Chunks;

#[derive(Debug)]
pub enum Riboucleotide {
    A, C, G, U
}

impl fmt::Display for Riboucleotide {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Riboucleotide::A => write!(f, "A"),
            &Riboucleotide::C => write!(f, "C"),
            &Riboucleotide::G => write!(f, "G"),
            &Riboucleotide::U => write!(f, "U"),
        }

    }
}

#[derive(Debug)]
pub enum Deoxyribonucleotide {
    A, C, G, T
}

impl fmt::Display for Deoxyribonucleotide {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}


//
// Sequences
//

#[derive(Debug)]
pub struct Seq<T>(pub Vec<T>);

impl<T> Seq<T> {
    pub fn chunks(&self, size: usize) -> Chunks<T> {
        self.0.chunks(size)
    }
}

impl<T: fmt::Display> fmt::Display for Seq<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for monomer in &self.0 {
            let _ = write!(f, "{}", monomer);
        }
        Ok({})
    }
}
impl<T> IntoIterator for Seq<T> {
    type Item = T;
    type IntoIter = ::std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T> FromIterator<T> for Seq<T> {
    fn from_iter<I: IntoIterator<Item=T>>(iter: I) -> Self {
        let mut vec = Vec::new();
        for i in iter {
            vec.push(i);
        }
        Seq(vec)
    }
}

pub type RNA = Seq<Riboucleotide>;
pub type DNA = Seq<Deoxyribonucleotide>;

pub fn parse_rna(input: &str) -> RNA {
    Seq(input.chars().map(|c: char| -> Riboucleotide {
        match c {
            'A' => Riboucleotide::A,
            'C' => Riboucleotide::C,
            'G' => Riboucleotide::G,
            'U' => Riboucleotide::U,
            _   => Riboucleotide::U,
        }
    }).collect())
}

pub fn parse_dna(input: &str) -> DNA {
    Seq(input.chars().map(|c: char| -> Deoxyribonucleotide {
        match c {
            'A' => Deoxyribonucleotide::A,
            'C' => Deoxyribonucleotide::C,
            'G' => Deoxyribonucleotide::G,
            'T' => Deoxyribonucleotide::T,
            _   => Deoxyribonucleotide::T,
        }
    }).collect())
}

// RNA polymerase
pub fn rna_polymerase(nt: Deoxyribonucleotide) -> Riboucleotide {
    match nt {
        Deoxyribonucleotide::A => Riboucleotide::A,
        Deoxyribonucleotide::C => Riboucleotide::C,
        Deoxyribonucleotide::G => Riboucleotide::G,
        Deoxyribonucleotide::T => Riboucleotide::U,
    }
}

pub fn transcribe(dna: DNA) -> RNA {
    dna
        .into_iter()
        .map(rna_polymerase)
        .collect()
}

#[derive(Debug)]
pub enum AminoAcid {
    Phe, Leu, Ile, Met, Val,
    Ser, Pro, Thr, Ala, Tyr,
    Stop, His, Gln, Asn, Lys,
    Asp, Glu, Cys, Trp, Arg, Gly
}

impl fmt::Display for AminoAcid {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &AminoAcid::Phe => write!(f, "F"),
            &AminoAcid::Leu => write!(f, "L"),
            &AminoAcid::Ile => write!(f, "I"),
            &AminoAcid::Met => write!(f, "M"),
            &AminoAcid::Val => write!(f, "V"),
            &AminoAcid::Ser => write!(f, "S"),
            &AminoAcid::Pro => write!(f, "P"),
            &AminoAcid::Thr => write!(f, "T"),
            &AminoAcid::Ala => write!(f, "A"),
            &AminoAcid::Tyr => write!(f, "Y"),
            &AminoAcid::Stop => write!(f, ""),
            &AminoAcid::His => write!(f, "H"),
            &AminoAcid::Gln => write!(f, "Q"),
            &AminoAcid::Asn => write!(f, "N"),
            &AminoAcid::Lys => write!(f, "K"),
            &AminoAcid::Asp => write!(f, "D"),
            &AminoAcid::Glu => write!(f, "E"),
            &AminoAcid::Cys => write!(f, "C"),
            &AminoAcid::Trp => write!(f, "W"),
            &AminoAcid::Arg => write!(f, "R"),
            &AminoAcid::Gly => write!(f, "G")
        }
    }
}

pub fn translate(chunk: &[Riboucleotide]) -> AminoAcid {
    match (&chunk[0], &chunk[1], &chunk[2]) {
        (&Riboucleotide::U, &Riboucleotide::U, &Riboucleotide::U) => AminoAcid::Phe,
        (&Riboucleotide::U, &Riboucleotide::U, &Riboucleotide::C) => AminoAcid::Phe,
        (&Riboucleotide::U, &Riboucleotide::U, &Riboucleotide::A) => AminoAcid::Leu,
        (&Riboucleotide::U, &Riboucleotide::U, &Riboucleotide::G) => AminoAcid::Leu,
        (&Riboucleotide::U, &Riboucleotide::C, &Riboucleotide::U) => AminoAcid::Ser,
        (&Riboucleotide::U, &Riboucleotide::C, &Riboucleotide::C) => AminoAcid::Ser,
        (&Riboucleotide::U, &Riboucleotide::C, &Riboucleotide::A) => AminoAcid::Ser,
        (&Riboucleotide::U, &Riboucleotide::C, &Riboucleotide::G) => AminoAcid::Ser,
        (&Riboucleotide::U, &Riboucleotide::A, &Riboucleotide::U) => AminoAcid::Tyr,
        (&Riboucleotide::U, &Riboucleotide::A, &Riboucleotide::C) => AminoAcid::Tyr,
        (&Riboucleotide::U, &Riboucleotide::A, &Riboucleotide::A) => AminoAcid::Stop,
        (&Riboucleotide::U, &Riboucleotide::A, &Riboucleotide::G) => AminoAcid::Stop,
        (&Riboucleotide::U, &Riboucleotide::G, &Riboucleotide::U) => AminoAcid::Cys,
        (&Riboucleotide::U, &Riboucleotide::G, &Riboucleotide::C) => AminoAcid::Cys,
        (&Riboucleotide::U, &Riboucleotide::G, &Riboucleotide::A) => AminoAcid::Stop,
        (&Riboucleotide::U, &Riboucleotide::G, &Riboucleotide::G) => AminoAcid::Trp,
        (&Riboucleotide::C, &Riboucleotide::U, &Riboucleotide::U) => AminoAcid::Leu,
        (&Riboucleotide::C, &Riboucleotide::U, &Riboucleotide::C) => AminoAcid::Leu,
        (&Riboucleotide::C, &Riboucleotide::U, &Riboucleotide::A) => AminoAcid::Leu,
        (&Riboucleotide::C, &Riboucleotide::U, &Riboucleotide::G) => AminoAcid::Leu,
        (&Riboucleotide::C, &Riboucleotide::C, &Riboucleotide::U) => AminoAcid::Pro,
        (&Riboucleotide::C, &Riboucleotide::C, &Riboucleotide::C) => AminoAcid::Pro,
        (&Riboucleotide::C, &Riboucleotide::C, &Riboucleotide::A) => AminoAcid::Pro,
        (&Riboucleotide::C, &Riboucleotide::C, &Riboucleotide::G) => AminoAcid::Pro,
        (&Riboucleotide::C, &Riboucleotide::A, &Riboucleotide::U) => AminoAcid::His,
        (&Riboucleotide::C, &Riboucleotide::A, &Riboucleotide::C) => AminoAcid::His,
        (&Riboucleotide::C, &Riboucleotide::A, &Riboucleotide::A) => AminoAcid::Gln,
        (&Riboucleotide::C, &Riboucleotide::A, &Riboucleotide::G) => AminoAcid::Gln,
        (&Riboucleotide::C, &Riboucleotide::G, &Riboucleotide::U) => AminoAcid::Arg,
        (&Riboucleotide::C, &Riboucleotide::G, &Riboucleotide::C) => AminoAcid::Arg,
        (&Riboucleotide::C, &Riboucleotide::G, &Riboucleotide::A) => AminoAcid::Arg,
        (&Riboucleotide::C, &Riboucleotide::G, &Riboucleotide::G) => AminoAcid::Arg,
        (&Riboucleotide::A, &Riboucleotide::U, &Riboucleotide::U) => AminoAcid::Ile,
        (&Riboucleotide::A, &Riboucleotide::U, &Riboucleotide::C) => AminoAcid::Ile,
        (&Riboucleotide::A, &Riboucleotide::U, &Riboucleotide::A) => AminoAcid::Ile,
        (&Riboucleotide::A, &Riboucleotide::U, &Riboucleotide::G) => AminoAcid::Met,
        (&Riboucleotide::A, &Riboucleotide::C, &Riboucleotide::U) => AminoAcid::Thr,
        (&Riboucleotide::A, &Riboucleotide::C, &Riboucleotide::C) => AminoAcid::Thr,
        (&Riboucleotide::A, &Riboucleotide::C, &Riboucleotide::A) => AminoAcid::Thr,
        (&Riboucleotide::A, &Riboucleotide::C, &Riboucleotide::G) => AminoAcid::Thr,
        (&Riboucleotide::A, &Riboucleotide::A, &Riboucleotide::U) => AminoAcid::Asn,
        (&Riboucleotide::A, &Riboucleotide::A, &Riboucleotide::C) => AminoAcid::Asn,
        (&Riboucleotide::A, &Riboucleotide::A, &Riboucleotide::A) => AminoAcid::Lys,
        (&Riboucleotide::A, &Riboucleotide::A, &Riboucleotide::G) => AminoAcid::Lys,
        (&Riboucleotide::A, &Riboucleotide::G, &Riboucleotide::U) => AminoAcid::Ser,
        (&Riboucleotide::A, &Riboucleotide::G, &Riboucleotide::C) => AminoAcid::Ser,
        (&Riboucleotide::A, &Riboucleotide::G, &Riboucleotide::A) => AminoAcid::Arg,
        (&Riboucleotide::A, &Riboucleotide::G, &Riboucleotide::G) => AminoAcid::Arg,
        (&Riboucleotide::G, &Riboucleotide::U, &Riboucleotide::U) => AminoAcid::Val,
        (&Riboucleotide::G, &Riboucleotide::U, &Riboucleotide::C) => AminoAcid::Val,
        (&Riboucleotide::G, &Riboucleotide::U, &Riboucleotide::A) => AminoAcid::Val,
        (&Riboucleotide::G, &Riboucleotide::U, &Riboucleotide::G) => AminoAcid::Val,
        (&Riboucleotide::G, &Riboucleotide::C, &Riboucleotide::U) => AminoAcid::Ala,
        (&Riboucleotide::G, &Riboucleotide::C, &Riboucleotide::C) => AminoAcid::Ala,
        (&Riboucleotide::G, &Riboucleotide::C, &Riboucleotide::A) => AminoAcid::Ala,
        (&Riboucleotide::G, &Riboucleotide::C, &Riboucleotide::G) => AminoAcid::Ala,
        (&Riboucleotide::G, &Riboucleotide::A, &Riboucleotide::U) => AminoAcid::Asp,
        (&Riboucleotide::G, &Riboucleotide::A, &Riboucleotide::C) => AminoAcid::Asp,
        (&Riboucleotide::G, &Riboucleotide::A, &Riboucleotide::A) => AminoAcid::Glu,
        (&Riboucleotide::G, &Riboucleotide::A, &Riboucleotide::G) => AminoAcid::Glu,
        (&Riboucleotide::G, &Riboucleotide::G, &Riboucleotide::U) => AminoAcid::Gly,
        (&Riboucleotide::G, &Riboucleotide::G, &Riboucleotide::C) => AminoAcid::Gly,
        (&Riboucleotide::G, &Riboucleotide::G, &Riboucleotide::A) => AminoAcid::Gly,
        (&Riboucleotide::G, &Riboucleotide::G, &Riboucleotide::G) => AminoAcid::Gly
    }
}

//
// FASTA
//

#[derive(Debug)]
pub struct FASTA<T> {
    pub id: String,
    pub sequence: Seq<T>
}

impl<T: fmt::Display> fmt::Display for FASTA<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let &FASTA { ref id, ref sequence } = self;
        write!(f, ">{}\n", id)?;
        write!(f, "{}\n", sequence)
    }
}

pub fn parse_dna_fasta(input: &str) -> Vec<FASTA<Deoxyribonucleotide>> {
    let mut fastas = Vec::new();

    let mut id: String = String::new();
    let mut sequence = Vec::new();

    for (i, line) in input.lines().enumerate() {
        let id_start = &line[0 .. 1] == ">";
        if id_start {
            if i != 0 {
                // push the last result and clear
                fastas.push(FASTA {
                    id: id,
                    sequence: Seq(sequence)
                });
            }
            id = (&line[1..]).to_string();
            sequence = Vec::new();
        } else {
            sequence.extend(parse_dna(line).0);
        }

    }
    // push the last result
    fastas.push(FASTA {
        id: id,
        sequence: Seq(sequence)
    });

    fastas
}

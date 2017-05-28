use std::fmt;
use std::iter::FromIterator;

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
        match self {
            &Deoxyribonucleotide::A => write!(f, "A"),
            &Deoxyribonucleotide::C => write!(f, "C"),
            &Deoxyribonucleotide::G => write!(f, "G"),
            &Deoxyribonucleotide::T => write!(f, "T"),
        }

    }
}


pub struct RNA(pub Vec<Riboucleotide>);

impl fmt::Display for RNA {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for nt in &self.0 {
            let _ = write!(f, "{}", nt);
        }
        Ok({})
    }
}

impl IntoIterator for RNA {
    type Item = Riboucleotide;
    type IntoIter = ::std::vec::IntoIter<Riboucleotide>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FromIterator<Riboucleotide> for RNA {
    fn from_iter<I: IntoIterator<Item=Riboucleotide>>(iter: I) -> Self {
        let mut vec = Vec::new();
        for i in iter {
            vec.push(i);
        }
        RNA(vec)
    }
}


pub struct DNA(pub Vec<Deoxyribonucleotide>);

impl fmt::Display for DNA {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for nt in &self.0 {
            let _ = write!(f, "{}", nt);
        }
        Ok({})
    }
}

impl IntoIterator for DNA {
    type Item = Deoxyribonucleotide;
    type IntoIter = ::std::vec::IntoIter<Deoxyribonucleotide>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FromIterator<Deoxyribonucleotide> for DNA {
    fn from_iter<I: IntoIterator<Item=Deoxyribonucleotide>>(iter: I) -> Self {
        let mut vec = Vec::new();
        for i in iter {
            vec.push(i);
        }
        DNA(vec)
    }
}


pub fn parse_rna(input: &str) -> RNA {
    RNA(input.chars().map(|c: char| -> Riboucleotide {
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
    DNA(input.chars().map(|c: char| -> Deoxyribonucleotide {
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

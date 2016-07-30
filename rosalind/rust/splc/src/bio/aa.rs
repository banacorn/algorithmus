use bio::rna::RNA;
use bio::fasta::FastaUnit;
use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum AA {
    Phe, Leu, Ile, Met, Val,
    Ser, Pro, Thr, Ala, Tyr,
    Stop, His, Gln, Asn, Lys,
    Asp, Glu, Cys, Trp, Arg, Gly
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

impl FastaUnit for AA {
    fn parse(c: char) -> Result<AA, String> {
        match c {
            'F' => Ok(AA::Phe),
            'L' => Ok(AA::Leu),
            'I' => Ok(AA::Ile),
            'M' => Ok(AA::Met),
            'V' => Ok(AA::Val),
            'S' => Ok(AA::Ser),
            'P' => Ok(AA::Pro),
            'T' => Ok(AA::Thr),
            'A' => Ok(AA::Ala),
            'Y' => Ok(AA::Tyr),
            'H' => Ok(AA::His),
            'Q' => Ok(AA::Gln),
            'N' => Ok(AA::Asn),
            'K' => Ok(AA::Lys),
            'D' => Ok(AA::Asp),
            'E' => Ok(AA::Glu),
            'C' => Ok(AA::Cys),
            'W' => Ok(AA::Trp),
            'R' => Ok(AA::Arg),
            'G' => Ok(AA::Gly),
            _   => Err(format!("Failed to parse {} as a DNA", c))
        }
    }
}

// from Nucleotides to AAs
pub trait ToAA {
    fn toAA([Self; 3]) -> Result<AA, String> where Self: Sized;
}

pub fn monoisotopic_mass(aa: AA) -> f64 {
    match aa {
        AA::Ala => 71.03711,
        AA::Cys => 103.00919,
        AA::Asp => 115.02694,
        AA::Glu => 129.04259,
        AA::Phe => 147.06841,
        AA::Gly => 57.02146,
        AA::His => 137.05891,
        AA::Ile => 113.08406,
        AA::Lys => 128.09496,
        AA::Leu => 113.08406,
        AA::Met => 131.04049,
        AA::Asn => 114.04293,
        AA::Pro => 97.05276,
        AA::Gln => 128.05858,
        AA::Arg => 156.10111,
        AA::Ser => 87.03203,
        AA::Thr => 101.04768,
        AA::Val => 99.06841,
        AA::Trp => 186.07931,
        AA::Tyr => 163.06333,
        AA::Stop => 0.0
    }
}

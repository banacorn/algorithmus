use bio::fasta::FastaUnit;
use bio::rna::{RNA, IntoRNA, transcribe};
use bio::aa::{AA, IntoAA};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum DNA {
    T, C, A, G
}


impl FastaUnit for DNA {
    fn parse(c: char) -> Result<DNA, String> {
        match c {
            'T' => Ok(DNA::T),
            'C' => Ok(DNA::C),
            'A' => Ok(DNA::A),
            'G' => Ok(DNA::G),
            _   => Err(format!("Failed to parse {} as a DNA", c)),
        }
    }
}

impl IntoAA for DNA {
    fn chunk_size() -> usize { 3 }
    fn into_aa(chunk: &[DNA]) -> Result<AA, String> {
        if chunk.len() >= 3 {
            let rna = [transcribe(chunk[0]), transcribe(chunk[1]), transcribe(chunk[2])];
            IntoAA::into_aa(&rna)
        } else {
            Err("input chunk not large enough".to_owned())
        }
    }
}

impl IntoRNA for DNA {
    fn into_rna(dna: DNA) -> RNA {
        match dna {
            DNA::T => RNA::U,
            DNA::C => RNA::C,
            DNA::A => RNA::A,
            DNA::G => RNA::G
        }
    }
}


pub fn complement(c: DNA) -> DNA {
    match c {
        DNA::T => DNA::A,
        DNA::C => DNA::G,
        DNA::A => DNA::T,
        DNA::G => DNA::C
    }
}

pub fn reverse_complement(s: &[DNA]) -> Vec<DNA> {
    s.iter().map(|x| complement(*x)).rev().collect()
}
pub struct DNASequence<'a> {
    sequence: &'a [DNA],
    cursor: usize
}

impl<'a> DNASequence<'a> {
    pub fn new(s: &'a [DNA]) -> DNASequence<'a> {
        DNASequence {
            sequence: s,
            cursor: 0
        }
    }
}

impl<'a> Iterator for DNASequence<'a> {
    type Item = DNA;
    fn next(&mut self) -> Option<DNA> {
        if self.cursor < self.sequence.len() {
            let result = Some(self.sequence[self.cursor]);
            self.cursor += 1;
            return result;
        } else {
            None
        }
    }
}

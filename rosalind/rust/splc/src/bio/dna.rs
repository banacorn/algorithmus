use bio::fasta::FastaUnit;
use bio::rna::transcribe;
use bio::aa::{AA, ToAA};

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

impl ToAA for DNA {
    fn toAA(chunk: [DNA; 3]) -> Result<AA, String> {
        let rna = [transcribe(chunk[0]), transcribe(chunk[1]), transcribe(chunk[2])];
        ToAA::toAA(rna)
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

use bio::aa::{AA, translate};
use bio::rna::{RNA, transcribe};
use bio::dna;
use bio::dna::{DNA, DNASequence};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Fasta {
    pub id: String,
    pub sequence: Vec<DNA>
}

impl Fasta {
    pub fn new(id: String, seq: Vec<DNA>) -> Fasta {
        Fasta {
            id: id,
            sequence: seq
        }
    }

    pub fn dna(&self) -> DNASequence {
        DNASequence::new(&self.sequence)
    }

    pub fn reading_frames(&self) -> MultipleReadingFrame {
        MultipleReadingFrame::new(&self.sequence)
    }

}

pub fn parse_fasta(s: &str) -> Result<Fasta, String> {
    let mut id = "";
    let mut sequence = Vec::new();

    for line in s.lines() {
        if &line[0 .. 1] == ">" {
            id = &line[1..];
        } else {
            for c in line.chars() {
                let dna = try!(dna::parse_dna(c));
                sequence.push(dna);
            }
        }
    }
    Ok(Fasta {
        id: id[1..].to_owned(),
        sequence: sequence
    })
}

pub fn parse_fastas(s: &str) -> Vec<Fasta> {
    let mut vec = Vec::new();

    let mut id = "";
    let mut sequence = Vec::new();

    for (i, line) in s.lines().enumerate() {
        if &line[0 .. 1] == ">" {
            if i != 0 {
                // should push and reinitialize
                vec.push(Fasta {
                    id: id.to_owned(),
                    sequence: sequence.clone()
                });
            }
            id = &line[1..];
            sequence = Vec::new();
        } else {
            for c in line.chars() {
                match dna::parse_dna(c) {
                    Ok(dna) => sequence.push(dna),
                    Err(_) => {}
                }
            }
        }
    }
    // push the last result
    vec.push(Fasta {
        id: id.to_owned(),
        sequence: sequence.clone()
    });

    return vec
}


#[derive(Debug, Clone)]
pub struct ReadingFrame {
    sequence: Vec<DNA>,
    cursor: usize
}

// impl<'a> ReadingFrame<'a> {
//     pub fn new(s: &'a [DNA]) -> ReadingFrame<'a> {
//         MultipleReadingFrame {
//             sequence: s,
//             flipped: false,
//             offset: 0
//         }
//     }
// }

impl Iterator for ReadingFrame {
    type Item = AA;
    fn next(&mut self) -> Option<AA> {
        if self.cursor + 3 < self.sequence.len() {
            let dna_slice = &self.sequence[self.cursor .. self.cursor + 3];
            let rna_slice: Vec<RNA> = dna_slice.iter().map(|x| transcribe(*x)).collect();
            self.cursor += 3;
            return Some(translate(&rna_slice));
        } else {
            None
        }
    }
}


#[derive(Debug)]
pub struct MultipleReadingFrame<'a> {
    sequence: &'a [DNA],
    flipped: bool,
    offset: usize
}

impl<'a> MultipleReadingFrame<'a> {
    pub fn new(s: &'a [DNA]) -> MultipleReadingFrame<'a> {
        MultipleReadingFrame {
            sequence: s,
            flipped: false,
            offset: 0
        }
    }
}

impl<'a> Iterator for MultipleReadingFrame<'a> {
    type Item = ReadingFrame;
    fn next(&mut self) -> Option<ReadingFrame> {
        if !self.flipped && self.offset == 3 {
            self.flipped = true;
            self.offset = 0;
        }

        let sequence = if self.flipped {
            dna::reverse_complement(&self.sequence[0 .. self.sequence.len() - self.offset])
        } else {
            self.sequence[self.offset .. ].to_vec()
        };

        if self.offset < 3 {
            // None
            let result = ReadingFrame {
                sequence: sequence,
                cursor: self.offset,
            };
            self.offset += 1;
            return Some(result)
        } else {
            None
        }
    }
}

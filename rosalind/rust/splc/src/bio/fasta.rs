use bio::aa::{AA, ToAA};
use bio::rna::{RNA};
use bio::dna;
use bio::dna::{DNA, DNASequence};
use std::fmt::Debug;

pub trait FastaUnit : Debug + Clone {
    fn parse(char) -> Result<Self, String> where Self: Sized;
    fn parse_sequence(s: &str) -> Vec<Self> where Self: Sized {
        let mut vec = Vec::new();
        for c in s.chars() {
            match FastaUnit::parse(c) {
                Ok(rna) => vec.push(rna),
                Err(_) => {}
            }
        }
        vec
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Fasta<T: FastaUnit> {
    pub id: String,
    pub sequence: Vec<T>
}

impl<T: FastaUnit> Fasta<T> {
    pub fn new(id: String, seq: Vec<T>) -> Fasta<T> {
        Fasta {
            id: id,
            sequence: seq
        }
    }
    pub fn aa(&self) -> ReadingFrame<T> {
        ReadingFrame::new(&self.sequence)
    }
    //
    // pub fn reading_frames(&self) -> MultipleReadingFrame {
    //     MultipleReadingFrame::new(&self.sequence)
    // }
}

pub fn parse_fasta<T: FastaUnit>(s: &str) -> Result<Fasta<T>, String> {
    let mut id = "";
    let mut sequence = Vec::new();

    for line in s.lines() {
        if &line[0 .. 1] == ">" {
            id = &line[1..];
        } else {
            sequence.append(&mut FastaUnit::parse_sequence(line));
        }
    }
    Ok(Fasta {
        id: id[1..].to_owned(),
        sequence: sequence
    })
}

pub fn parse_fastas<T: FastaUnit>(s: &str) -> Vec<Fasta<T>> {
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
            sequence.append(&mut FastaUnit::parse_sequence(line));
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
pub struct ReadingFrame<'a, T: 'a + FastaUnit> {
    sequence: &'a [T],
    cursor: usize
}

impl<'a, T: FastaUnit> ReadingFrame<'a, T> {
    pub fn new(s: &'a [T]) -> ReadingFrame<'a, T> {
        ReadingFrame {
            sequence: s,
            cursor: 0
        }
    }
}

impl<'a, T: FastaUnit + ToAA + Copy> Iterator for ReadingFrame<'a, T> {
    type Item = AA;
    fn next(&mut self) -> Option<AA> {
        if self.cursor + 3 < self.sequence.len() {
            let aa = ToAA::toAA([
                self.sequence[self.cursor],
                self.sequence[self.cursor + 1],
                self.sequence[self.cursor + 2]
            ]);
            self.cursor += 3;
            return aa.ok();
        } else {
            None
        }
    }
}


// #[derive(Debug, Clone)]
// pub struct OpenReadingFrame<'a> {
//     sequence: &'a [AA],
//     cursor: usize
// }
//
// impl<'a, T: FastaUnit> ReadingFrame<'a, T> {
//     pub fn new(s: &'a [T]) -> ReadingFrame<'a, T> {
//         ReadingFrame {
//             sequence: s,
//             cursor: 0
//         }
//     }
// }
//
// impl<'a, T: FastaUnit + ToAA + Copy> Iterator for ReadingFrame<'a, T> {
//     type Item = AA;
//     fn next(&mut self) -> Option<AA> {
//         if self.cursor + 3 < self.sequence.len() {
//             let aa = ToAA::toAA([
//                 self.sequence[self.cursor],
//                 self.sequence[self.cursor + 1],
//                 self.sequence[self.cursor + 2]
//             ]);
//             self.cursor += 3;
//             return aa.ok();
//         } else {
//             None
//         }
//     }
// }
//
//
// #[derive(Debug)]
// pub struct MultipleReadingFrame<'a> {
//     sequence: &'a [DNA],
//     flipped: bool,
//     offset: usize
// }
//
// impl<'a> MultipleReadingFrame<'a> {
//     pub fn new(s: &'a [DNA]) -> MultipleReadingFrame<'a> {
//         MultipleReadingFrame {
//             sequence: s,
//             flipped: false,
//             offset: 0
//         }
//     }
// }
//
// impl<'a> Iterator for MultipleReadingFrame<'a> {
//     type Item = ReadingFrame;
//     fn next(&mut self) -> Option<ReadingFrame> {
//         if !self.flipped && self.offset == 3 {
//             self.flipped = true;
//             self.offset = 0;
//         }
//
//         let sequence = if self.flipped {
//             dna::reverse_complement(&self.sequence[0 .. self.sequence.len() - self.offset])
//         } else {
//             self.sequence[self.offset .. ].to_vec()
//         };
//
//         if self.offset < 3 {
//             // None
//             let result = ReadingFrame {
//                 sequence: sequence,
//                 cursor: self.offset,
//             };
//             self.offset += 1;
//             return Some(result)
//         } else {
//             None
//         }
//     }
// }

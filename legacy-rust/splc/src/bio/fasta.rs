// use std::iter::FromIterator;
use bio::aa::{AA, IntoAA};
use bio::rna::{RNA, IntoRNA};
use bio::dna;
use bio::dna::{DNA, DNASequence};
use std::fmt::Debug;
use std::collections::VecDeque;

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

    pub fn aa(&self) -> AASequence<T> {
        AASequence::new(&self.sequence)
    }

    pub fn rna(&self) -> RNASequence<T> {
        RNASequence::new(&self.sequence)
    }
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

#[derive(Debug)]
pub struct AASequence<'a, T: 'a + FastaUnit> {
    sequence: &'a [T],
    cursor: usize
}

impl<'a, T: FastaUnit> AASequence<'a, T> {
    pub fn new(s: &'a [T]) -> AASequence<'a, T> {
        AASequence {
            sequence: s,
            cursor: 0
        }
    }
}

impl<'a, T: FastaUnit + IntoAA> Iterator for AASequence<'a, T> {
    type Item = AA;
    fn next(&mut self) -> Option<AA> {
        let chunk_size = <T as IntoAA>::chunk_size();
        if self.cursor + chunk_size <= self.sequence.len() {
            let aa = IntoAA::into_aa(&self.sequence[self.cursor ..]);
            self.cursor += chunk_size;
            return aa.ok();
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct RNASequence<'a, T: 'a + FastaUnit> {
    sequence: &'a [T],
    cursor: usize
}

impl<'a, T: FastaUnit> RNASequence<'a, T> {
    pub fn new(s: &'a [T]) -> RNASequence<'a, T> {
        RNASequence {
            sequence: s,
            cursor: 0
        }
    }
}

impl<'a, T: FastaUnit + IntoRNA + Copy> Iterator for RNASequence<'a, T> {
    type Item = RNA;
    fn next(&mut self) -> Option<RNA> {
        if self.cursor + 1 <= self.sequence.len() {
            let rna = IntoRNA::into_rna(self.sequence[self.cursor]);
            self.cursor += 1;
            return Some(rna)
        } else {
            None
        }
    }
}



// #[derive(Debug, Clone)]
// pub struct OpenReadingFrame {
//     sequence: Vec<AA>,
//     starts_at: VecDeque<usize>,
//     stops_at: Option<usize>
// }
//
// impl OpenReadingFrame {
//     pub fn new(s: Vec<AA>) -> OpenReadingFrame {
//         OpenReadingFrame {
//             sequence: s,
//             starts_at: VecDeque::new(),
//             stops_at: None
//         }
//     }
// }
//
// // impl Iterator for OpenReadingFrame {
// //     type Item = (usize, usize);
// //     fn next(&mut self) -> Option<(usize, usize)> {
// //         // println!("{:?}", self.sequence);
// //
// //         // let () = &self.sequence.into_iter().enumerate().next();
// //         for (i, &aa) in (&self.sequence).iter().enumerate() {
// //             match aa {
// //                 AA::Met => {
// //                     self.starts_at.push_back(i);
// //                 },
// //                 AA::Stop => {
// //                     self.stops_at = Some(i);
// //                     match self.starts_at.pop_front() {
// //                         Some(start) => {
// //                             return Some((start, i))
// //                         }
// //                         None => {
// //                             return None
// //                         }
// //                     }
// //                 },
// //                 _ => {}
// //             }
// //             println!("{:?}", aa);
// //         }
// //         None
// //     }
// // }

use bio::dna::DNA;
use bio::aa::{AA, ToAA};
use bio::fasta::FastaUnit;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RNA {
    U, C, A, G
}

impl FastaUnit for RNA {
    fn parse(c: char) -> Result<RNA, String> {
        match c {
            'U' => Ok(RNA::U),
            'C' => Ok(RNA::C),
            'A' => Ok(RNA::A),
            'G' => Ok(RNA::G),
            _   => Err(format!("Failed to parse {} as a RNA", c)),
        }
    }
}

// pub fn parse_rna_sequence
pub fn transcribe(dna: DNA) -> RNA {
    match dna {
        DNA::T => RNA::U,
        DNA::C => RNA::C,
        DNA::A => RNA::A,
        DNA::G => RNA::G
    }
}

impl ToAA for RNA {
    fn toAA(chunk: [RNA; 3]) -> Result<AA, String> {
        match chunk.len() {
            3 => {
                match (chunk[0], chunk[1], chunk[2]) {
                    (RNA::U, RNA::U, RNA::U) => Ok(AA::Phe),
                    (RNA::U, RNA::U, RNA::C) => Ok(AA::Phe),
                    (RNA::U, RNA::U, RNA::A) => Ok(AA::Leu),
                    (RNA::U, RNA::U, RNA::G) => Ok(AA::Leu),
                    (RNA::U, RNA::C, RNA::U) => Ok(AA::Ser),
                    (RNA::U, RNA::C, RNA::C) => Ok(AA::Ser),
                    (RNA::U, RNA::C, RNA::A) => Ok(AA::Ser),
                    (RNA::U, RNA::C, RNA::G) => Ok(AA::Ser),
                    (RNA::U, RNA::A, RNA::U) => Ok(AA::Tyr),
                    (RNA::U, RNA::A, RNA::C) => Ok(AA::Tyr),
                    (RNA::U, RNA::A, RNA::A) => Ok(AA::Stop),
                    (RNA::U, RNA::A, RNA::G) => Ok(AA::Stop),
                    (RNA::U, RNA::G, RNA::U) => Ok(AA::Cys),
                    (RNA::U, RNA::G, RNA::C) => Ok(AA::Cys),
                    (RNA::U, RNA::G, RNA::A) => Ok(AA::Stop),
                    (RNA::U, RNA::G, RNA::G) => Ok(AA::Trp),
                    (RNA::C, RNA::U, RNA::U) => Ok(AA::Leu),
                    (RNA::C, RNA::U, RNA::C) => Ok(AA::Leu),
                    (RNA::C, RNA::U, RNA::A) => Ok(AA::Leu),
                    (RNA::C, RNA::U, RNA::G) => Ok(AA::Leu),
                    (RNA::C, RNA::C, RNA::U) => Ok(AA::Pro),
                    (RNA::C, RNA::C, RNA::C) => Ok(AA::Pro),
                    (RNA::C, RNA::C, RNA::A) => Ok(AA::Pro),
                    (RNA::C, RNA::C, RNA::G) => Ok(AA::Pro),
                    (RNA::C, RNA::A, RNA::U) => Ok(AA::His),
                    (RNA::C, RNA::A, RNA::C) => Ok(AA::His),
                    (RNA::C, RNA::A, RNA::A) => Ok(AA::Gln),
                    (RNA::C, RNA::A, RNA::G) => Ok(AA::Gln),
                    (RNA::C, RNA::G, RNA::U) => Ok(AA::Arg),
                    (RNA::C, RNA::G, RNA::C) => Ok(AA::Arg),
                    (RNA::C, RNA::G, RNA::A) => Ok(AA::Arg),
                    (RNA::C, RNA::G, RNA::G) => Ok(AA::Arg),
                    (RNA::A, RNA::U, RNA::U) => Ok(AA::Ile),
                    (RNA::A, RNA::U, RNA::C) => Ok(AA::Ile),
                    (RNA::A, RNA::U, RNA::A) => Ok(AA::Ile),
                    (RNA::A, RNA::U, RNA::G) => Ok(AA::Met),
                    (RNA::A, RNA::C, RNA::U) => Ok(AA::Thr),
                    (RNA::A, RNA::C, RNA::C) => Ok(AA::Thr),
                    (RNA::A, RNA::C, RNA::A) => Ok(AA::Thr),
                    (RNA::A, RNA::C, RNA::G) => Ok(AA::Thr),
                    (RNA::A, RNA::A, RNA::U) => Ok(AA::Asn),
                    (RNA::A, RNA::A, RNA::C) => Ok(AA::Asn),
                    (RNA::A, RNA::A, RNA::A) => Ok(AA::Lys),
                    (RNA::A, RNA::A, RNA::G) => Ok(AA::Lys),
                    (RNA::A, RNA::G, RNA::U) => Ok(AA::Ser),
                    (RNA::A, RNA::G, RNA::C) => Ok(AA::Ser),
                    (RNA::A, RNA::G, RNA::A) => Ok(AA::Arg),
                    (RNA::A, RNA::G, RNA::G) => Ok(AA::Arg),
                    (RNA::G, RNA::U, RNA::U) => Ok(AA::Val),
                    (RNA::G, RNA::U, RNA::C) => Ok(AA::Val),
                    (RNA::G, RNA::U, RNA::A) => Ok(AA::Val),
                    (RNA::G, RNA::U, RNA::G) => Ok(AA::Val),
                    (RNA::G, RNA::C, RNA::U) => Ok(AA::Ala),
                    (RNA::G, RNA::C, RNA::C) => Ok(AA::Ala),
                    (RNA::G, RNA::C, RNA::A) => Ok(AA::Ala),
                    (RNA::G, RNA::C, RNA::G) => Ok(AA::Ala),
                    (RNA::G, RNA::A, RNA::U) => Ok(AA::Asp),
                    (RNA::G, RNA::A, RNA::C) => Ok(AA::Asp),
                    (RNA::G, RNA::A, RNA::A) => Ok(AA::Glu),
                    (RNA::G, RNA::A, RNA::G) => Ok(AA::Glu),
                    (RNA::G, RNA::G, RNA::U) => Ok(AA::Gly),
                    (RNA::G, RNA::G, RNA::C) => Ok(AA::Gly),
                    (RNA::G, RNA::G, RNA::A) => Ok(AA::Gly),
                    (RNA::G, RNA::G, RNA::G) => Ok(AA::Gly)
                }
            },
            _ => Err(format!("The length of RNA sequence isn't 3, but {}", chunk.len()))
        }
    }
}
//
// pub struct RNASequence<'a> {
//     sequence: &'a [RNA],
//     cursor: usize
// }
//
// impl<'a> RNASequence<'a> {
//     pub fn new(s: &'a [RNA]) -> RNASequence<'a> {
//         RNASequence {
//             sequence: s,
//             cursor: 0
//         }
//     }
// }
//
// impl<'a> Iterator for RNASequence<'a> {
//     type Item = RNA;
//     fn next(&mut self) -> Option<RNA> {
//         if self.cursor < self.sequence.len() {
//             let result = Some(self.sequence[self.cursor]);
//             self.cursor += 1;
//             return result;
//         } else {
//             None
//         }
//     }
// }

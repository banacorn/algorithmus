use bio::dna::DNA;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RNA {
    U, C, A, G
}
pub fn parse_rna(c: char) -> Result<RNA, String> {
    match c {
        'U' => Ok(RNA::U),
        'C' => Ok(RNA::C),
        'A' => Ok(RNA::A),
        'G' => Ok(RNA::G),
        _   => Err(format!("Failed to parse {} as a RNA", c)),
    }
}

pub fn parse_rna_sequence(s: &str) -> Vec<RNA> {
    let mut vec = Vec::new();
    for c in s.chars() {
        match parse_rna(c) {
            Ok(rna) => vec.push(rna),
            Err(_) => {}
        }
    }
    vec
}

pub fn transcribe(dna: DNA) -> RNA {
    match dna {
        DNA::T => RNA::U,
        DNA::C => RNA::C,
        DNA::A => RNA::A,
        DNA::G => RNA::G
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

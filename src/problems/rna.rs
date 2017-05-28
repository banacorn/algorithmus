use super::super::bio::*;

pub fn run(input: &str) -> String {
    transcribe(parse_dna(input))
        .to_string()
}

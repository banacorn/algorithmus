use super::super::bio::*;

pub fn run(input: &str) -> String {
    parse_rna(input)
        .chunks(3)
        .map(translate)
        .collect::<Seq<AminoAcid>>()
        .to_string()
}

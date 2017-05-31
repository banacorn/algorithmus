use super::super::bio::*;

pub fn run(input: &str) -> String {
    let fastas = parse_dna_fasta(input);

    let vec = fastas
        .into_iter()
        .map(|fasta: FASTA<Deoxyribonucleotide>| {
            let FASTA { id, sequence } = fasta;
            (id, calculate_gc_content(sequence))
        })
        .collect::<Vec<(String, f32)>>();
    let result = max(vec).unwrap();
    format!("{}\n{}", result.0, result.1)
}

fn calculate_gc_content(dna: DNA) -> f32 {
    let total_len = dna.0.len();
    let gc_len = dna.0.into_iter().filter(is_gc).collect::<Vec<Deoxyribonucleotide>>().len();
    (gc_len * 100) as f32 / total_len as f32
}

fn is_gc(nt: &Deoxyribonucleotide) -> bool {
    match nt {
        &Deoxyribonucleotide::A => false,
        &Deoxyribonucleotide::C => true,
        &Deoxyribonucleotide::G => true,
        &Deoxyribonucleotide::T => false,
    }
}

// since the trait `core::cmp::Ord` is not implemented for the type `f32`
fn max<T: Clone>(vec: Vec<(T, f32)>) -> Option<(T, f32)> {
    let mut maximum: Option<(T, f32)> = None;

    for (i, x) in vec {
        maximum = maximum.map_or(Some((i.clone(), x)), |(j, y)| {
            if y > x { Some((j, y)) } else { Some((i.clone(), x)) }
        });
    }
    return maximum;
}

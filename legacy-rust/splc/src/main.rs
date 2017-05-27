extern crate splc;

use std::io::prelude::*;
use std::fs::File;
use splc::bio::rna::*;
use splc::bio::fasta::*;
use splc::bio::dna::DNA;
use splc::bio::aa::*;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let fastas: Vec<Fasta<DNA>> = parse_fastas(buffer.trim());
    let mut sequence: Vec<RNA> = fastas[0].rna().collect();
    for intron in &fastas[1..] {
        let intron_rna: Vec<RNA> = intron.rna().collect();
        remove_intron(&mut sequence, &intron_rna);
    }
    let spliced = Fasta {
        id: fastas[0].id.clone(),
        sequence: sequence
    };

    for aa in spliced.aa() {
        print!("{}", aa);
    }
    println!("");
}

fn remove_intron(sequence: &mut Vec<RNA>, intron: &[RNA]) {

    // println!("{:?}", intron);
    let mut result = Vec::new();
    let mut i = 0;
    for start in indices(&sequence, intron) {
        result.extend_from_slice(&sequence[i .. start]);
        i = start + intron.len();
    }
    result.extend_from_slice(&sequence[i ..]);
    *sequence = result;
}

fn indices<T: Eq>(haystack: &[T], needle: &[T]) -> Vec<usize> {
    let mut vec = Vec::new();
    let mut i = 0;
    while i < haystack.len() {
        let rest = &haystack[i..];
        i = i + kmp(rest, needle) + 1;
        if i < haystack.len() {
            vec.push(i - 1);
        }
    }
    vec
}

fn kmp<T: Eq>(haystack: &[T], needle: &[T]) -> usize {
    let mut m = 0;
    let mut i = 0;

    let table = build_table(needle);
    while m + i < haystack.len() {
        if needle[i] == haystack[m+i] {
            if i + 1 == needle.len() {
                return m;
            }
            i = i + 1;
        } else {
            match table[i] {
                None => {
                    m = m + 1;
                    i = 0;
                },
                Some(n) => {
                    m = m + i - n;
                    i = n;
                }
            }
        }
    }
    haystack.len()
}

fn build_table<T: Eq>(s: &[T]) -> Vec<Option<usize>> {
    match s.len() {
        0 => Vec::new(),
        1 => vec![None],
        _ => {
            // let chars: Vec<char> = s.chars().collect();
            let mut table = Vec::new();
            table.push(None);
            table.push(Some(0));

            let mut pos = 2;
            let mut cnd = Some(0);

            while pos < s.len() {
                match cnd {
                    None => {}, // should never happen
                    Some(0) => {
                        if &s[pos] == &s[0] {
                            table.push(Some(1));
                            cnd = Some(1);
                        } else {
                            table.push(Some(0));
                        }
                        pos = pos + 1;
                    },
                    Some(n) => {
                        if &s[pos] == &s[n] {
                            table.push(Some(n + 1));
                            cnd = Some(n + 1);
                            pos = pos + 1;
                        } else {
                            cnd = table[n];
                        }
                    },
                }
            }
            return table
        }
    }

}

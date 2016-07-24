extern crate long;

use std::io::prelude::*;
use std::fs::File;

use long::fasta::*;
use long::contig::*;

use std::collections::BTreeMap;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();
    let input_str = buffer.trim();

    let mut map: BTreeMap<usize, Contig> = BTreeMap::new();
    for (i, s) in parse_fasta(input_str).into_iter().enumerate() {
        &map.insert(i, Contig::new(s.content));
    }

    let mut defending_champion = map.remove(&0).unwrap();

    while map.len() > 0 {
        let mut round_champion: Option<Contig> = None;
        let mut merged: Option<usize> = None;

        for (i, challenger) in map.iter() {
            let result = overlap(&defending_champion, &challenger);
            if result > round_champion {
                round_champion = result;
                merged = Some(*i);
            }
        }

        if let Some(key) = merged {
            map.remove(&key);
            defending_champion = round_champion.unwrap();
        }
    }
    println!("{}", defending_champion.val);




}

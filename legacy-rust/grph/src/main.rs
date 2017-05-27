use std::io::prelude::*;
use std::fs::File;
use std::collections::BTreeMap;

#[derive(Debug, Clone, Eq, PartialEq)]
struct Fasta<'a> {
    id: &'a str,
    content: String
}
type Suffix<'a> = &'a str;
type Prefix<'a> = &'a str;
type Map<'a> = BTreeMap<&'a str, Vec<&'a (Fasta<'a>)>>;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let string = buffer.trim();
    let fasta_vec = parse_fasta(string);

    let mut prefix_map: Map = BTreeMap::new();

    for x in &fasta_vec {
        insert(&mut prefix_map, get_prefix(x), x);
    }
    for head in &fasta_vec {
        let suffix = get_suffix(head);
        if let Some(tail_vec) = prefix_map.get(suffix) {
            for tail in tail_vec {
                if head != *tail {
                    println!("{} {}", head.id, tail.id);
                }
            }
        }
    }
}

fn get_suffix<'a>(x : &'a Fasta) -> Suffix<'a> {
    let len = x.content.len();
    return &x.content[len - 3 ..];
}

fn get_prefix<'a>(x : &'a Fasta) -> Prefix<'a> {
    return &x.content[0 .. 3];
}

fn insert<'a>(map: &mut Map<'a>, key: &'a str, x: &'a Fasta<'a>) {
    if map.contains_key(key) {
        if let Some(val) = map.get_mut(key) {
            val.push(x);
        }
    } else {
        map.insert(key, vec![x]);
    }
}

fn parse_fasta(s: &str) -> Vec<Fasta> {
    let mut vec = Vec::new();

    let mut id = "";
    let mut content = String::new();

    for (i, line) in s.lines().enumerate() {
        if &line[0 .. 1] == ">" {
            if i != 0 {
                // should push and reinitialize
                vec.push(Fasta {
                    id: id,
                    content: content.clone()
                });
            }
            id = &line[1..];
            content = String::new();
        } else {
            content.push_str(line);
        }
    }
    // push the last result
    vec.push(Fasta {
        id: id,
        content: content.clone()
    });

    return vec
}

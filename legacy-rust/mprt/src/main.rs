extern crate mprt;
extern crate hyper;

use std::io::prelude::*;
use std::fs::File;
use hyper::client::*;
use hyper::error::Result;
use mprt::bio::*;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let ids: Vec<_> = buffer.trim().lines().collect();
    let iter = ids.clone().into_iter()
        .map(fetch_fasta)
        .filter(|x| x.is_ok())
        .map(|x| parse_fasta(&x.unwrap()[..]));
    for (i, fasta) in iter.enumerate() {
        let result = search(&fasta);
        if result.len() > 0 {
            println!("{}", ids[i]);
            for i in result {
                print!("{} ", i);
            }
            println!("");
        }
    }
}

fn fetch_fasta(id: &str) -> Result<String> {
    let client = Client::new();
    let mut res = try!(client.get(&("http://www.uniprot.org/uniprot/".to_owned() + id + ".fasta")).send());
    let mut buf = String::new();
    try!(res.read_to_string(&mut buf));
    Ok(buf)
}

fn search(fasta: &Fasta) -> Vec<usize> {
    let mut result = Vec::new();
    let s = &fasta.content;
    for i in 0 .. s.len() - 3 {
        let c0 = &s[i     .. i + 1] == "N";
        let c1 = &s[i + 1 .. i + 2] != "P";
        let c2 = &s[i + 2 .. i + 3] == "S" || &s[i + 2 .. i + 3] == "T";
        let c3 = &s[i + 3 .. i + 4] != "P";
        if c0 && c1 && c2 && c3 {
            result.push(i + 1);
        }
    }
    result
}

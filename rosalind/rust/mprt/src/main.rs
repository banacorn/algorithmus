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
    let iter = ids.into_iter()
        .map(get_fasta)
        .filter(|x| x.is_ok())
        .map(|x| parse_fasta(&x.unwrap()[..]));
    for result in iter {
        println!("{:?}", result);
    }
}

fn get_fasta(id: &str) -> Result<String> {
    let client = Client::new();
    let mut res = try!(client.get(&("http://www.uniprot.org/uniprot/".to_owned() + id + ".fasta")).send());
    let mut buf = String::new();
    try!(res.read_to_string(&mut buf));
    Ok(buf)
}

extern crate orf;

use std::io::prelude::*;
use std::fs::File;
use orf::bio::fasta::*;
use orf::bio::aa::*;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let fasta = parse_fasta(buffer.trim()).unwrap();

    let mut result: Vec<Vec<AA>> = Vec::new();
    for frame in fasta.reading_frames() {
        let mut threads: Vec<Vec<AA>> = Vec::new();
        for aa in frame {
            match aa {
                // start a new thread
                AA::Met => {
                    threads.push(Vec::new());
                    for thread in threads.iter_mut() {
                        thread.push(aa);
                    }
                },
                // transfer all threads to the result
                AA::Stop => {
                    for thread in &threads {
                        result.push(thread.clone());
                    }
                    threads = Vec::new();
                },
                _ => {
                    for thread in threads.iter_mut() {
                        thread.push(aa);
                    }
                }
            }
        }
    }

    // println!("{:?}", result);
    result.sort();
    result.dedup();
    for seq in result {
        for aa in seq {
            print!("{}", aa);
        }
        println!("");
    }
}

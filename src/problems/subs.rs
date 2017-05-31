use super::super::bio::*;
use std::fmt;

pub fn run(input: &str) -> String {

    let lines: Vec<&str> = input.lines().collect();
    let haystack = parse_dna(&lines[0]);
    let needle = parse_dna(&lines[1]);

    let result = indices(&haystack.0, &needle.0);
    let mut output = String::new();
    for i in result {
        output = output + &i.to_string() + " ";
    }
    output.trim().to_string()
}


fn indices<T: Eq + fmt::Debug>(haystack: &[T], needle: &[T]) -> Vec<usize> {
    let mut vec = Vec::new();
    let mut i = 0;
    while i < haystack.len() {
        let rest = &haystack[i..];
        i = i + kmp(rest, needle) + 1;
        if i < haystack.len() {
            vec.push(i);
        }
    }
    vec
}

fn kmp<T: Eq + fmt::Debug>(haystack: &[T], needle: &[T]) -> usize {
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
                0 => {
                    m = m + 1;
                    i = 0;
                },
                n => {
                    m = m + 1 + i - n;
                    i = n;
                }
            }
        }
    }
    haystack.len()
}


fn build_table<T: Eq>(string: &[T]) -> Vec<usize> {


    match string.len() {
        0 => Vec::new(),
        len => {
            let mut table = vec![0];

            // matched prefix length
            let mut matched = 0;

            // start scanning from the second character of the input string
            for pos in 1 .. len {

                // in case the character we are looking at matches with the prefix
                // increase the matched prefix length and keep looking
                // else reset the matched prefix length
                if string[matched] == string[pos] {
                    matched = matched + 1;
                } else {
                    matched = 0;
                }
                table.push(matched);
            }
            table
        }
    }
}

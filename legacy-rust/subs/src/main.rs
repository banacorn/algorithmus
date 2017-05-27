use std::io::prelude::*;
use std::fs::File;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let input: Vec<&str> = buffer.trim().lines().collect();
    let haystack = &input[0];
    let needle = &input[1];
    // println!("{:?}", build_table(needle));
    for i in indices(haystack, needle) {
        print!("{} ", i);
    }
    println!("");
}

fn indices(haystack: &str, needle: &str) -> Vec<usize> {
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

fn kmp(haystack: &str, needle: &str) -> usize {
    let mut m = 0;
    let mut i = 0;

    let haystack_vec: Vec<char> = haystack.chars().collect();
    let needle_vec: Vec<char> = needle.chars().collect();
    let table = build_table(needle);

    while m + i < haystack.len() {
        if needle_vec[i] == haystack_vec[m+i] {
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

fn build_table(s: &str) -> Vec<Option<usize>> {
    match s.len() {
        0 => Vec::new(),
        1 => vec![None],
        _ => {
            let chars: Vec<char> = s.chars().collect();
            let mut table = Vec::new();
            table.push(None);
            table.push(Some(0));

            let mut pos = 2;
            let mut cnd = Some(0);

            while pos < s.len() {
                match cnd {
                    None => {}, // should never happen
                    Some(0) => {
                        if &chars[pos] == &chars[0] {
                            table.push(Some(1));
                            cnd = Some(1);
                        } else {
                            table.push(Some(0));
                        }
                        pos = pos + 1;
                    },
                    Some(n) => {
                        if &chars[pos] == &chars[n] {
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

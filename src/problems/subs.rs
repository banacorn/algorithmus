use super::super::bio::*;

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


fn indices<T: Eq>(haystack: &[T], needle: &[T]) -> Vec<usize> {
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

fn build_table<T: Eq>(vec: &[T]) -> Vec<Option<usize>> {
    match vec.len() {
        0 => Vec::new(),
        1 => vec![None],
        _ => {
            let mut table = Vec::new();
            table.push(None);
            table.push(Some(0));

            let mut pos = 2;
            let mut cnd = Some(0);

            while pos < vec.len() {
                match cnd {
                    None => {}, // should never happen
                    Some(0) => {
                        if vec[pos] == vec[0] {
                            table.push(Some(1));
                            cnd = Some(1);
                        } else {
                            table.push(Some(0));
                        }
                        pos = pos + 1;
                    },
                    Some(n) => {
                        if vec[pos] == vec[n] {
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

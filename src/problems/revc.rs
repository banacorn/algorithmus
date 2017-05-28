pub fn run(input: &String) -> String {
    input.chars().map(complement).rev().collect()
}

fn complement(c: char)-> char {
    match c {
        'A' => 'T',
        'C' => 'G',
        'G' => 'C',
        'T' => 'A',
        others => others,
    }
}

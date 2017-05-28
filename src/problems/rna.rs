pub fn run(input: &String) -> String {
    input.chars().map(transcript).collect()
}

fn transcript(c: char) -> char {
    match c {
        'T' => 'U',
        others => others
    }
}

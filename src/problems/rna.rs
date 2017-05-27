pub fn run(input: &String) -> String {
    let mut output = String::new();
    for c in input.chars() {
        match c {
            'T' => output.push('U'),
            others => output.push(others),
        }
    }
    output
}

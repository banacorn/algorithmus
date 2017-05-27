pub fn run(input: &String) -> String {
    let mut a = 0;
    let mut c = 0;
    let mut g = 0;
    let mut t = 0;

    for x in input.chars() {
        match x {
            'A' => a = a + 1,
            'C' => c = c + 1,
            'G' => g = g + 1,
            'T' => t = t + 1,
            _ => {},
        }
    }

    format!("{} {} {} {}", a, c, g, t)
}

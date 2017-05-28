pub fn run(input: &str) -> String {
    let mut iter = input.split_whitespace();
    let k: u64 = iter.next().unwrap().parse().unwrap();
    let n: u64 = iter.next().unwrap().parse().unwrap();
    format!("{}", fib(k, n))
}

fn fib(k: u64, n: u64) -> u64 {
    let mut vec: Vec<u64> = Vec::new();
    vec.push(1);
    vec.push(1);

    for i in 2..k {
        let grandparent = vec[(i - 2) as usize];
        let parent      = vec[(i - 1) as usize];
        vec.push(grandparent * n + parent);
    }
    let last: &u64 = vec.last().unwrap();
    return *last;
}

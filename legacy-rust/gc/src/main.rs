use std::io::prelude::*;
use std::fs::File;
use std::fmt::Debug;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let string = buffer.trim();
    let vec = parse_fasta(string);
    let tagged: Vec<_> = (0..).zip(vec.clone().into_iter().map(calculate_gc_content)).collect();
    let (index, val): (usize, f32) = max(&tagged).unwrap();
    println!("{}", &vec[index].id);
    println!("{}", val);
    // let percentage: Vec<f32> = )

    //
    // println!("{:?}", zipped.into_iter().max());
    //
    // for i in iter {
    //
    // }
    // let percentage: Vec<f32> = vec.into_iter().zip((0..))
        // .map(calculate_gc_content).collect();
    // println!("{:?}", percentage);

}

#[derive(Debug, Clone)]
struct Fasta<'a> {
    id: &'a str,
    content: String
}

fn parse_fasta(s: &str) -> Vec<Fasta> {
    let mut vec = Vec::new();

    let mut id = "";
    let mut content = String::new();

    for (i, line) in s.lines().enumerate() {
        if &line[0 .. 1] == ">" {
            if i != 0 {
                // should push and reinitialize
                vec.push(Fasta {
                    id: id,
                    content: content.clone()
                });
            }
            id = &line[1..];
            content = String::new();
        } else {
            content.push_str(line);
        }
    }
    // push the last result
    vec.push(Fasta {
        id: id,
        content: content.clone()
    });

    return vec
}

fn calculate_gc_content(x: Fasta) -> f32 {
    let total_len = x.content.len();
    let filtered: String = x.content.chars().filter(is_gc).collect();
    (filtered.len() * 100) as f32 / total_len as f32
}

fn is_gc(c: &char) -> bool {
    *c == 'C' || *c == 'G'
}

// since the trait `core::cmp::Ord` is not implemented for the type `f32`
fn max<T : Copy + Debug>(vec: &Vec<(T, f32)>) -> Option<(T, f32)> {
    let mut maximum: Option<(T, f32)> = None;

    for &(i, x) in vec {
        maximum = maximum.map_or(Some((i, x)), |(j, y)| {
            if y > x { Some((j, y)) } else { Some((i, x)) }
        });
    }
    return maximum;
}

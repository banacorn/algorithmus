use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Fasta<'a> {
    pub id: &'a str,
    pub content: String
}

impl<'a> fmt::Display for Fasta<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.id, self.content)
    }
}

pub fn parse_fasta(s: &str) -> Vec<Fasta> {
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

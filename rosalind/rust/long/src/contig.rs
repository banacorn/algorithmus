// extern crate long;

// use std::io::prelude::*;
// use std::fs::File;
use std::cmp::Ordering;
use std::cmp;


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Contig {
    pub val: String,
    pub count: usize
}

impl Contig {
    pub fn new(s: String) -> Contig {
        Contig {
            val: s,
            count: 0
        }
    }
}

impl PartialOrd for Contig {
    fn partial_cmp(&self, other: &Contig) -> Option<Ordering> {
        if self.count == other.count {
            // compare string length if the counts are the same
            // the smaller the better
            let self_len  = self.val.len();
            let other_len = other.val.len();
            Some(Ordering::reverse(self_len.cmp(&other_len)))
        } else {
            Some(self.count.cmp(&other.count))
        }
    }
}

impl Ord for Contig {
    fn cmp(&self, other: &Contig) -> Ordering {
        match self.partial_cmp(other) {
            Some(ord) => ord,
            None      => Ordering::Equal
        }
    }
}

// O(length x + length y)
pub fn overlap(x: &Contig, y: &Contig) -> Option<Contig> {

    //  slide one over the another, return the shortest overlapped result
    let x_len = x.val.len();
    let y_len = y.val.len();

    let longer      = if x_len > y_len { &x } else { &y };
    let longer_len  = longer.val.len();
    let shorter     = if x_len > y_len { &y } else { &x };
    let shorter_len = shorter.val.len();

    let mut best: Option<Contig> = None;

    //  longer           ===============
    //  shorter   --------
    //                                 --------
    for i in 1 .. longer_len + shorter_len {
        let longer_slice_start = cmp::max(0,          i as i32 - shorter_len as i32) as usize;
        let longer_slice_end   = cmp::min(longer_len, i) as usize;
        let longer_slice       = &longer.val[longer_slice_start .. longer_slice_end];

        let shorter_slice_start = cmp::max(0,          shorter_len as i32 - i as i32) as usize;
        let shorter_slice_end   = cmp::min(shorter_len, longer_len + shorter_len - i) as usize;
        let shorter_slice       = &shorter.val[shorter_slice_start .. shorter_slice_end];

        if longer_slice == shorter_slice {
            let shorter_init_slice = &shorter.val[0 .. shorter_slice_start];
            let shorter_tail_slice = &shorter.val[shorter_slice_end .. ];
            best = cmp::max(best, Some(Contig {
                val: format!("{}{}{}", shorter_init_slice, longer.val, shorter_tail_slice),
                count: 1 + cmp::max(longer.count, shorter.count)
            }));
        }

    }
    best
}

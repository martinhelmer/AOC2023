#![allow(unused)]

use crate::util;
use array2d::Array2D;
use itertools::{Itertools, enumerate, all};
use core::num;
use std::cmp::{max, self};
use std::collections::HashSet;
use std::iter::zip;
use std::str;
use std::collections::BTreeMap;

pub const NAME: &str = "Day 12: Hot Springs";

pub fn data() -> String {
    util::get_input("day12.txt")
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Row {
    pat: String,
    nums: Vec<usize>,
    unknowns: Vec<usize>,
    num_known_damaged: usize,
    num_tot_damaged: usize,
}

// ???.### 1,1,3
fn parse_row(s: &str) -> Row {
    let (pat, nums) = s.split_once(" ").unwrap();
    let nums: Vec<_> = nums.split(',').map(|a| a.parse().unwrap()).collect();
    get_row(pat.to_string(), nums)
}

fn parse_row2(s: &str) -> Row {
    let (pat, nums) = s.split_once(" ").unwrap();
    let nums: Vec<usize> = nums.split(',').map(|a| a.parse().unwrap()).collect();
    let p = [pat,pat,pat,pat,pat].join("?");
    let n: Vec<usize> = nums.repeat(5);
    get_row(p.to_string(), n)
}

fn get_row(pat : String, nums : Vec<usize>) -> Row {
    let pat = pat.trim_start_matches('.');
    let unknowns: Vec<usize> = pat
        .chars()
        .enumerate()
        .filter_map(|(i, c)| if c == '?' { Some(i) } else { None })
        .collect();
    let num_known_damaged = pat.chars().filter(|c| c == &'#').collect::<Vec<_>>().len();
    let num_tot_damaged = nums.iter().sum();
    Row {
        pat:pat.to_string(),
        nums,
        unknowns,
        num_known_damaged,
        num_tot_damaged,
        }
}

pub fn part01(data: String) -> usize {
    let q = data.lines().map(|r|{let row = parse_row(r); 
        let n = nf3(&mut BTreeMap::new(), &row.pat, row.nums);
        n });
    let s = q.sum();
    println!("{}", s);
    s
}

pub fn part02(data: String) -> usize {
    let q = data.lines().map(|r|{let row = parse_row2(r); 
                                  let n = nf3(&mut BTreeMap::new(), &row.pat, row.nums);
                                  n });
    let s = q.sum();
    println!("{}", s);
    s
}


fn is_valid_chunk(chunk : &str)-> bool {
    let lix = chunk.len()-1;
    all(chunk[..lix].as_bytes(), |c| *c == b'#' || *c == b'?') && (chunk.as_bytes()[lix] == b'?' || chunk.as_bytes()[lix] == b'.')
}
fn grab(pat : &str, s : usize ) -> Vec<&str> {
    // println!("pat {} s {}", pat, s);
    let mut pat = pat.trim_start_matches('.');
    let mut res = vec![];
    loop {  
        if pat.len() < s {
            return res;
        }
        if pat.len() == s {
            if all(pat.as_bytes(), |c| *c == b'?' || *c == b'#') {
               res.push("");               
            } 
            return res;
        }  
        let (chunk, rem) = pat.split_at(s+1);
        if is_valid_chunk(chunk) {
            res.push(rem);
        }
        if chunk.starts_with('#') {
            return res;
        }
        pat = &pat[1..];
    }
    res
}

fn nf3<'a>(hm : &mut BTreeMap<(usize,usize) , usize>, pat :&str, chunks : Vec<usize>) -> usize {
    match hm.get(&(pat.len(), chunks.len()))  {
        Some(v) => {return *v;}, 
        None => (),
    }
    if chunks.len() == 0 {
        match pat.contains("#") {
            true => {return 0;},
            false => {return 1;},
        }
    }
    if pat.len() == 0{
        return 0;
    }
    
    let grabs : Vec<&str> = grab(pat, chunks[0]);
    let mut sum = 0;
    for grab in grabs{
        sum += nf3(hm, grab, chunks[1..].to_vec());
    }
    hm.insert((pat.len(), chunks.len()), sum);
    sum
}

#[cfg(test)]
mod test {
    use super::*;
    fn testRow() -> Row {
        Row {
            pat: "???.###".to_string(),
            nums: vec![1, 1, 3],
            unknowns: vec![0, 1, 2],
            num_known_damaged: 3,
            num_tot_damaged: 5,
        }
    }
    #[test]
    fn parse_row() {
        assert_eq!(super::parse_row("???.### 1,1,3"), testRow());
    }
    #[test]
    fn gr() {
        assert_eq!(super::get_row("#".to_string(), vec![]),        Row {
            pat: "#".to_string(),
            nums: vec![],
            unknowns: vec![],
            num_known_damaged: 1,
            num_tot_damaged: 0,
        } );
    }

    #[test]
    fn grb() {
        assert_eq!(super::grab(&"#.#", 1),vec!["#"]);
        assert_eq!(super::grab(&"???#?.#", 2),vec!["#?.#",".#","#"]);
        // assert_eq!(super::grab(&"#.#", 1),vec!["#"]);
    }
    #[test]
    fn nf3_() {
        assert_eq!(super::nf3(&mut BTreeMap::new(), &"?###????????", vec![3,2,1]),10);
        assert_eq!(super::nf3(&mut BTreeMap::new(), &".??..??...?##.", vec![1,1,3]),4);
        let r = super::parse_row2(".??..??...?##. 1,1,3");
        assert_eq!(super::nf3(&mut BTreeMap::new(), &r.pat, r.nums),16384);
        let r = super::parse_row2("?###???????? 3,2,1");
        assert_eq!(super::nf3(&mut BTreeMap::new(), &r.pat, r.nums),506250);
        let r = super::parse_row2("#?#???????#?.? 3,1,2,2");
        assert_eq!(super::nf3(&mut BTreeMap::new(),  &r.pat, r.nums),58564);
    }

}


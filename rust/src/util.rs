#![allow(unused)]
use std::{fs, num::ParseIntError, cmp::Reverse} ;
use array2d::Array2D;

pub fn get_input(f : &str) -> String  {
    let contents = fs::read_to_string(["../input/",f].join(""))
    .expect("Should have been able to read the file");
    contents
}

pub fn s_to_ints(s: &str) -> Result<Vec<usize>, ParseIntError> {
    s.split_whitespace().map(|x| x.parse::<usize>()).collect()
}


fn string_as_int(s: &String) -> u32 {
    s.as_bytes()
        .iter()
        .fold(0, |acc, c| acc * 2 + if c == &b'#' { 1 } else { 0 })
}

// array
pub fn a2dc_to_grid(a :& Array2D<char>) -> String {
    let x : Vec<String> = a.rows_iter().map(|row| row.collect::<String>()).collect();
    x.join("\n")
}

pub fn grid_to_a2d(s: &str) -> Array2D<char> {
let l: Vec<Vec<char>> = s.lines().map(|q| q.chars().collect()).collect();
Array2D::from_rows(&l).unwrap()
}

pub fn num_to_a2d(s: &str) -> Array2D<u32> {
    let l: Vec<Vec<u32>> = s.lines().map(|q| q.chars().map(|c| c.to_digit(10).unwrap()).collect()).collect();
    Array2D::from_rows(&l).unwrap()
    }


pub fn flipy(a : Array2D<char>) -> Array2D<char> {
        let mut a2 : Vec<Vec<char>> = vec![];
    for  mut r in a.as_rows() {
        r.reverse();
        a2.push(r);
    }
    Array2D::from_rows(&a2).unwrap()
}

pub fn fliptldr(a : Array2D<char>) -> Array2D<char> {
    Array2D::from_rows(&a.as_columns()).unwrap()
}

pub fn rotleft (a : Array2D<char>) -> Array2D<char> {
  fliptldr(flipy(a))
}

pub fn rotright (a : Array2D<char>) -> Array2D<char> {
    flipy(fliptldr(a))
  }

#[cfg(test)]
mod test {
    use crate::util;

        #[test]
    fn flipy() {
        let y: String = String::from("123\n456");
        assert_eq!(util::a2dc_to_grid(& util::flipy(util::grid_to_a2d(&y))),String::from("321\n654") );
    }
    #[test]
    fn fliptldr() {
        let x: String = String::from("123\n456");
        assert_eq!(util::a2dc_to_grid(& util::fliptldr(util::grid_to_a2d(&x))),String::from("14\n25\n36") );
    }
    #[test]
    fn rotleft() {
        let x: String = String::from("123\n456");
        assert_eq!(util::a2dc_to_grid(& util::rotleft(util::grid_to_a2d(&x))),String::from("36\n25\n14") );
    }

    #[test]
    fn rotright() {
        let x: String = String::from("123\n456");
        assert_eq!(util::a2dc_to_grid(& util::rotright(util::grid_to_a2d(&x))),String::from("41\n52\n63") );
    }

}


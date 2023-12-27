#![allow(unused)]
use std::{fs, num::ParseIntError, cmp::Reverse} ;
use array2d::Array2D;
use std::ops;

#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone, Ord, PartialOrd)]
pub struct Pos(pub i32, pub i32);
#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone, Ord, PartialOrd)]
pub struct Dir(pub i32, pub i32);

impl ops::Add<Dir> for Pos {
    type Output = Pos;

    fn add(self, _rhs: Dir) -> Pos {
        Pos(self.0 + _rhs.0, self.1 + _rhs.1) 
    }
}

impl Pos {
    pub fn as_a2d_index(&self) -> (usize, usize) {
        (self.0 as usize, self.1 as usize)
    }
}

pub const EAST: Dir = Dir(0, 1);
pub const WEST: Dir = Dir(0, -1);
pub const NORTH: Dir = Dir(-1, 0);
pub const SOUTH: Dir = Dir(1, 0);


pub fn rl(dir: Dir) -> Dir {
    Dir(-dir.1, dir.0)
}

pub fn rr(dir: Dir) -> Dir {
    Dir(dir.1, -dir.0)
}

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

// array ---------------------------------
pub type Grid  = Array2D<char>; 

pub fn ok_pos<T>(a :& Array2D<T>, p: Pos) -> bool {
    p.0 >= 0 && p.1 >= 0 && p.0 < (a.num_rows() as i32) && p.1 < a.num_columns() as i32
}

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


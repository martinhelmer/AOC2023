#![allow(unused)]
use std::{fs, num::ParseIntError} ;
use array2d::Array2D;

pub fn get_input(f : &str) -> String  {
    let contents = fs::read_to_string(["../input/",f].join(""))
    .expect("Should have been able to read the file");
    contents
}

pub fn s_to_ints(s: &str) -> Result<Vec<usize>, ParseIntError> {
    s.split_whitespace().map(|x| x.parse::<usize>()).collect()
}


pub fn grid_to_a2d(s: &str) -> Array2D<char> {
let l: Vec<Vec<char>> = s.lines().map(|q| q.chars().collect()).collect();
Array2D::from_rows(&l).unwrap()
}

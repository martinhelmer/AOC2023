#![allow(unused)]

use crate::util::{self, rotright};
use array2d::Array2D;
use core::num;
use itertools::{all, enumerate, Itertools};
use std::cmp::{self, max};
use std::collections::HashMap as MMap;
use std::iter::zip;
use std::str;
use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;

pub const NAME: &str = "Day 14: Parabolic Reflector Dish";


pub fn example() -> String {
    String::from(
"O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....",
    )
}


pub fn data() -> String {
    util::get_input("day14.txt")
}
fn parse_input<'a>(data: &'a str) -> Vec<Vec<String>> {
    let mut v: Vec<_> = vec![];
    for chunk in data.split("\n\n").clone() {
        let mut sc = vec![];
        for line in chunk.lines() {
            sc.push(line.to_string())
        }
        v.push(sc)
    }
    v
}

fn rollnorth(a : Array2D<char>) -> Array2D<char> 
{
    let mut movetoix: Option<usize> = None;
    let mut done: bool;
    let mut newgrid = vec![];

    for mut col in a.as_columns() {
        loop {
            done = true;
            movetoix = None;
            for cix in 0..col.len() {
                let char = col[cix];
                if char == '.' {
                    if movetoix == None {
                        movetoix = Some(cix);
                    }
                    continue;
                }
                if char == '#' {
                    movetoix = None;
                    continue;
                }
                if char == 'O' {
                    match movetoix {
                        None => continue,
                        Some(mix) => {
                            col[mix] = 'O';
                            col[cix] = '.';
                            done = false;
                            movetoix = Some(mix+1);
                            continue
                                      }
                                    }
                                }
                panic!("Should not be here")
            }
            if done {
                break
            }
        }
        newgrid.push(col);
       
    }
    Array2D::from_columns(&newgrid).unwrap()
}

fn calc_col_weight(col :& Vec<char> ) -> usize {
    let l = col.len();
    col.iter().enumerate().map(|(ix, c)| if c == &'O' { l-ix } else {0}).sum() 
}
fn weight(a : & Array2D<char>) -> usize {
    let mut s = 0;
    for c in a.as_columns() {
        s+= calc_col_weight(&c);
    }
    s
}
fn cycle(a : Array2D<char>) -> Array2D<char> {
    let mut b = a;
    for i in 0..4 {
        b = rotright(rollnorth(b));
    }
    b
}


fn my_hash<T>(obj: T) -> u64
where
    T: Hash,
{
    let mut hasher = DefaultHasher::new();
    obj.hash(&mut hasher);
    hasher.finish()
}

//
pub fn part01(data: String) -> usize {
    let mut vvc: Vec<Vec<char>> = data.lines().map(|c| c.chars().collect()).collect();
    let mut a = Array2D::from_rows(&vvc).unwrap();
    let b = rollnorth(a);
    weight(&b)
}

pub fn part02(data: String) -> usize {
    let mut vvc: Vec<Vec<char>> = data.lines().map(|c| c.chars().collect()).collect();
    let mut a = Array2D::from_rows(&vvc).unwrap();
    let mut b = a;
    let mut visited : MMap<u64, usize> = MMap::new();
    let mut i = 0;
    let mut h = 0;
    let mut history = vec![weight(&b)];
   loop {
        i += 1;
        b = cycle(b);
        h = my_hash(&b);
        history.push(weight(&b));
        // println!("{} {} {}", i, weight(&b),h);
        if visited.contains_key(&h) {
            let cs = visited[&h];
            let ce = i; 
            let lead_in = cs -1 ;
            let ix = ((1_000_000_000 -lead_in) % (ce - cs)) + lead_in; 
            // println!("found cycle after {} iterations, starting at {}, ix = {}", i, visited[&h], ix);
            return history[ix]
        }
        visited.insert(h,i);
    }
    0
}

#[cfg(test)]
mod test_result {
    use super::*;
    #[test]
    fn part1() {
        let x: String = String::from(
"O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....",
        );
        assert_eq!(part01(x), 136);
        assert_eq!(part01(data()),109833 );
    }

    #[test]
    fn part2() {

        assert_eq!(part02(example()), 64);
        assert_eq!(part02(data()),99875 )
    }
}

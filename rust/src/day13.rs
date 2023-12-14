#![allow(unused)]

use crate::util;
use array2d::Array2D;
use core::num;
use itertools::{all, enumerate, Itertools};
use std::cmp::{self, max};
use std::collections::HashMap as QQ;
use std::collections::HashSet;
use std::iter::zip;
use std::str;

pub const NAME: &str = "Day 13: Point of Incidence";

pub fn data() -> String {
    util::get_input("day13.txt")
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

fn solve(data: String, diff_target: usize) -> usize {
    parse_input(&data)
        .iter()
        .map(|r| match find_mirror_rownum(&r, diff_target) {
            Some(row) => row * 100,
            None => match find_mirror_rownum(&transpose_strvec(&r), diff_target) {
                Some(col) => col,
                None => {
                    println!("{}", r.join("\n"));
                    panic!("Didn't find mirror axis!")
                }
            },
        })
        .sum()
}

// 34911
pub fn part01(data: String) -> usize {
    solve(data, 0)
}

pub fn part02(data: String) -> usize {
    solve(data, 1)
}

fn transpose_strvec(v: &Vec<String>) -> Vec<String> {
    let ss = v.iter().join("\n");
    let a = util::grid_to_a2d(&ss);
    let q = a
        .as_columns()
        .iter()
        .map(|v| v.iter().collect::<String>())
        .collect();
    q
}

fn string_as_int(s: &String) -> u32 {
    s.as_bytes()
        .iter()
        .fold(0, |acc, c| acc * 2 + if c == &b'#' { 1 } else { 0 })
}

fn find_mirror_rownum(v: &Vec<String>, diff_target: usize) -> Option<usize> {
    let ints: Vec<u32> = v.iter().map(|s| string_as_int(s)).collect();
    (0..(ints.len() - 1))
        .filter_map(|j| {
            if fold_diff(&ints, j) == diff_target {
                Some(j + 1)
            } else {
                None
            }
        })
        .next()
}

// count bit mismmatch, stop if diff > 1
fn fold_diff(v: &Vec<u32>, pos: usize) -> usize {
    let mut top_ix = pos;
    let mut bottom_ix = pos + 1;
    let mut diffsum = 0;
    loop {
        diffsum += (v[top_ix] ^ v[bottom_ix]).count_ones();
        if diffsum > 1 || top_ix == 0 || bottom_ix == v.len() - 1 {
            return diffsum as usize;
        }
        top_ix -= 1;
        bottom_ix += 1;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(
            super::find_mirror_rownum(
                &vec![
                    "##.##.##.##".to_string(),
                    ".#.##.##.##".to_string(),
                    "..#.##..##.".to_string(),
                    "###....#..#".to_string(),
                    "#..#.###...".to_string(),
                    "#..#.###...".to_string(),
                    "###....#..#".to_string(),
                ],
                0
            ),
            Some(5)
        );
    }
    #[test]
    fn transpose() {
        let inp = vec!["abc".to_string(), "def".to_string()];
        assert_eq!(
            transpose_strvec(&inp),
            vec!["ad".to_string(), "be".to_string(), "cf".to_string()]
        )
    }

    #[test]
    fn ttt() {
        let x: Vec<String> = String::from(
            "#...#.##..##..#.#
.###.#......#.#..
##.##..#...#.####
.#..##.#..#.###..
..#.#..#..###..#.
..##....###.#.###
#..#..#####..##.#
...##...##..#..#.
...##...##..#..#.
#..#..#####..##.#
.###....###.#.###
..#.#..#..###..#.
.#..##.#..#.###..
##.##..#...#.####
.###.#......#.#..
#...#.##..##..#.#
#...#.##..##..#.#",
        )
        .lines()
        .map(|l| l.to_string())
        .collect();
        assert_eq!(find_mirror_rownum(&x, 0), Some(16))
    }

    #[test]
    fn fold() {
        let v = vec![0b001, 0b111, 0b111, 0b001, 0b100];
        assert_eq!(super::fold_diff(&v, 0), 2);
        assert_eq!(super::fold_diff(&v, 1), 0);
        assert_eq!(super::fold_diff(&v, 3), 2);
    }
}

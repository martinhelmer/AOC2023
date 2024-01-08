#![allow(unused)]

use crate::util::{self, a2dc_to_grid, rotright, Dir, Pos, EAST, NORTH, SOUTH, WEST};
use array2d::Array2D;
use core::num;
use itertools::{all, chain, enumerate, Itertools};
use std::cmp::{self, max};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap as MMap;
use std::f32::consts::E;
use std::hash::{Hash, Hasher};
use std::iter::zip;
use std::str;

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

fn roll_any_dir(a: &mut Array2D<char>, dir: Dir) {
    let mut moveto_pos: Option<Pos>;
    assert_eq!(a.num_columns(), a.num_rows());
    let grid_size = a.num_rows() as i32;
    for outer_index in 0..grid_size {
        moveto_pos = None;
        for inner_index in 0..grid_size {
            let this_pos = match dir {
                NORTH => Pos(inner_index, outer_index),
                SOUTH => Pos(grid_size - inner_index - 1, outer_index),
                EAST => Pos(outer_index, grid_size - inner_index - 1),
                WEST => Pos(outer_index, inner_index),
                _ => panic!("Invalid direction"),
            };
            match a[this_pos.as_a2d_index()] {
                '.' => {
                    if moveto_pos == None {
                        moveto_pos = Some(this_pos)
                    }
                }
                '#' => moveto_pos = None,
                'O' => {
                    if let Some(m_pos) = moveto_pos {
                        a.set(m_pos.0 as usize, m_pos.1 as usize, 'O');
                        a.set(this_pos.0 as usize, this_pos.1 as usize, '.');
                        moveto_pos = Some(m_pos - dir);
                    }
                }
                _ => panic!("Invalid cell contents"),
            }
        }
    }
}

fn weight(a: &Array2D<char>) -> usize {
    a.enumerate_column_major()
        .filter_map(|((r, c), elem)| {
            if elem == &'O' {
                Some(a.num_columns() - r)
            } else {
                None
            }
        })
        .sum()
}
fn cycle(a: &mut Array2D<char>) {
    [NORTH, WEST, SOUTH, EAST].map(|d| roll_any_dir(a, d));
}

fn my_hash<T>(obj: T) -> u64
where
    T: Hash,
{
    let mut hasher = DefaultHasher::new();
    obj.hash(&mut hasher);
    hasher.finish()
}

pub fn part01(data: String) -> usize {
    let mut vvc: Vec<Vec<char>> = data.lines().map(|c| c.chars().collect()).collect();
    let mut a = Array2D::from_rows(&vvc).unwrap();
    roll_any_dir(&mut a, NORTH);
    //println!("{}", a2dc_to_grid(&a));
    weight(&a)
}

pub fn part02(data: String) -> usize {
    let mut vvc: Vec<Vec<char>> = data.lines().map(|c| c.chars().collect()).collect();
    let mut a = Array2D::from_rows(&vvc).unwrap();
    let mut visited: MMap<u64, usize> = MMap::new();
    let mut i = 0;
    let mut h = 0;
    let mut history = vec![weight(&a)];
    loop {
        i += 1;
        cycle(&mut a);
        h = my_hash(&a);
        history.push(weight(&a));
        // println!("{} {} {} ", i, weight(&b),h);
        if visited.contains_key(&h) {
            let cs = visited[&h];
            let ce = i;
            let lead_in = cs - 1;
            let ix = ((1_000_000_000 - lead_in) % (ce - cs)) + lead_in;
            //println!("found cycle after {} iterations, starting at {}, ix = {}", i, visited[&h], ix);
            return history[ix];
        }
        visited.insert(h, i);
    }
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
        assert_eq!(part01(data()), 109833);
    }

    #[test]
    fn part2() {
        assert_eq!(part02(example()), 64);
        assert_eq!(part02(data()), 99875)
    }
}

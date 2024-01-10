use std::collections::HashSet;

use array2d::Array2D;

use crate::util::{self, grid_to_a2d, Dir, Pos, EAST, NORTH, SOUTH, WEST};

pub const NAME: &str = "Day 10: Pipe Maze";

pub fn _example() -> String {
    String::from("")
}

pub fn data() -> String {
    util::get_input("day10.txt")
}

#[cfg(test)]
mod test_result {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(data()), 0);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(data()), 0);
    }
}
#[cfg(test)]
mod test_example {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(_example()), 0);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(_example()), 0);
    }
}

fn next_dir(d: Dir, t: (Dir, Dir)) -> Option<Dir> {
    if t.0 == -d {
        return Some(t.1);
    }
    if t.1 == -d {
        return Some(t.0);
    }
    None
}

fn bends(a: &Array2D<char>, p: Pos) -> (Dir, Dir) {
    match a[p.as_a2d_index()] {
        '|' => (NORTH, SOUTH),
        '-' => (EAST, WEST),
        'L' => (NORTH, EAST),
        'J' => (NORTH, WEST),
        '7' => (SOUTH, WEST),
        'F' => (SOUTH, EAST),
        _ => panic!("p"),
    }
}

fn flips(c: char) -> bool {
    match c {
        '|' => true,
        '-' => false,
        'L' => false,
        'J' => false,
        '7' => true,
        'F' => true,
        _ => panic!("p"),
    }
}

fn find_start(a: &Array2D<char>) -> Pos {
    a.enumerate_row_major()
        .filter_map(|(p, c)| match *c == 'S' {
            true => Some(Pos::from_a2d_index(p)),
            false => None,
        })
        .next()
        .unwrap()
}

fn find_start_dir(a: &Array2D<char>, start_pos: Pos) -> Dir {
    [NORTH, SOUTH, EAST, WEST]
        .iter()
        .filter_map(|d| next_dir(*d, bends(&a, start_pos + *d)))
        .next()
        .unwrap()
}
pub fn part01(data: String) -> usize {
    let a = grid_to_a2d(&data);
    let start_pos = find_start(&a);
    let mut dir = find_start_dir(&a, start_pos);
    let mut pos = start_pos;
    let mut n = 0;
    loop {
        pos = pos + dir;
        n += 1;
        if pos == start_pos {
            break;
        };
        dir = next_dir(dir, bends(&a, pos)).unwrap();
    }
    n / 2
}

pub fn part02(data: String) -> usize {
    let mut a = grid_to_a2d(&data);
    let mut visited = HashSet::with_capacity(10000);
    let start_pos = find_start(&a);
    let mut dir = find_start_dir(&a, start_pos);
    a.set(
        start_pos.0 as usize,
        start_pos.1 as usize,
        match next_dir(SOUTH, bends(&a, start_pos + SOUTH)) {
            Some(_) => '|',
            None => '-',
        },
    )
    .unwrap();
    let mut pos = start_pos;
    loop {
        visited.insert(pos.as_a2d_index());
        pos = pos + dir;
        if pos == start_pos {
            break;
        };
        dir = next_dir(dir, bends(&a, pos)).unwrap();
    }
    let mut n = 0;
    let mut is_inside: bool;
    for (row_num, row) in a.rows_iter().enumerate() {
        is_inside = false;
        for (col_num, c) in row.enumerate() {
            match visited.get(&(row_num, col_num)) {
                Some(_) => {
                    if flips(*c) {
                        is_inside = !is_inside
                    }
                }
                None => {
                    if is_inside {
                        n += 1
                    }
                }
            }
        }
    }
    n
}

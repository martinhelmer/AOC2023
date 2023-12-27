use crate::util::{self, ok_pos, Dir, Pos, EAST, NORTH, SOUTH, WEST};
use array2d::Array2D;
use std::collections::HashSet;

pub const NAME: &str = "Day 16: The Floor Will Be Lava";

pub fn example() -> String {
    String::from(".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")
}

pub fn data() -> String {
    util::get_input("day16.txt")
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
        assert_eq!(super::part01(example()), 0);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(example()), 0);
    }
}

fn new_moves(a: &Array2D<char>, this_pos: Pos, this_dir: Dir) -> Vec<(Pos, Dir)> {
    //println!("{:?} {:?}", this_pos, this_pos.as_a2d_index());
    let dirs = match a[this_pos.as_a2d_index()] {
        '.' => vec![this_dir],
        '|' => {
            if this_dir == NORTH || this_dir == SOUTH {
                vec![this_dir]
            } else {
                vec![NORTH, SOUTH]
            }
        }
        '-' => {
            if this_dir == EAST || this_dir == WEST {
                vec![this_dir]
            } else {
                vec![EAST, WEST]
            }
        }
        '/' => match this_dir {
            EAST => vec![NORTH],
            WEST => vec![SOUTH],
            NORTH => vec![EAST],
            SOUTH => vec![WEST],
            _ => panic!("illegal dir"),
        },
        '\\' => match this_dir {
            EAST => vec![SOUTH],
            WEST => vec![NORTH],
            NORTH => vec![WEST],
            SOUTH => vec![EAST],
            _ => panic!("illegal dir"),
        },
        _ => panic!("illegal char"),
    };
    dirs
        .iter()
        .map(|d| (this_pos + *d, *d))
        .filter(|(p, _)| ok_pos(&a, *p))
        .collect()
}

fn calc_energized(a :& Array2D<char>, start_pos : Pos, start_dir : Dir) -> usize {
    let mut visited = HashSet::new();
    let mut energized = HashSet::new();
    let mut stack = vec![(start_pos, start_dir)];

    while let Some((mut this_pos, this_dir)) = stack.pop() {
        if visited.contains(&(this_pos, this_dir)) {
            continue;
        }
        loop {
            visited.insert((this_pos, this_dir));
            energized.insert(this_pos);
            let nm = new_moves(&a, this_pos, this_dir);

            if a[this_pos.as_a2d_index()] == '.' {
                let np = this_pos + this_dir;
                if ok_pos(&a, np) {
                    this_pos = np;
                    continue
                }
            }
            break
        }
        for npos_dir in new_moves(&a, this_pos, this_dir) {
            if !visited.contains(&npos_dir) {
                stack.push(npos_dir);
            }
        }
    }
    energized.len()
}
pub fn part01(data: String) -> usize {
    let vvc: Vec<Vec<char>> = data.lines().map(|c| c.chars().collect()).collect();
    let a = Array2D::from_rows(&vvc).unwrap();
    calc_energized(&a, Pos(0,0), EAST)
}
// TODO: parallelize
pub fn part02(data: String) -> usize {
    let vvc: Vec<Vec<char>> = data.lines().map(|c| c.chars().collect()).collect();
    let a = Array2D::from_rows(&vvc).unwrap();
    let r1 = (0..a.num_rows()).map(|r|calc_energized(&a, Pos(r as i32,0), EAST));
    let r2 = (0..a.num_rows()).map(|r|calc_energized(&a, Pos(r as i32,a.num_columns() as i32-1), WEST));
    let r3 = (0..a.num_columns()).map(|c|calc_energized(&a, Pos(0, c as i32), SOUTH));
    let r4 = (0..a.num_columns()).map(|c|calc_energized(&a, Pos(a.num_rows() as i32 -1, c as i32), NORTH));
    let r1s : usize = r1.max().unwrap();
    let r2s : usize = r2.max().unwrap();
    let r3s : usize = r3.max().unwrap();
    let r4s : usize = r4.max().unwrap();
    let l = vec![r1s, r2s, r3s, r4s];
    println!("{}", l.iter().max().unwrap());
    0
}

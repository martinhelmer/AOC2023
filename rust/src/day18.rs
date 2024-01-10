use crate::util;
use crate::util::{Dir, Pos, EAST, NORTH, SOUTH, WEST};
use array2d::Array2D;
use itertools::all;
use std::cmp::{max, min};
//use std::collections::HashMap;
use queues::{IsQueue, Queue};
use std::collections::HashSet;
use std::mem::swap;

pub const NAME: &str = "Day 18: Lavaduct Lagoon";

pub fn example() -> String {
    String::from(
        "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)",
    )
}

pub fn data() -> String {
    util::get_input("day18.txt")
}

#[cfg(test)]
mod test_result {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(data()), 49578);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(data()), 52885384955882);
    }
}
#[cfg(test)]
mod test_example {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(example()), 62);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(example()), 952408144115);
    }
}

#[derive(Eq, PartialEq, Debug)]
struct Instr {
    dir: Dir,
    length: usize,
}

fn parse_instr(s: &str) -> Instr {
    let items: Vec<_> = s.split_whitespace().collect();
    let dir = match items[0] {
        "R" => EAST,
        "D" => SOUTH,
        "L" => WEST,
        "U" => NORTH,
        _ => panic!("panic"),
    };
    let length: usize = items[1].parse().unwrap();
    let color = items[2].trim_matches(|c| c == '(' || c == ')').to_string();
    Instr { dir, length }
}

#[cfg(test)]
mod test_parse_instr2 {
    use super::*;
    #[test]
    fn pi() {
        assert_eq!(
            super::parse_instr2("R 6 (#70c710)"),
            Instr {
                dir: EAST,
                length: 461937
            }
        );
    }
}

fn parse_instr2(s: &str) -> Instr {
    let items: Vec<_> = s.split_whitespace().collect();
    let color = items[2].trim_matches(|c| c == '(' || c == ')');
    let length: usize = usize::from_str_radix(&color[1..6], 16).unwrap();
    let dir = match &color[6..7] {
        "0" => EAST,
        "1" => SOUTH,
        "2" => WEST,
        "3" => NORTH,
        _ => panic!("aa"),
    };
    Instr { dir, length }
}

pub fn part01(data: String) -> usize {
    let dig_plan: Vec<Instr> = data.lines().map(parse_instr).collect();
    calculate_size(&dig_plan)
}

//

fn count_aross(row: i32, vertical_lines: &Vec<((Pos, Pos), bool)>) -> usize {
    let mut is_inside = false;
    let mut prevx: i32 = i32::MIN;
    let mut s: usize = 0;
    for (l, flipit) in vertical_lines {
        //println!("l={:?}", l);
        if l.0 .0 <= row && l.1 .0 >= row {
            s += (l.1 .1 - l.0 .1) as usize + 1; // add the line width no matter what
            if *flipit {
                is_inside = !is_inside;
                if !is_inside {
                    s += (l.0 .1 - prevx) as usize;
                }
            } else {
                if is_inside {
                    s += (l.0 .1 - prevx) as usize
                };
            }

            prevx = l.1 .1 + 1;
        }
    }
    s
}
fn calculate_size(dig_plan: &Vec<Instr>) -> usize {
    let mut horisontal_lines = vec![];
    let mut all_lines: Vec<((Pos, Pos), bool)> = vec![];
    let mut p = Pos(0, 0);
    for (ix, i) in dig_plan.iter().enumerate() {
        let end_point = p + (i.dir * i.length as i32);
        if i.dir == WEST || i.dir == EAST {
            // horisontal
            let t = if p.1 <= end_point.1 {
                (p, end_point)
            } else {
                (end_point, p)
            };
            horisontal_lines.push(t);
            let flipit = dig_plan[if ix == 0 { dig_plan.len() - 1 } else { ix - 1 }].dir
                == dig_plan[ix + 1].dir;
            all_lines.push((t, flipit));
        } else {
            // vertical
            let mut sp = p;
            let mut ep = end_point;
            if sp.0 > ep.0 {
                swap(&mut sp, &mut ep);
            }
            sp = sp + SOUTH;
            ep = ep + NORTH;
            if sp.0 <= ep.0 {
                all_lines.push(((sp, ep), true));
            }
        }
        p = end_point;
    }
    horisontal_lines.sort();
    all_lines.sort_by(|a, b| a.0 .0 .1.cmp(&b.0 .0 .1));
    //println!("Part 2 \n {:?}\n {:?}", &horisontal_lines, &all_lines);
    let mut s = 0;
    for (ix, hl) in horisontal_lines.iter().enumerate() {
        if ix < horisontal_lines.len() - 1 {
            let next_hl = horisontal_lines[ix + 1];
            if next_hl.0 .0 == hl.0 .0 {
                continue;
            }
            s += count_aross(hl.0 .0, &all_lines);
            let diff = (next_hl.0 .0 - hl.0 .0) as usize - 1;
            //println!("row = {} diff = {}", hl.0.0+1, diff);
            s += count_aross(hl.0 .0 + 1, &all_lines) * diff;
        } else {
            s += count_aross(hl.0 .0, &all_lines);
        }
    }
    s
}
pub fn part02(data: String) -> usize {
    let dig_plan: Vec<Instr> = data.lines().map(parse_instr2).collect();
    calculate_size(&dig_plan)
}

use crate::util;
use crate::util::{ Dir, Pos, EAST, NORTH, SOUTH, WEST};
use array2d::Array2D;
use std::cmp::{max, min};
//use std::collections::HashMap;
use std::collections::HashSet;
use queues::{IsQueue, Queue};

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

struct Instr {
    dir: Dir,
    length: usize,
    color: String,
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
    Instr { dir, length, color }
}
pub fn part01(data: String) -> usize {
    let dig_plan: Vec<Instr> = data.lines().map(parse_instr).collect();
    let mut p = Pos(0, 0);
    let mut land: HashSet<Pos> = HashSet::from([p]);
    let mut min_col = 0;
    let mut min_row = 0;
    let mut max_col = 0;
    let mut max_row = 0;
    for instruction in dig_plan {
        for _ in 0..instruction.length {
            p = p + instruction.dir;
            min_col = min(min_col, p.1);
            min_row = min(min_row, p.0);
            max_col = max(max_col, p.1);
            max_row = max(max_row, p.0);
            land.insert(p);
        }
    }
    // println!(
    //     "End pos {:?}, ({}), ({},{})-({},{})",
    //     p,
    //     land.len(),
    //     min_row,
    //     min_col,
    //     max_row,
    //     max_col
    // );

    let fill_pos = Pos(1,1);
    let mut q : Queue<Pos> = Queue::new();
    let _ = q.add(fill_pos);
    while  q.size() > 0 {
        p = q.remove().unwrap();
        if !land.contains(&p) {
            land.insert(p);
            for d in [NORTH, EAST, SOUTH, WEST] {
                let np = p + d ;
                let _ = q.add(np);
            }
        }
    }
    let mut a = Array2D::filled_with(
        '.',
        (max_row - min_row+1) as usize,
        (max_col - min_col+1) as usize,
    );
    let offset = Dir(-min_row, -min_col);
    for k in &land {
        let p = *k+offset;
        a.set(p.0 as usize, p.1 as usize, if *k == Pos(0, 0) {'O'} else {' '}).unwrap();
    }
    //println!("{}",util::a2dc_to_grid(&a));
    land.len()
}

pub fn part02(_data: String) -> usize {
    //println!("Part 2");
    0
}

// def fill(data, p, fc):
//     q = queue.SimpleQueue()
//     q.put(p)
//     while not q.empty():
//         p = q.get()
//         if is_valid_pos(p, data) and data[p[0]][p[1]] == '.':
//             data[p[0]][p[1]] = fc
//             for d in [NORTH, EAST, SOUTH, WEST]:
//                 np = move(p, d)
//                 q.put(np)

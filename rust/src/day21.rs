
use crate::util::{self, ok_pos, Dir, Pos, EAST, NORTH, SOUTH, WEST};
use array2d::Array2D;
use std::collections::HashSet;

pub const NAME: &str = "Day 21: Step Counter";

pub fn example() -> String {
    String::from(
"...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........",
    )
}

pub fn data() -> String {
    util::get_input("day21.txt")
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

fn next_steps(a :& Array2D<char> , pos : Pos) -> Vec<Pos> {
    let pp : Vec<Pos>= vec![NORTH, EAST, SOUTH, WEST]
            .iter()
            .map(|d|pos+*d)
            .filter(|p|ok_pos(a,*p))
            .filter(|p| a[p.as_a2d_index()] != '#')
            .collect();
    pp
}

pub fn part01(data: String) -> usize {
    let a = util::grid_to_a2d(&data); 
    let spos = Pos((a.num_rows() / 2) as i32, (a.num_columns() / 2) as i32);
    assert_eq!(a[spos.as_a2d_index()], 'S');
    let mut steps = HashSet::from([spos]);

    for i in 0..64 {
        let mut newsteps = HashSet::new();
        for from_pos in &steps {
            newsteps.extend(next_steps(&a, *from_pos));
        }
        steps = newsteps;
        //println!(" {}  : {}", i , steps.len());
    }
    steps.len()
}

pub fn part02(data: String) -> usize {
    // println!("Part 2");
    0
}

// 154209541800672 too low 
// 158670486613908 too low ((202300+2)* (202300+2)*3877)
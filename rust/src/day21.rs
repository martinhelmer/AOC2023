use crate::util::{self};
use num_bigint::BigUint;
use num_traits::{abs, One, ToPrimitive, Zero};
use std::iter::zip;

pub const NAME: &str = "Day 21: Step Counter";

pub fn _example() -> String {
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
        assert_eq!(super::part01(_example()), 0);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(_example()), 0);
    }
}


fn more_data(s: &String) -> String {
    let mut v = vec![];
    for r in s.lines() {
        let repl = r.to_string().replace("S", ".");
        v.push(format!("{}{}{}", repl, repl, repl));
    }
    let s = v.join("\n");
    format!("{}\n{}\n{}", s, s, s)
}

fn data2ints(data: &String) -> (Vec<BigUint>, Vec<BigUint>) {
    let mut v = vec![];
    let mut rocks = vec![];

    for (rix,row) in data.lines().enumerate() {
        let mut w: BigUint = Zero::zero();
        let mut r: BigUint = Zero::zero();
        for (cix,c) in row.chars().enumerate() {
            w = (w << 1)
                + (if c == 'S' || (rix == 131+64 && cix == 131+64) {
                    BigUint::one()
                } else {
                    BigUint::zero()
                });
            r = (r << 1)
                + if c == '#' {
                    BigUint::zero()
                } else {
                    BigUint::one()
                };
        }
        v.push(w);
        rocks.push(r);
    }
    (v, rocks)
}

fn make_block(steps: usize, n: usize) -> Vec<BigUint> {
    let mhd = |i, j| {
        (abs(j as isize - ((steps as isize - 1) / 2)) 
         + abs(i as isize- (steps as isize - 1) / 2)) as usize
    };
    let matc1or0 = |i, j| match mhd(i, j) <= n {
        true => BigUint::one(),
        false => BigUint::zero(),
    };

    let mut vv: Vec<BigUint> = Vec::with_capacity(steps);
    for i in 0..steps {
        vv.push((0..steps).fold(BigUint::zero(), |a, j| (a << 1) + matc1or0(i, j)));
    }
    vv
}

fn not_block(v: &Vec<BigUint>) -> Vec<BigUint> {
    v.iter()
        .map(|x| (x) ^ ((BigUint::one() << (v.len())) - BigUint::one()))
        .collect()
}

fn and_block(v1: &Vec<BigUint>, v2: &Vec<BigUint>) -> Vec<BigUint> {
    zip(v1, v2).map(|(a, b)| a & b).collect()
}

fn grow(steps: &mut Vec<BigUint>, rocks: &Vec<BigUint>, n: usize) {
    for _i in 0..n {
        let mut new_steps = Vec::with_capacity(steps.len());
        for i in 0..steps.len() {
            if i == 0 {
                new_steps.push((&steps[i + 1] | &steps[i] << 1 | &steps[i] >> 1) & &rocks[i]);
                continue;
            }
            if i == steps.len() - 1 {
                new_steps.push((&steps[i - 1] | &steps[i] << 1 | &steps[i] >> 1) & &rocks[i]);
                continue;
            }
            new_steps.push(
                (&steps[i + 1] | &steps[i - 1] | &steps[i] << 1 | &steps[i] >> 1) & &rocks[i],
            );
        }
        *steps = new_steps;
    }
}
fn count_ones(steps: &Vec<BigUint>) -> usize {
    steps
        .iter()
        .map(|n| n.count_ones())
        .sum::<BigUint>()
        .to_usize()
        .unwrap()
}
fn _print_block(s: &str, steps: &Vec<BigUint>) {
    println!("{s}");
    for s in steps {
        println!(" {s:0131b}");
    }
}
fn _print_block2(s: &str, steps: &Vec<BigUint>) {
    println!("{s}");
    for s in steps {
        println!(" {s:0393b}");
    }
}

pub fn part01(data: String) -> usize {
    let (mut steps, rocks) = data2ints(&data);
    grow(&mut steps, &rocks, 64);
    count_ones(&steps)
}

pub fn part02(data: String) -> usize {
    let (mut steps, rocks) = data2ints(&more_data(&data));
    let zeroblock = make_block(steps.len(), 65);
    let oneblock = and_block(&make_block(steps.len(), 65 + 131), &not_block(&zeroblock));
    grow(&mut steps, &rocks, 198);
    //let inner_even = count_ones(&and_block(&zeroblock, &steps)) as f64;
    let outer_odd = count_ones(&and_block(&oneblock, &steps)) as f64 / 8.0;

    grow(&mut steps, &rocks, 1);
    let inner_odd = count_ones(&and_block(&zeroblock, &steps)) as f64;
    let outer_even = count_ones(&and_block(&oneblock, &steps)) as f64 / 8.0;

    //println!("{inner_even}, {inner_odd} / {outer_even} , {outer_odd}");
    let odds: usize = (2..202301).step_by(2).map(|i| (4 * (1 + 2 * i) - 4)).sum();
    let evens: usize = (1..202300).step_by(2).map(|i| (4 * (1 + 2 * i) - 4)).sum();
    (inner_odd + outer_odd * (odds as f64) + outer_even * (evens as f64)) as usize
}

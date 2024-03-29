use crate::util;
use std::iter::zip;

pub const NAME :&str = "Day 11: Cosmic Expansion";

pub fn _example() -> String {
    String::from(
        "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....",
    )
}

pub fn data() -> String {
    util::get_input("day11.txt")
}

fn calc_shortest(galaxies: Vec<(usize, usize)>) -> usize {
    let sum: isize = combine(galaxies)
        .iter()
        .map(|x| (x.1 .1 as isize - x.0 .1 as isize).abs() + (x.1 .0 as isize - x.0 .0 as isize))
        .sum();
    sum as usize
}

fn combine(v: Vec<(usize, usize)>) -> Vec<((usize, usize), (usize, usize))> {
    let mut combined = vec![];
    for i in 0..v.len() {
        for j in (i + 1)..v.len() {
            combined.push((v[i], v[j]));
        }
    }
    combined
}

// 10292708
// 790194712336

// try 2
fn add_gaps(xs: Vec<usize>, gapsize: usize) -> Vec<usize> {
    let diffs = xs.windows(2).map(|w| {
        let d = w[1] - w[0];
        match d > 1 {
            true => (d - 1) * gapsize + 1,
            false => d,
        }
    });
    let mut x = match xs[0] {
        0 => 0,
        _ => xs[0] * gapsize,
    };
    let mut acc = vec![x];
    for d in diffs {
        x += d ;
        acc.push(x);
    }
    acc
}

fn parse_data2(s: &str, gapsize: usize) -> (Vec<usize>, Vec<usize>) {
    let mut xs = vec![];
    let mut ys = vec![];
    for (rix, row) in s.lines().enumerate() {
        for (cix, _) in row.chars().enumerate().filter(|(_, c)| c == &'#') {
            xs.push(cix);
            ys.push(rix);
        }
    }
    xs.sort();
    (add_gaps(xs, gapsize), add_gaps(ys, gapsize))
}

pub fn part01(data: String) -> usize {
    let (xs, ys): (Vec<usize>, Vec<usize>) = parse_data2(&data, 2); // original coordinates
    // let back = zip(xs, ys).collect();
    calc_shortest(zip(xs, ys).collect())
}

pub fn part02(data: String) -> usize {
    let (xs, ys): (Vec<usize>, Vec<usize>) = parse_data2(&data, 1000000); // original coordinates
    let back = zip(xs, ys).collect();
    calc_shortest(back)
}

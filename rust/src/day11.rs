use crate::util;
use array2d::Array2D;
use std::collections::HashSet;
use std::iter::zip;

pub const NAME :&str = "Day 11: Cosmic Expansion";

pub fn example() -> String {
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

//  try 1

fn parse_input(s: &str, galaxy_distance: usize) -> Vec<(usize, usize)> {
    let grid = util::grid_to_a2d(&s);
    let empty_cols = find_cols_with_nothing(&grid);
    let mut galaxies: Vec<(usize, usize)> = vec![];
    let mut rowcount = 0;
    // println!("{:?}",empty_cols);
    for row in grid.rows_iter() {
        let mut row_has_galaxy = false;
        let mut colcount = 0;
        for (colix, c) in row.enumerate() {
            if c == &'#' {
                galaxies.push((rowcount, colcount));
                row_has_galaxy = true;
            }
            colcount += match empty_cols.contains(&colix) {
                true => galaxy_distance,
                false => 1,
            }
        }
        rowcount += match row_has_galaxy {
            false => galaxy_distance,
            true => 1,
        }
    }
    galaxies
}

fn find_cols_with_nothing(grid: &Array2D<char>) -> HashSet<usize> {
    let mut double_cols = HashSet::new();
    for (colix, mut col) in grid.columns_iter().enumerate() {
        if col.all(|x| x == &'.') {
            double_cols.insert(colix);
        }
    }
    double_cols
}

// 10292708
pub fn part01b(data: String) -> usize {
    let shortest = calc_shortest(parse_input(&data, 2));
    println!("Part1 : {:?}", shortest);
    shortest
}

// 790194712336
pub fn part02b(data: String) -> usize {
    let shortest = calc_shortest(parse_input(&data, 1000000));
    println!("Part2 : {:?}", shortest);
    shortest
}

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
    calc_shortest(zip(xs, ys).collect())
}

pub fn part02(data: String) -> usize {
    let (xs, ys): (Vec<usize>, Vec<usize>) = parse_data2(&data, 1000000); // original coordinates
    let back = zip(xs, ys).collect();
    calc_shortest(back)
}

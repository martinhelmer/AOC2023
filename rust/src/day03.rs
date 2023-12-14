use crate::util;
use array2d::Array2D;
use std::collections::BTreeMap;

pub const NAME :&str = "Day 3: Gear Ratios";

pub fn data() -> String {
    let contents: String = util::get_input("day03.txt");
    contents
}

pub fn example() -> String {
    String::from(
        "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..",
    )
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct Symbol {
    s: char,
    p: (isize, isize),
}
#[derive(Clone, Debug)]
struct PartNumber {
    number: usize,
    symbols: Vec<Symbol>,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum States {
    OffDigit,
    OnDigit,
}

fn symbol_at_point(a: &Array2D<char>, point: (isize, isize)) -> Option<char> {
    if point.0 < 0
        || point.1 < 0
        || point.0 >= a.num_rows() as isize
        || point.1 >= a.num_columns() as isize
    {
        return None;
    }
    let s = a[(point.0 as usize, point.1 as usize)];
    if s == '.' || s.is_digit(10) {
        return None;
    }
    return Some(s);
}

fn addsymbs(
    a: &Array2D<char>,
    symbs: &mut Vec<Symbol>,
    pos: (isize, isize),
    dirs: Vec<(isize, isize)>,
) {
    for p in dirs.iter().map(|d| (pos.0 + d.0, pos.1 + d.1)) {
        match symbol_at_point(a, p) {
            Some(s) => symbs.push(Symbol { s, p }),
            None => continue,
        }
    }
}

fn partnumbers(a: &Array2D<char>) -> Vec<PartNumber> {
    let mut state: States;
    let mut pn: Vec<PartNumber> = vec![];
    let mut number: usize = 0;
    let mut currsymbs: Vec<Symbol> = vec![];
    for row in 0..(a.num_rows()) {
        state = States::OffDigit;
        for col in 0..(a.num_columns()) {
            let c: char = a[(row, col)];
            match (state, c.is_digit(10)) {
                (States::OffDigit, false) => continue,
                (States::OffDigit, true) => {
                    // start of number
                    state = States::OnDigit;
                    number = (c as u8 - '0' as u8) as usize;
                    currsymbs = vec![];
                    addsymbs(
                        a,
                        &mut currsymbs,
                        (row as isize, col as isize),
                        vec![(-1, -1), (-1, 0), (0, -1), (1, -1), (1, 0)],
                    );
                }
                (States::OnDigit, false) => {
                    // end number
                    state = States::OffDigit;
                    addsymbs(
                        a,
                        &mut currsymbs,
                        (row as isize, col as isize),
                        vec![(-1, 0), (0, 0), (1, 0)],
                    );
                    if currsymbs.len() > 0 {
                        pn.push(PartNumber {
                            number,
                            symbols: currsymbs.clone(),
                        })
                    }
                }
                (States::OnDigit, true) => {
                    // continue number
                    number = number * 10 + (c as u8 - '0' as u8) as usize;
                    addsymbs(
                        a,
                        &mut currsymbs,
                        (row as isize, col as isize),
                        vec![(-1, 0), (1, 0)],
                    );
                }
            }
        }
        // we ended the row on a number
        if state == States::OnDigit && currsymbs.len() > 0 {
            pn.push(PartNumber {
                number,
                symbols: currsymbs.clone(),
            })
        }
    }
    pn
}

pub fn part01(data: String) -> usize{
    let l: Vec<Vec<char>> = data.lines().map(|q| q.chars().collect()).collect();
    let array = Array2D::from_rows(&l).unwrap();
    let pn = partnumbers(&array);
    let s: usize = pn.iter().map(|p| p.number).sum();

    println!("part1 {:?} ({}", s, pn.len());
    let mut symhash = BTreeMap::new();
    for part in pn {
        for symbol in part.symbols {
            if symbol.s == '*' {
                symhash
                    .entry(symbol)
                    .and_modify(|v: &mut Vec<usize>| v.push(part.number))
                    .or_insert(vec![part.number]);
            }
        }
    }
    let gear_ratios = symhash
        .values()
        .filter(|v| v.len() == 2)
        .map(|v| v.iter().product::<usize>());
    let gearsum: usize = gear_ratios.sum();
    println!("{}", gearsum);
    gearsum
}

pub fn part02(data: String) -> usize { 0 }
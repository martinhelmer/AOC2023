use itertools::Itertools;

use crate::util;
pub const NAME: &str = "Day 1: Trebuchet?! ";

pub fn data() -> String {
    util::get_input("day01.txt")
}

const W2: [&str; 9] = [
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
];

fn gd1(s: &str) -> Option<usize> {
    return match s[0..1].parse::<usize>() {
        Ok(n) => Some(n),
        _ => None,
    };
}

fn gd2(s: &str) -> Option<usize> {
    if let Some(n) = gd1(s) {
        return Some(n);
    }
    (1..10).filter(|i|s.starts_with(W2[i-1])).next()
}
fn slices(s: &str) -> impl Iterator<Item = &str> {
    (0..s.len()).map(|i| &s[i..])
}

fn firslastprod<'a, I>(mut q: I) -> usize
where
    I: Iterator<Item = usize>,
{
    let f = q.next().unwrap();
    match q.last() {
        Some(v) => f * 10 + v,
        None => f + f * 10,
    }
}
// 55386
pub fn part01(contents: String) -> usize {
    let qq: usize = contents
        .lines()
        .map(|s| firslastprod(slices(s).map(gd1).flatten()))
        .sum();
    qq
}

/*
54824
*/
pub fn part02(contents: String) -> usize {
    let qq: usize = contents
        .lines()
        .map(|s| firslastprod(slices(s).map(gd2).flatten()))
        .sum();
    qq
}
#[cfg(test)]
mod test_result {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(data()), 55386);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(data()), 54824);
    }
}

// other ppls solutions
mod others {
    #![allow(unused)]

    use crate::util;

    pub fn part01b() {
        let contents: String = util::get_input("day01.txt");
        println!("{}", parse_input(contents.as_str(), false))
    }

    pub fn part02b() {
        let contents: String = util::get_input("day01.txt");
        println!("{}", parse_input(contents.as_str(), true))
    }

    pub fn part01c() {
        let contents: String = util::get_input("day01.txt");
        println!("{}", part_one(contents.as_str()))
    }

    pub fn part02c() {
        let contents: String = util::get_input("day01.txt");
        println!("{}", part_two(contents.as_str()))
    }

    fn parse_input(input: &str, replace: bool) -> u32 {
        input
            .lines()
            .filter(|line| !line.is_empty())
            .map(|line| {
                if replace {
                    line.to_string()
                        .replace("one", "one1one")
                        .replace("two", "two2two")
                        .replace("three", "three3three")
                        .replace("four", "four4four")
                        .replace("five", "five5five")
                        .replace("six", "six6six")
                        .replace("seven", "seven7seven")
                        .replace("eight", "eight8eight")
                        .replace("nine", "nine9nine")
                } else {
                    line.to_string()
                }
            })
            .map(|line| {
                line.chars()
                    .filter_map(|c| c.to_digit(10))
                    .collect::<Vec<u32>>()
            })
            .map(|vec| 10 * vec.first().unwrap() + vec.last().unwrap())
            .sum()
    }

    //
    const LUT: [&str; 9] = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];

    pub fn part_one(input: &str) -> u32 {
        input.lines().map(parse_line_1).sum()
    }

    pub fn part_two(input: &str) -> u32 {
        input.lines().map(parse_line_2).sum()
    }

    fn parse_line_1(line: &str) -> u32 {
        let first = line.chars().find_map(|c| c.to_digit(10));
        let last = line.chars().rev().find_map(|c| c.to_digit(10));
        10 * first.unwrap() + last.unwrap()
    }

    fn parse_line_2(line: &str) -> u32 {
        let first = find_pattern(0..line.len(), line);
        let last = find_pattern((0..line.len()).rev(), line);
        10 * first + last
    }

    fn find_pattern(mut it: impl Iterator<Item = usize>, line: &str) -> u32 {
        it.find_map(|i| compare_slice(&line[i..])).unwrap()
    }

    fn compare_slice(slice: &str) -> Option<u32> {
        LUT.iter()
            .enumerate()
            .find(|(_, pattern)| slice.starts_with(*pattern))
            .map(|(i, _)| i as u32 + 1)
            .or_else(|| slice.chars().next().unwrap().to_digit(10))
    }
}

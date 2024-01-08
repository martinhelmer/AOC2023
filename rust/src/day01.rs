use crate::util;
pub const NAME :&str = "Day 1: Trebuchet?! ";

pub fn data() -> String {
    util::get_input("day01.txt")
}

const W: [&[u8]; 10] = [
    b"zero", b"one", b"two", b"three", b"four", b"five", b"six", b"seven", b"eight", b"nine",
];

fn find_digit(s: &str, checkforwords: bool) -> usize {
    let b = s.as_bytes();
    _find_digit(b, 0..b.len(), checkforwords)
}

fn find_digit_reverse(s: &str, checkforwords: bool) -> usize {
    let b = s.as_bytes();
    _find_digit(b, (0..b.len()).rev(), checkforwords)
}

fn _find_digit<I>(b: &[u8], range: I, checkforwords: bool) -> usize
where
    I: Iterator<Item = usize>,
{
    for i in range {
        if b[i].is_ascii_digit() {
            return (b[i] - b'0') as usize;
        }
        if !checkforwords {
            continue;
        }
        for j in 0..W.len() {
            if b[i..].starts_with(W[j]) {
                return j as usize;
            }
        }
    }
    0
}

// 55386
pub fn part01(contents: String) -> usize {
    let qq: usize = contents
        .lines()
        .map(|s: &str| find_digit(s, false) * 10 + find_digit_reverse(s, false))
        .sum();
    qq
}

/*
54824
*/
pub fn part02(contents: String) -> usize {
    let qq: usize = contents
        .lines()
        .map(|s: &str| find_digit(s, true) * 10 + find_digit_reverse(s, true))
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
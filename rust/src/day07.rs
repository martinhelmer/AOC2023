use std::{
    cmp::Ordering,
    collections::HashMap,
    usize,
};

use itertools::Itertools;

use crate::util;

pub const NAME: &str = "Day 7: Camel Cards";

pub fn data() -> String {
    util::get_input("day07.txt")
}

pub fn _example() -> String {
    String::from(
        "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483",
    )
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
        assert_eq!(super::part01(_example()), 6440);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(_example()), 5905);
    }
}


#[cfg(test)]
mod test_counted {
    #[test]
    fn counted() {
        assert_eq!(super::counted("AABBC", false), vec![2, 2, 1]);
        assert_eq!(super::counted("ABCDJ", false), vec![1, 1, 1, 1, 1]);
        assert_eq!(super::counted("ABCDJ", true), vec![2, 1, 1, 1]);
        assert_eq!(super::counted("BAAAA", false), vec![4, 1]);
        assert_eq!(super::counted("AABBB", false), vec![3, 2]);
        assert_eq!(super::counted("AJBBB", true), vec![4, 1]);
    }
    #[test]
    fn sorted() {
        let mut v = vec![vec![2, 2], vec![2, 1, 1]];
        v.sort();
        assert_eq!(v, vec![vec![2, 1, 1], vec![2, 2]]);
    }
}

fn counted(s: &str, jacks_are_wild: bool) -> Vec<usize> {
    let mut hm = HashMap::new();
    for c in s.chars() {
        hm.entry(c).and_modify(|e| *e += 1).or_insert(1);
    }
    let mut jackadd = 0;
    //println!("{:?}", hm);
    if jacks_are_wild {
        if let Some(vv) = hm.remove(&'J') {
            jackadd = vv
        }
    }
    let mut v: Vec<usize> = hm.into_values().sorted().rev().collect();
    if v.len() == 0 {
        return vec![5];
    } // special case 5 jacks
    v[0] += jackadd;
    v
}


#[derive(PartialEq, Eq, Ord, Debug)]
struct Card<'a> {
    rankish: Vec<usize>,
    stringord: usize,
    s: &'a str,
    bid: usize,
}

impl PartialOrd for Card<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if let Some(c) = self.rankish.partial_cmp(&other.rankish) {
            if c == Ordering::Equal {
                return self.stringord.partial_cmp(&other.stringord);
            } else {
                return Some(c);
            }
        }
        panic!("panic!");
    }
}

fn parse_line<'a>(s: &'a str, cardord: &'a str, jacks_are_wild: bool) -> Card<'a> {
    let (cards, bids) = s.split_once(' ').unwrap();
    Card {
        rankish: counted(cards, jacks_are_wild),
        stringord: cards
            .chars()
            .fold(0, |acc, ss| acc * 13 + cardord.find(ss).unwrap()),
        s,
        bid: bids.parse().unwrap(),
    }
}


pub fn part01(data: String) -> usize {
    data.lines()
        .map(|s| parse_line(s, "23456789TJQKA", false))
        .sorted()
        .enumerate()
        .map(|(i, c)| (i + 1) * c.bid)
        .sum()
}

pub fn part02(data: String) -> usize {
    data.lines()
        .map(|s| parse_line(s, "J23456789TQKA", true))
        .sorted()
        .enumerate()
        .map(|(i, c)| (i + 1) * c.bid)
        .sum()
}
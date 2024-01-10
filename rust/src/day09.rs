use itertools::Itertools;

use crate::util;

pub const NAME: &str = "Day 9: Mirage Maintenance";

pub fn data() -> String {
    util::get_input("day09.txt")
}

pub fn _example() -> String {
    String::from(
        "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45",
    )
}

#[cfg(test)]
mod test_diffs {
    #[test]
    fn diffs() {
        assert_eq!(super::diffs(&vec![1, 3, 8]), vec![2, 5]);
    }
}

fn diffs(v: &Vec<isize>) -> Vec<isize> {
    v[0..].windows(2).map(|w| w[1] - w[0]).collect_vec()
}

fn predict_next(v: Vec<isize>) -> isize {
    if v.iter().all(|f| *f == 0) {
        return 0;
    }
    v.last().unwrap() + predict_next(diffs(&v))
}

#[cfg(test)]
mod test_result {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(data()), 1898776583);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(data()), 1100);
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

pub fn part01(data: String) -> usize {
    data
        .lines()
        .map(|s| {
            predict_next(
                s.split_whitespace()
                    .map(|c| c.parse().unwrap())
                    .collect_vec(),
            )
        })
        .sum::<isize>() as usize
}

pub fn part02(data: String) -> usize {
    data
        .lines()
        .map(|s| {
            predict_next(
                s.split_whitespace()
                    .map(|c| c.parse().unwrap())
                    .rev()
                    .collect_vec(),
            )
        })
        .sum::<isize>() as usize
}

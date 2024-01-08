use std::collections::HashMap;

use num::integer::lcm;

use crate::util;

pub const NAME: &str = "Day 8: Haunted Wasteland";

pub fn _example() -> String {
    String::from(
        "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)",
    )
}

pub fn data() -> String {
    util::get_input("day08.txt")
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
// AAA = (BBB, BBB)
fn parse_node(s: &str) -> (&str, (&str, &str)) {
    (&s[0..3], (&s[7..10], &s[12..15]))
}

fn count_it(hm: &HashMap<&str, (&str, &str)>, i: &str, p: &str) -> usize {
    let mut pos = p;
    let mut n = 0;
    while !pos.ends_with("Z") {
        let (l, r) = hm.get(pos).unwrap();
        match i.as_bytes()[n % i.len()] {
            b'L' => pos = l,
            b'R' => pos = r,
            _ => panic!("??"),
        }
        n += 1;
    }
    n
}
pub fn part01(data: String) -> usize {
    let network: Vec<&str> = data.lines().collect();
    let instr = network[0];
    let hm: HashMap<&str, (&str, &str)> =
        HashMap::from_iter(network[2..].iter().map(|q| parse_node(q)));
    count_it(&hm, instr, "AAA")
}

pub fn part02(data: String) -> usize {
    let network: Vec<&str> = data.lines().collect();
    let instr = network[0];
    let hm: HashMap<&str, (&str, &str)> =
        HashMap::from_iter(network[2..].iter().map(|q| parse_node(q)));
    hm.keys()
        .filter(|k| k.ends_with("A"))
        .map(|x| count_it(&hm, instr, x))
        .fold(1, |acc, x| lcm(acc, x))
}

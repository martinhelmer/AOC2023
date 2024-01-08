
use crate::util;

pub const NAME: &str = "Day 7: Camel Cards";

pub fn _example() -> String {
    String::from(
"",
    )
}

pub fn data() -> String {
    util::get_input("day07.txt")
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

pub fn part01(_data: String) -> usize {
    // println!("Part 1");
    0
}

pub fn part02(_data: String) -> usize {
    // println!("Part 2");
    0
}

use crate::util;

pub const NAME: &str = "Day 9: Mirage Maintenance";

pub fn example() -> String {
    String::from(
        "",
    )
}

// some_slice.windows(2).map(|w| w[1] - w[0])

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
        assert_eq!(super::part01(example()), 0);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(example()), 0);
    }
}

pub fn part01(data: String) -> usize {
    //println!("Part 1");
    0
}

pub fn part02(data: String) -> usize {
    //println!("Part 2");
    0
}
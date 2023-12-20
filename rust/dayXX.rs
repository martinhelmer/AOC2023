
use crate::util;

pub const NAME: &str = "";

pub fn example() -> String {
    String::from(
        "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533",
    )
}

pub fn data() -> String {
    util::get_input("")
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
    println!("Part 1");
    0
}

pub fn part02(data: String) -> usize {
    println!("Part 2");
    0
}
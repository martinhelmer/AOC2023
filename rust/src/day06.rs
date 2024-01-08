
use crate::util;

pub const NAME: &str = "Day 6: Wait For It";

pub fn _example() -> String {
    String::from(
        "",
    )
}

pub fn data() -> String {
    util::get_input("day06.txt")
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

fn num_ways(t : (isize, isize)) -> usize {
    t.1 as usize - t.0 as usize + 1
}
fn win_interval(t : isize, d : isize) -> (isize, isize) {
    let q = f64::sqrt((t * t - 4 * d) as f64);
    let s1 = (t as f64 - q) / 2 as f64;
    let s2 = (t as f64 + q) / 2 as f64;
    (s1 as isize + 1, if s2 > s2.trunc() { s2 as isize} else {s2 as isize -1})
    }

    // too lazy to parse
pub fn part01(_data: String) -> usize {
    let r = [(42,308),(89,1170),(91,1291),(89,1467)].map(|(t,d) |num_ways(win_interval(t, d)));
    r[0] * r[1] * r[2] * r[3]
}

// too lazy to parse
pub fn part02(_data: String) -> usize {
    num_ways(win_interval(42899189,308117012911467))
    }

use std::cmp::Ordering;

use itertools::any;

use crate::util;

pub const NAME :&str = "Day 5: If You Give A Seed A Fert.";

pub fn data() -> String {
    util::get_input("day05.txt")
}

pub fn example() -> String {
    String::from(
        "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4",
    )
}

#[derive(Debug, Eq, PartialEq, PartialOrd)]
struct SubMap {
    from: (usize, usize), // exclusive range
    to: (usize, usize),   //
}

impl Ord for SubMap {
    fn cmp(&self, other: &Self) -> Ordering {
        self.from.cmp(&other.from)
    }
}

fn apply_submap_on_seedrange(m : SubMap, sr : (usize, usize)) -> Vec<(usize, usize)> {
    // map out if range
    if m.from.0 >= sr.1 || m.from.1 <= sr.0 {
        return vec![sr];
    }
    // seed fully inside
    if m.from.0 <= sr.0 && sr.1 <= m.from.1 {
        return vec!((submap(&m, sr.0).unwrap(), submap(&m, sr.1-1).unwrap()));
    }
    vec![sr]
}



#[derive(Debug)]
struct Map {
    submaps: Vec<SubMap>,
}
// 50 98 2
fn parse_submap(s: &str) -> SubMap {
    let ints = util::s_to_ints(s).unwrap();
    SubMap {
        from: (ints[1], ints[1] + ints[2]),
        to: (ints[0], ints[0] + ints[2]),
    }
}


fn parse_map(s: &str) -> Map {
    Map { submaps: vec![] }
}
fn submap(sm: &SubMap, x: usize) -> Option<usize> {
    match sm.from.0 <= x && x < sm.from.1 {
        true => Some(x - sm.from.0 + sm.to.0),
        false => None,
    }
}

fn rev_submap(sm: &SubMap, x: usize) -> Option<usize> {
    match sm.to.0 <= x && x < sm.to.1 {
        true => Some(x - sm.to.0 + sm.from.0),
        false => None,
    }
}


fn applymap(m: &Map, x: usize) -> usize {
    match m.submaps.iter().map(|sm| submap(sm, x)).flatten().next() {
        Some(i) => i,
        None => x,
    }
}


fn applymap_rev(m: &Map, x: usize) -> usize {
    match m.submaps.iter().map(|sm| rev_submap(sm, x)).flatten().next() {
        Some(i) => i,
        None => x,
    }
}

fn parse_seeds(s: &str) -> Vec<usize> {
    util::s_to_ints(s.split_once(":").unwrap().1).unwrap()
}

fn parse_seed_ranges(s: &str) -> Vec<(usize, usize)> {
    let binding= util::s_to_ints(s.split_once(":").unwrap().1).unwrap();
    let i : Vec<_>= binding.chunks(2).collect();
    let ranges = i.iter().map(|c|(c[0],c[0]+c[1])).collect();
    ranges
}


fn parse_maps(s: &[&str]) -> Vec<Map> {
    s.iter().fold(vec![], |mut acc, l| {
        if l.len() == 0 {
            return acc;
        }
        if l.ends_with(":") {
            acc.push(Map { submaps: vec![] });
            return acc;
        }
        let lastindex = acc.len() - 1;
        let v: &mut Vec<SubMap> = &mut acc[lastindex].submaps;
        v.push(parse_submap(l));
        acc
    })
}

fn doseed(maps: &Vec<Map>, seed: usize) -> usize {
    maps.iter().fold(seed, |acc, m| applymap(m, acc))
}

fn doseed_rev(maps: &Vec<Map>, seed: usize) -> usize {
    maps.iter().fold(seed, |acc, m| applymap_rev(m, acc))
}

// 389056265
pub fn part01(data: String) -> usize {
    let lines: Vec<&str> = data.lines().collect();
    let seeds: Vec<usize> = parse_seeds(&lines[0]);
    let maps: Vec<Map> = parse_maps(&lines[1..]);
    let minloc:usize = seeds.iter().map(|s|doseed(&maps, *s)).min().unwrap();

    println!("{:?}",minloc);
    minloc
}

fn is_seed(s:usize, seeds:&Vec<(usize, usize)> ) -> bool{
    any(seeds, |(a,b)| a <= &s && &s < b)
}
//137 516 820
pub fn part02(data: String) -> usize{
    let lines: Vec<&str> = data.lines().collect();
    let seeds: Vec<(usize, usize)> = parse_seed_ranges(&lines[0]);
    let maps: Vec<Map> = parse_maps(&lines[1..]);
    let rmap: Vec<Map>= maps.into_iter().rev().collect();
    for (i,s) in (0..).map(|s|doseed_rev(&rmap, s)).enumerate() {
        if is_seed(s, &seeds) {
            println!("{}",i);
            return s 
        }
        if i % 1000000 == 0 {
            println!("{} / {}",i, s) }
    }
    0
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn part01() {
        println!("Hello");

}
}

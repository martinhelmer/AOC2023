#![allow(unused)]

use crate::util;
use itertools::{any, Itertools};
use std::cmp::Ordering;
use std::sync::{Arc, Mutex};
use std::thread;

pub const NAME: &str = "Day 5: (MT) If You Give A Seed A Fert.";

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

#[derive(Debug, Eq, PartialEq, PartialOrd, Clone, Copy)]
struct SubMap {
    from: (usize, usize), // exclusive range
    to: (usize, usize),   //
}

impl Ord for SubMap {
    fn cmp(&self, other: &Self) -> Ordering {
        self.from.cmp(&other.from)
    }
}
// 1. *--* 
// 2. *--*  |--| 
// 3. *--|--*--|
// 4. *--|--|--*
// 5. |--*--*--|
// 6. |--*--|--*
// 7. |--| *--*
mod test_range_on_map {
    use super::*;
    #[test]
    fn a1() {
        let d = example();
        let lines: Vec<&str> = d.lines().collect();
        let seeds: Vec<usize> = parse_seeds(&lines[0]);
        let maps: Vec<Map> = parse_maps(&lines[1..]);
        let m = &maps[0];
        println!("{:?}\n{:?}",m, range_on_map((0,100), m));
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(data()), 137516820);
    }
}


fn range_on_map((rfrom, rto): (usize, usize), m : &Map) -> Vec<(usize, usize)> {
    //println!("called with ({},{}) =>  {:?}", rfrom, rto, m.submaps);
    if m.submaps.len() == 0  {return vec![(rfrom, rto)]};   // 1. 
    let first_map_from = m.submaps[0].from.0;
    let first_map_to= m.submaps[0].from.1;
    if (rto < first_map_from)  {return vec![(rfrom, rto)]}; // 2. 
    if rfrom >= first_map_to { return range_on_map((rfrom, rto), &Map{submaps : m.submaps[1..].to_vec()})} // 7.
    if rfrom < first_map_from { let mut v = vec![(rfrom,first_map_from-1 )];   //  3. && 4. 
                                v.extend(range_on_map((first_map_from, rto), m));
                                return v}
    if rfrom < first_map_to && rto >= first_map_to { // 6. 
        let mut v = range_on_map((rfrom, first_map_to-1), m);
        v.extend(range_on_map((first_map_to, rto), &Map{submaps : m.submaps[1..].to_vec()}));
        return v;
    }
    // 5. 
    //println!("5.. calling submap({:?}, {}, {}",&m.submaps[0], rfrom, rto );
    vec![(submap(&m.submaps[0], rfrom).unwrap(), submap(&m.submaps[0], rto).unwrap())]
}

#[derive(Debug, Clone)]
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
    match m.submaps.iter().filter_map(|sm| submap(sm, x)).next() {
        Some(i) => i,
        None => x,
    }
}

fn applymap_rev(m: &Map, x: usize) -> Result<usize, usize> {
    match m.submaps.iter().filter_map(|sm| rev_submap(sm, x)).next() {
        Some(i) => Ok(i),
        None => Err(0),
    }
}

fn parse_seeds(s: &str) -> Vec<usize> {
    util::s_to_ints(s.split_once(":").unwrap().1).unwrap()
}

fn parse_seed_ranges(s: &str) -> Vec<(usize, usize)> {
    let binding = util::s_to_ints(s.split_once(":").unwrap().1).unwrap();
    let i: Vec<_> = binding.chunks(2).collect();
    let ranges = i.iter().map(|c| (c[0], c[0] + c[1])).sorted().collect();
    ranges
}

fn parse_maps(s: &[&str]) -> Vec<Map> {
    let mut vm = 
    s.iter().fold(vec![], |mut acc : Vec<Map>, l| {
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
    });
    for mut m in &mut vm { m.submaps.sort()};
    vm
}

fn doseed(maps: &Vec<Map>, seed: usize) -> usize {
    maps.iter().fold(seed, |acc, m| applymap(m, acc))
}

fn doseed_rev(maps: &Vec<Map>, seed: usize) -> Result<usize, usize> {
    maps.iter().try_fold(seed, |acc, m| applymap_rev(m, acc))
}

// 389056265
pub fn part01(data: String) -> usize {
    let lines: Vec<&str> = data.lines().collect();
    let seeds: Vec<usize> = parse_seeds(&lines[0]);
    let maps: Vec<Map> = parse_maps(&lines[1..]);
    let minloc: usize = seeds.iter().map(|s| doseed(&maps, *s)).min().unwrap();

    // println!("{:?}", minloc);
    minloc
}

// fn is_seed(s: usize, seeds: &Vec<(usize, usize)>) -> bool {
//     any(seeds, |(a, b)| a <= &s && &s < b)
// }
fn is_seed(s: usize, seeds: &Vec<(usize, usize)>) -> bool {
    for (a, b) in seeds {
        if s < *a {
            return false;
        }
        if *a <= s && s < *b {
            return true;
        }
    }
    false
}

fn do_some(
    rmap: & Vec<Map>,
    seeds: & Vec<(usize, usize)>,
    status: Arc<Mutex<Option<usize>>>,
    step: usize,
    offset: usize,
) -> usize {
    // println!("({}) starting thread", offset);
    for (j, s) in (0..)
        .map(|s| doseed_rev(&rmap, s * step + offset))
        .enumerate()
    {
        let i = j * step + offset;
        match s {
            Ok(s) => {
                if is_seed(s, &seeds) {
                    // println!("({}) Thread found {}", offset, i);
                    let mut st = status.lock().unwrap();
                    match *st {
                        None => {
                            // println!("({}) I'm first ({})", offset, i);
                            *st = Some(i);
                        }
                        Some(v) => {
                            if i < v {
                                *st = Some(i);
                                // println!("({}) My value is better. {} < {} ", offset, i, v);
                            } else {
                                // println!("({}) But someone else found a better value ({})", offset, v);
                            }
                        }
                    }
                    return i;
                }
            }
            Err(s) => {
                continue;
            }
        }
    }
    0
}

//137 516 820
pub fn part02(data: String) -> usize {
    let lines: Vec<&str> = data.lines().collect();
    let seeds: Vec<(usize, usize)> = parse_seed_ranges(&lines[0]);
    //println!("{:?}", seeds);
    let maps: Vec<Map> = parse_maps(&lines[1..]);
    let rmap: Vec<Map> = maps.into_iter().rev().collect();

    let status: Arc<Mutex<Option<usize>>> = Arc::new(Mutex::new(None));
    let mut handles = vec![];

    let num_threads = 10;
    for i in 0..num_threads {
        let status = Arc::clone(&status);
        let rmap = rmap.clone();
        let seeds = seeds.clone();
        let handle = thread::spawn(move || {
            do_some(&rmap, &seeds, status, num_threads, i)
        });
        handles.push(handle);
    }

    for handle in handles {
       handle.join().unwrap();
    }

    let r = *status.lock().unwrap();
    //println!("Result: {:?}",r );
    r.unwrap()
}

#[cfg(test)]
mod test_result {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(data()), 389056265);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(data()), 137516820);
    }
}

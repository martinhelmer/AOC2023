use itertools::Itertools;

use crate::util::{self, djikstra, shortestpath_from_dj};
use std::collections::{HashMap, HashSet};
// use std::thread;
// use std::time::Duration;
use rustc_hash::FxHashMap as FastMap;

pub const NAME: &str = "Day 25: Snowoverload";

pub fn _example() -> String {
    String::from(
        "jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr",
    )
}

pub fn data() -> String {
    util::get_input("day25.txt")
}

#[cfg(test)]
mod test_result {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(data()), 0);
    }
}
#[cfg(test)]
mod test_example {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(_example()), 0);
    }
}

type Graph<'a> = FastMap<&'a str, Vec<(&'a str, usize)>>;

fn get_graph<'a>(data: &'a String) -> Graph<'a> {
    // cmg: qnr nvd lhk bvb
    fn parse_row(s: &str) -> (&str, Vec<&str>) {
        let (k, vs) = s.split_once(": ").unwrap();
        let v = vs.split_ascii_whitespace().collect();
        (k, v)
    }

    let mut g: Graph = Graph::default();
    g = data.lines().fold(g, |mut acc, r| {
        let (k, v) = parse_row(r);
        let vv: Vec<_> = v.iter().map(|x| (*x, 1)).collect();
        acc.entry(k)
            .and_modify(|v| v.extend(vv.clone()))
            .or_insert(vv);
        for i in v {
            acc.entry(i)
                .and_modify(|f| f.push((k, 1)))
                .or_insert(vec![(k, 1)]);
        }
        acc
    });
    g
}

fn subgraph_size(g : & Graph, start_node : &str , exclude_nodes : HashSet<&str>) -> usize {
    let mut stack = vec![start_node];
    let mut visited = HashSet::new();
    while let Some(n) = stack.pop() {
        visited.insert(n);
        for neigh in &g[n] {
            if visited.contains(neigh.0) || (exclude_nodes.contains(n) && exclude_nodes.contains(neigh.0)) { continue;}
            stack.push(neigh.0);
        }

    }
    visited.len()
}

pub fn part01(data: String) -> usize {
    let g = get_graph(&data);
    let gl = g.len();
    //println!("graph size = {}", subgraph_size(&g, g.keys().next().unwrap(), HashSet::new()));
    let mut sp_count = HashMap::with_capacity(g.len());
    let mut n = 0;
    for k in g.keys() {
        n+=1 ;
        let (_d, p) = djikstra(&g, k, false);
        for k2 in g.keys() {
            if k == k2  { continue;}
            let visited = shortestpath_from_dj(&p, k, k2).unwrap();
            for v in &visited[1..(visited.len() - 1)] {
                *sp_count.entry(*v).or_insert(0) += 1;
            }
        }
        let mut favs : HashSet<&str>= HashSet::with_capacity(g.len());
        for (i,f) in sp_count.iter().sorted_by(|a, b| Ord::cmp(&b.1, &a.1)).enumerate() {
            favs.insert(f.0);
            if i >= 5 { break}
        }
        if n < 5 {continue;}
        let sub =  subgraph_size(&g, g.keys().next().unwrap(), favs);
        if sub < gl {
            // println!("n = {}",n);
            return sub * (gl-sub)
        }
    }
    0
}
// [("qdp", 162083), ("jxx", 161767), ("qqq", 152532), ("mlp", 151151), ("zbr", 143809), ("vsx", 143133), ("bht", 54021)]

pub fn part02(_data: String) -> usize {
    0
}

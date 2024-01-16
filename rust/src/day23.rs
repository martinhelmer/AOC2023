#![warn(dead_code)]
use itertools::Itertools;
use priority_queue::PriorityQueue;
use std::{
    cmp::max,
    collections::HashSet,
    // {BTreeSet, HashMap as MM, HashSet},
};

use rustc_hash::FxHashMap as MM;

use crate::util::{self, djikstra, rl, rr, Dir, Grid, Pos, EAST, NORTH, SOUTH, WEST};

pub const NAME: &str = "Day 23: A Long Walk";

pub fn _example() -> String {
    String::from(
        "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#",
    )
}

pub fn data() -> String {
    util::get_input("day23.txt")
}

#[cfg(test)]
mod test_result {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(data()), 2178);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(data()), 6486);
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
// find the next node in this direction.
// step in (assuming it's valid)
// find next direction
// if multiple directions are feasible, we've found the next node
fn checkdir(a: &Grid, d: Dir, p: Pos) -> Option<Dir> {
    if match a[(p + d).as_a2d_index()] {
        '.' => true,
        '#' => false,
        '>' => d == EAST,
        'v' => d == SOUTH,
        '<' => d == WEST,
        '^' => d == NORTH,
        _ => panic!("unexpected cell content"),
    } {
        Some(d)
    } else {
        None
    }
}

fn _checkdir2(a: &Grid, d: Dir, p: Pos) -> Option<Dir> {
    if (p + d).0 < 0 || (p + d).0 > (a.num_rows() as i32 - 1) {
        return None;
    };
    match a[(p + d).as_a2d_index()] {
        '.' | '>' | '<' | '^' | 'v' => Some(d),
        '#' => None,
        _ => panic!("unexpected cell content"),
    } 
}

struct NextNodeResult {
    pos: Pos,    // position
    dist: usize, // distance to get there
    branches: Vec<Dir>,
}
fn next_node(
    a: &Grid,
    p: Pos,
    mut d: Dir,
    cf: fn(a: &Grid, d: Dir, p: Pos) -> Option<Dir>,
) -> NextNodeResult {
    let mut this_pos = p + d;
    let mut stepcount = 1;
    loop {
        if this_pos == Pos(a.num_rows() as i32 - 1, a.num_columns() as i32 - 2) {
            return NextNodeResult {
                pos: this_pos,
                dist: stepcount,
                branches: vec![],
            };
        }
        let ok_dirs: Vec<Dir> = [rl(d), rr(d), d]
            .iter()
            .map(|d| cf(a, *d, this_pos))
            .flatten()
            .collect();
        match ok_dirs.len() {
            0 => {
                return NextNodeResult {
                    pos: this_pos,
                    dist: stepcount,
                    branches: vec![],
                }
            }
            1 => {
                d = ok_dirs[0];
                this_pos = this_pos + d;
                stepcount += 1;
                continue;
            }
            2 | 3 => {
                return NextNodeResult {
                    pos: this_pos,
                    dist: stepcount,
                    branches: ok_dirs,
                }
            }
            _ => panic!("Too many matches!! ({})", ok_dirs.len()),
        }
    }
}

type Graph = MM<Pos, (u128, Vec<(Pos, usize)>)>;

type Graph2 = MM<u128, Vec<(Node, usize)>>;

// ***************
fn part0102(graph: Graph) -> usize {
    let mut visited: HashSet<Pos> = HashSet::new();
    let mut pq = PriorityQueue::new();
    let mut this_node: Pos;
    let mut this_cost: isize;
    pq.push(Pos(0, 1), 0);
    let (tnp, thp) = pq.peek().unwrap();
    this_node = *tnp;
    this_cost = -*thp;

    loop {
        let (_, next_nodes) = graph.get(&this_node).unwrap();
        if next_nodes.len() == 0 {
            return (-this_cost) as usize;
        }
        //println!("accessing {:?} / {} : {:?}", this_node, this_cost, next_nodes);
        for (newnode, _cost) in next_nodes
            .into_iter()
            .sorted_by(|a, b| Ord::cmp(&b.1, &a.1))
        {
            let cost = -(*_cost as isize);
            if visited.contains(newnode) {
                continue;
            }
            let new_cost = this_cost + cost as isize;
            match pq.get_priority(newnode) {
                None => {
                    pq.push(*newnode, new_cost);
                }
                Some(p) => {
                    if new_cost < *p {
                        pq.change_priority(newnode, new_cost);
                    }
                }
            }
        }
        visited.insert(this_node);
        (this_node, this_cost) = pq.pop().unwrap();
    }
}

pub fn part01(data: String) -> usize {
    let a = util::grid_to_a2d(&data);
    let end_pos = Pos(a.num_rows() as i32 - 1, a.num_columns() as i32 - 2);
    let mut stack: Vec<(Pos, Dir)> = vec![(Pos(0, 1), SOUTH)];
    let mut graph: Graph = MM::default();
    let mut visited: HashSet<(Pos, Dir)> = HashSet::new();

    while let Some((pos, dir)) = stack.pop() {
        if visited.contains(&(pos, dir)) {
            continue;
        }
        let NextNodeResult {
            pos: next_pos,
            dist,
            branches,
        } = next_node(&a, pos, dir, checkdir);
        graph
            .entry(pos)
            .and_modify(|(_, nodes)| nodes.push((next_pos, dist)))
            .or_insert((0, vec![(next_pos, dist)]));
        for b in branches {
            stack.push((next_pos, b))
        }
        visited.insert((pos, dir));
    }
    graph.insert(end_pos, (0, vec![]));
    //print!("graph = {:?}, ({})", graph, graph.len());
    part0102(graph)
}

fn getgraph(a: &Grid, end_pos: &Pos) -> (Graph2, Node) {
    let mut stack: Vec<Pos> = vec![Pos(0, 1)];
    let mut graph1: MM<Node, Vec<(Pos, usize)>> = MM::default();
    let mut visited: HashSet<Pos> = HashSet::new();
    let mut idmap = MM::default();
    let mut count = 0;
    while let Some(pos) = stack.pop() {
        if visited.contains(&pos) { continue;}
        let ns: Vec<_> = [WEST, SOUTH, EAST, NORTH]
            .iter()
            .map(|d| _checkdir2(a, *d, pos))
            .flatten()
            .map(|d| next_node(a, pos, d, _checkdir2))
            .map(
                |NextNodeResult {
                     pos,
                     dist,
                     branches: _,
                 }| (pos, dist),
            )
            .collect();
        visited.insert(pos);
        for n in &ns[0..] {
            if !visited.contains(&n.0) {
                stack.push(n.0);
            }
        }
        count += 1;
        graph1.insert(count, ns);
        assert_eq!(idmap.insert(pos, count), None, "when trying to insert {pos:?} into {count} ");
    }
    // println!("graph1: {graph1:?}\n\n");
    // println!("idmap: {idmap:?}\n\n");
    let end_node = idmap[end_pos];
    let mut g2: Graph2 = Graph2::from_iter(
        graph1
            .iter()
            .map(|(k, v)| (*k, (*v).iter().map(|(p, w)| (idmap[p], *w)).collect())),
    );
    let (steps_from_end, _) = djikstra(&g2, end_node, true);
    assert!(g2[&end_node].len() == 1);
    let (pre_end_node, _) = g2[&end_node][0];
    g2.get_mut(&pre_end_node)
        .unwrap()
        .retain(|f| f.0 == end_node);
    // println!("{pre_end_node} : {:?}", g2[&pre_end_node]);
    // println!("{steps_from_end:?}  \n\n{g2:?}");
    let mut g3 = Graph2::default();
    for (k, v) in &g2 {
        // println!("endnode = {end_node} k={k}");
        let mut nv = v.clone();
        if nv.len() == 3 {
            nv.retain(|f| !(g2[&(f.0)].len() == 3 && steps_from_end[&f.0] > steps_from_end[k]));
        }
        g3.insert(*k, nv);
    }
    (g3, end_node)
}

pub fn part02(data: String) -> usize {
    let a = util::grid_to_a2d(&data);
    let end_pos = Pos(a.num_rows() as i32 - 1, a.num_columns() as i32 - 2);
    let (graph, end_node) = getgraph(&a, &end_pos);
    longest_path(graph, 1, end_node) as usize
}

type Node = u128;
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
struct State {
    pos: Pos,
    visited: u128,
}
struct State2 {
    node: Node,
    visited: u128,
}

fn longest_path(g: Graph2, start_vertex: Node, end_vertex: Node) -> isize {
    let mut stack: Vec<(State2, isize)> = vec![(
        State2 {
            node: start_vertex,
            visited: 0,
        },
        0,
    )];
    let mut ms = 0;
    while let Some((st, this_distance)) = stack.pop() {
        let this_node = st.node;
        let mut this_visited = st.visited;

        if this_node == end_vertex {
            ms = max(ms, this_distance);
        }
        let next_nodes = g.get(&this_node).unwrap();
        this_visited = this_visited | 1 << this_node;

        for (next_node, next_weight) in next_nodes {
            if this_visited & (1 << next_node) > 0 {
                continue;
            }

            stack.push((
                State2 {
                    node: *next_node,
                    visited: this_visited,
                },
                this_distance + *next_weight as isize,
            ))
        }
    }
    ms
}

use std::{collections::{HashMap as MM, HashSet, BTreeSet}, cmp::max};
use itertools::Itertools;
use priority_queue::PriorityQueue;

use crate::util::{self, rl, rr, Dir, Grid, Pos, EAST, NORTH, SOUTH, WEST};

pub const NAME: &str = "Day 23: A Long Walk";

pub fn example() -> String {
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
        assert_eq!(super::part01(example()), 0);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(example()), 0);
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

fn checkdir2(a: &Grid, d: Dir, p: Pos) -> Option<Dir> {
    if (p + d).0 < 0 || (p + d).0 > (a.num_rows() as i32 - 1) {
        return None;
    };
    if match a[(p + d).as_a2d_index()] {
        '.' | '>' | '<' | '^' | 'v' => true,
        '#' => false,
        _ => panic!("unexpected cell content"),
    } {
        Some(d)
    } else {
        None
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

type Graph = MM<Pos, Vec<(Pos, usize)>>;

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
        let next_nodes: &Vec<(util::Pos, usize)> = graph.get(&this_node).unwrap();
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
    let mut graph: Graph = MM::new();
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
            .and_modify(|nodes| nodes.push((next_pos, dist)))
            .or_insert(vec![(next_pos, dist)]);
        for b in branches {
            stack.push((next_pos, b))
        }
        visited.insert((pos, dir));
    }
    graph.insert(end_pos, vec![]);
    print!("graph = {:?}, ({})", graph, graph.len());
    part0102(graph)
}


fn getgraph(a: &Grid) -> Graph {
    let mut stack: Vec<(Pos)> = vec![Pos(0, 1)];
    let mut graph: Graph = MM::new();
    let mut visited: HashSet<Pos> = HashSet::new();

    while let Some(pos) = stack.pop() {
        let ns : Vec<_> =    [NORTH, SOUTH, EAST, WEST]
        .iter()
        .map(|d| checkdir2(a, *d, pos))
        .flatten()
        .map(|d| next_node(a, pos, d, checkdir2)).map(
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
        graph.insert(pos, ns);
    }
    graph
}
pub fn part02(data: String) -> usize {
    println!("Part 2...");
    let a = util::grid_to_a2d(&data);
    let end_pos = Pos(a.num_rows() as i32 - 1, a.num_columns() as i32 - 2);
    let graph = getgraph(&a);
    let visited : HashSet<Pos> = HashSet::new();

    println!("{:?}", graph);
    println!("{:?}", longest_path(graph, Pos(0,1), end_pos));
    0
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
struct State { pos : Pos, visited : BTreeSet<Pos>}

fn longest_path(g:Graph, start_vertex : Pos, end_vertex : Pos) -> isize {
    // let mut max_distance : MM<State, isize> = MM::from([(State{pos : start_vertex, visited : BTreeSet::new()},0)]);
    let mut stack : Vec<(State,isize)> = vec![(State{pos : start_vertex, visited : BTreeSet::new()},0)];
    let mut ms = 0;
    while let Some((st, this_distance))  = stack.pop() {
        let this_pos = st.pos;
        let mut this_visited = st.visited;
        let this_state = State{pos :  this_pos, visited: this_visited.clone()};
        if this_pos == end_vertex { 
            ms = max(ms, this_distance);
            if ms == this_distance {
             println!("end  : {}", ms) }
        }
        // if let Some(d) =  max_distance.get(&this_state)
        //     {if *d > this_distance {continue}};
        // max_distance.insert(this_state, this_distance);

        for next_node in g.get(&this_pos).unwrap() {
            if this_visited.contains(&next_node.0) { continue}
            this_visited.insert(this_pos);
            stack.push((State{pos: next_node.0,visited:this_visited.clone() }, this_distance + next_node.1 as isize))
        }
    }
    println!("max_distance : {:?}",ms);
    0
    }

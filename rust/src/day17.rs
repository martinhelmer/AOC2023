use crate::util;
use array2d::Array2D;
use priority_queue::PriorityQueue;
use std::collections::{BTreeMap as MM};

pub const NAME: &str = "Day 17: Clumsy Crucible";

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

const EAST: Dir = Dir(0, 1);
const _WEST: Dir = Dir(0, -1);
const _NORTH: Dir = Dir(-1, 0);
const SOUTH: Dir = Dir(1, 0);

fn rl(dir: Dir) -> Dir {
    Dir(-dir.1, dir.0)
}

fn rr(dir: Dir) -> Dir {
    Dir(dir.1, -dir.0)
}
fn movep(a: &Pos, b: &Dir) -> Pos {
    Pos(a.0 + b.0, a.1 + b.1)
}

fn t_from_pos(a: Pos) -> (usize, usize) {
    (a.0 as usize, a.1 as usize)
}

pub fn data() -> String {
    util::get_input("day17b.txt")
}

#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone, Ord, PartialOrd)]
struct Pos(i32, i32);
#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone, Ord, PartialOrd)]
struct Dir(i32, i32);


#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone, Ord, PartialOrd)]
struct Node {
    pos: Pos,
    dir: Dir,
    steps: usize,
}
fn pos_in_range<T>(m: &Array2D<T>, p: Pos) -> bool {
   !( p.0 < 0 || p.1 < 0 || p.0 >= m.num_rows() as i32 || p.1 >= m.num_columns() as i32)
}

fn nextnodes2(heatmap: &Array2D<u32>, this_node: Node) -> Vec<(Node, u32)> {
    if this_node.steps < 4 {
        let mut s = this_node.steps;
        let mut p = this_node.pos;
        let mut heat = 0;
        while s < 4 {
            p = movep(&p, &this_node.dir);
            if !pos_in_range(heatmap, p) {
                return vec![];
            }
            s += 1;
            heat += heatmap[t_from_pos(p)]
        }
        vec![(
            Node {
                pos: p,
                dir: this_node.dir,
                steps: s,
            },
            heat,
        )]
    } else {
        nextnodes(heatmap, this_node, 10)
    }
}

fn nextnodes1(heatmap: &Array2D<u32>, this_node: Node) -> Vec<(Node, u32)> {
    nextnodes(heatmap, this_node, 3)
}

fn nextnodes(heatmap: &Array2D<u32>, this_node: Node, max_straight: usize) -> Vec<(Node, u32)> {
    [
        Some((rr(this_node.dir), 1)),
        if this_node.steps < max_straight {
            Some((this_node.dir, this_node.steps + 1))
        } else {
            None
        },
        Some((rl(this_node.dir), 1)),
    ]
    .iter()
    .flatten()
    .map(|(d, s)| {
        let np = movep(&this_node.pos, &d);
        match pos_in_range(&heatmap, np) {
            true => Some((
                Node {
                    pos: np,
                    dir: *d,
                    steps: *s,
                },
                heatmap[t_from_pos(np)],
            )),
            false => None,
        }
    })
    .flatten()
    .collect()
}

#[cfg(test)]
mod test_result {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(data()), 674);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(data()), 773);
    }
}

pub fn part01(data: String) -> usize {
    part0102(data, nextnodes1)
}

pub fn part02(data: String) -> usize {
    part0102(data, nextnodes2)
}

fn part0102(data: String, nnf : fn(&Array2D<u32>,Node) -> Vec<(Node, u32)>) -> usize {
    let a = util::num_to_a2d(&data);
    let mut visited: MM<Node, isize> = MM::new();
    let mut pq = PriorityQueue::new();
    let mut this_node : Node;
    let mut this_heat : isize;
    pq.push(
        Node {
            pos: Pos(0, 0),
            dir: EAST,
            steps: 0,
        },
        0,
    );
    pq.push(
        Node {
            pos: Pos(0, 0),
            dir: SOUTH,
            steps: 0,
        },
        0,
    );
    let (tnp, thp) = pq.peek().unwrap();
    this_node = *tnp;
    this_heat = *thp;
    loop {
        // if !visited.contains_key(&this_node) {
        for (newnode, heat) in nnf(&a, this_node).iter() {
            if visited.contains_key(&newnode) {
                continue;
            }
            let new_heat = this_heat - *heat as isize;
            // if let Some(p) = pq.push(*newnode, new_heat) {
            //     if p > new_heat {
            //         pq.push(*newnode, p);
            //     }
            // }

            match pq.get_priority(newnode) {
                None => {
                    pq.push(*newnode, new_heat);
                }
                Some(p) => {
                    if new_heat > *p {
                        pq.change_priority(newnode, new_heat);
                    }
                }
            }
        // }
        visited.insert(this_node, this_heat as isize);
    }
        (this_node, this_heat) = pq.pop().unwrap();
        if this_node.pos == Pos((a.num_rows() - 1) as i32, (a.num_columns() - 1) as i32) {
            return -this_heat as usize
        }
    }
}


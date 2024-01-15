use crate::util::{self, BoolHash, Dir, Node2, Pos, EAST, SOUTH};
use array2d::Array2D;
use priority_queue::PriorityQueue;

pub const NAME: &str = "Day 17: (FM) Clumsy Crucible";

pub fn _example() -> String {
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

fn rl(dir: Dir) -> Dir {
    Dir(-dir.1, dir.0)
}

fn rr(dir: Dir) -> Dir {
    Dir(dir.1, -dir.0)
}

pub fn data() -> String {
    util::get_input("day17.txt")
}

#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone, Ord, PartialOrd)]
struct Node {
    pos: Pos,
    dir: Dir,
    steps: usize,
}
// #[derive(Debug, Eq, Hash, PartialEq, Copy, Clone, Ord, PartialOrd)]
// struct Node2 {
//     pos: Pos,
//     dir: Dir,
// }

fn pos_in_range<T>(m: &Array2D<T>, p: Pos) -> bool {
    !(p.0 < 0 || p.1 < 0 || p.0 >= m.num_rows() as i32 || p.1 >= m.num_columns() as i32)
}

fn nextnodes2b(heatmap: &Array2D<u32>, n: Node2, end_node: &Pos) -> Vec<(Node2, u32)> {
    let mut this_pos = n.pos;
    let mut accu_heat = 0;
    for _ in 0..3 {
        this_pos = this_pos + n.dir;
        if !pos_in_range(&heatmap, this_pos) {
            return vec![];
        }
        accu_heat += heatmap[this_pos.as_a2d_index()];
    }
    nextnodesb(
        &heatmap,
        Node2 {
            pos: this_pos,
            dir: n.dir,
        },
        7,
        accu_heat,
        end_node,
    )
}
fn nextnodes1b(heatmap: &Array2D<u32>, n: Node2, end_node: &Pos) -> Vec<(Node2, u32)> {
    nextnodesb(&heatmap, n, 3, 0, end_node)
}
fn nextnodesb(
    heatmap: &Array2D<u32>,
    n: Node2,
    max_straight: usize,
    start_heat: u32,
    end_node: &Pos,
) -> Vec<(Node2, u32)> {
    let mut this_pos = n.pos;
    let mut result = Vec::with_capacity(max_straight * 2);
    let mut accu_heat = start_heat;
    for _ in 0..max_straight {
        this_pos = this_pos + n.dir;
        if !pos_in_range(&heatmap, this_pos) {
            return result;
        }
        accu_heat += heatmap[this_pos.as_a2d_index()];
        if this_pos == *end_node {
            result.push((
                Node2 {
                    pos: this_pos,
                    dir: n.dir,
                },
                accu_heat,
            ));
            return result;
        };
        let rld = rl(n.dir);
        result.push((
            Node2 {
                pos: this_pos,
                dir: rld,
            },
            accu_heat,
        ));
        let rrd = rr(n.dir);
        result.push((
            Node2 {
                pos: this_pos,
                dir: rrd,
            },
            accu_heat,
        ));
    }
    result
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
    part0102b(data, nextnodes1b)
}

pub fn part02(data: String) -> usize {
    part0102b(data, nextnodes2b)
}

fn part0102b(data: String, nnf: fn(&Array2D<u32>, Node2, &Pos) -> Vec<(Node2, u32)>) -> usize {
    let a = util::num_to_a2d(&data);
    let ifx = |n: &_| BoolHash::<Node2>::ifx(a.num_columns(), n);
    let mut bh = BoolHash::<Node2>::new(&ifx, &BoolHash::<Node2>::maxelem(a.num_columns()));
    let end_node = Pos((a.num_rows() - 1) as i32, (a.num_columns() - 1) as i32);
    let mut pq: PriorityQueue<Node2, isize> = PriorityQueue::with_capacity(1000);
    let mut this_node: Node2;
    let mut this_heat: isize;
    let sn1 = Node2 {
        pos: Pos(0, 0),
        dir: EAST,
    };
    let sn2 = Node2 {
        pos: Pos(0, 0),
        dir: SOUTH,
    };
    pq.push(sn1, 0);
    pq.push(sn2, 0);
    let (tnp, thp) = pq.peek().unwrap();
    this_node = *tnp;
    this_heat = *thp;
    loop {
        for (newnode, heat) in nnf(&a, this_node, &end_node).iter() {
            if bh.contains(&newnode) {
                continue;
            }
            let new_heat = this_heat - *heat as isize;
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
        }
        bh.insert(&this_node);
        (this_node, this_heat) = pq.pop().unwrap();
        if this_node.pos == end_node {
            return -this_heat as usize;
        }
    }
}

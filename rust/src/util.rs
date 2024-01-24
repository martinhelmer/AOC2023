#![allow(unused)]
use array2d::Array2D;
use num_traits::abs;
use priority_queue::PriorityQueue;
use queues::{IsQueue, Queue};
use std::hash::Hash;
use std::ops;
use std::{
    cmp::Reverse,
    collections::{vec_deque, HashMap},
    fmt::Debug,
    fmt::Display,
    fs,
    num::ParseIntError,
};
use rustc_hash::FxHashMap as FastMap;

#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone, Ord, PartialOrd)]
pub struct Pos(pub i32, pub i32);
#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone, Ord, PartialOrd)]
pub struct Dir(pub i32, pub i32);

impl ops::Add<Dir> for Pos {
    type Output = Pos;

    fn add(self, _rhs: Dir) -> Pos {
        Pos(self.0 + _rhs.0, self.1 + _rhs.1)
    }
}
impl ops::Sub<Dir> for Pos {
    type Output = Pos;

    fn sub(self, _rhs: Dir) -> Pos {
        Pos(self.0 - _rhs.0, self.1 - _rhs.1)
    }
}

impl Pos {
    pub fn as_a2d_index(&self) -> (usize, usize) {
        (self.0 as usize, self.1 as usize)
    }
    pub fn from_a2d_index(ix : (usize, usize)) -> Pos {
        Pos(ix.0 as i32, ix.1 as i32)
    }
    pub fn mdist(&self, p : &Pos) -> i32 {
        (if p.0 > self.0 {p.0 - self.0} else {self.0 - p.0})
        +
        if p.1 > self.1 {p.1 - self.1} else {self.1 - p.1}

    }
}

impl ops::Neg for Dir {
    type Output = Dir;

    fn neg(self) -> Dir {
        Dir(-self.0, -self.1)
    }
}

impl ops::Mul<i32> for Dir {
    type Output = Dir;

    fn mul(self, rhs : i32) -> Dir {
        Dir(self.0 * rhs, self.1 * rhs)
    }
}


pub const EAST: Dir = Dir(0, 1);
pub const WEST: Dir = Dir(0, -1);
pub const NORTH: Dir = Dir(-1, 0);
pub const SOUTH: Dir = Dir(1, 0);

pub fn rl(dir: Dir) -> Dir {
    Dir(-dir.1, dir.0)
}

pub fn rr(dir: Dir) -> Dir {
    Dir(dir.1, -dir.0)
}

pub fn get_input(f: &str) -> String {
    let contents = fs::read_to_string(["../input/", f].join(""))
        .expect("Should have been able to read the file");
    contents
}

pub fn s_to_ints(s: &str) -> Result<Vec<usize>, ParseIntError> {
    s.split_whitespace().map(|x| x.parse::<usize>()).collect()
}

fn string_as_int(s: &String) -> u32 {
    s.as_bytes()
        .iter()
        .fold(0, |acc, c| acc * 2 + if c == &b'#' { 1 } else { 0 })
}

// array ---------------------------------
pub type Grid = Array2D<char>;

pub fn ok_pos<T>(a: &Array2D<T>, p: Pos) -> bool {
    p.0 >= 0 && p.1 >= 0 && p.0 < (a.num_rows() as i32) && p.1 < a.num_columns() as i32
}

pub fn a2dc_to_grid(a: &Array2D<char>) -> String {
    let x: Vec<String> = a.rows_iter().map(|row| row.collect::<String>()).collect();
    x.join("\n")
}

pub fn grid_to_a2d(s: &str) -> Array2D<char> {
    let l: Vec<Vec<char>> = s.lines().map(|q| q.chars().collect()).collect();
    Array2D::from_rows(&l).unwrap()
}

pub fn num_to_a2d(s: &str) -> Array2D<u32> {
    let l: Vec<Vec<u32>> = s
        .lines()
        .map(|q| q.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect();
    Array2D::from_rows(&l).unwrap()
}

pub fn flipy(a: Array2D<char>) -> Array2D<char> {
    let mut a2: Vec<Vec<char>> = vec![];
    for mut r in a.as_rows() {
        r.reverse();
        a2.push(r);
    }
    Array2D::from_rows(&a2).unwrap()
}

pub fn fliptldr(a: Array2D<char>) -> Array2D<char> {
    Array2D::from_rows(&a.as_columns()).unwrap()
}

pub fn rotleft(a: Array2D<char>) -> Array2D<char> {
    fliptldr(flipy(a))
}

pub fn rotright(a: Array2D<char>) -> Array2D<char> {
    flipy(fliptldr(a))
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use rustc_hash::FxHashMap as FastMap;

    use crate::util::{self, djikstra, shortestpath_from_dj};

    #[test]
    fn flipy() {
        let y: String = String::from("123\n456");
        assert_eq!(
            util::a2dc_to_grid(&util::flipy(util::grid_to_a2d(&y))),
            String::from("321\n654")
        );
    }
    #[test]
    fn fliptldr() {
        let x: String = String::from("123\n456");
        assert_eq!(
            util::a2dc_to_grid(&util::fliptldr(util::grid_to_a2d(&x))),
            String::from("14\n25\n36")
        );
    }
    #[test]
    fn rotleft() {
        let x: String = String::from("123\n456");
        assert_eq!(
            util::a2dc_to_grid(&util::rotleft(util::grid_to_a2d(&x))),
            String::from("36\n25\n14")
        );
    }

    #[test]
    fn rotright() {
        let x: String = String::from("123\n456");
        assert_eq!(
            util::a2dc_to_grid(&util::rotright(util::grid_to_a2d(&x))),
            String::from("41\n52\n63")
        );
    }

    // #[test]
    // fn test_djikstra() {
    //     let g : FastMap<char, Vec<(char, usize)>> = FastMap::from_iter([
    //         ('A', vec![('B', 3), ('C', 1)]),
    //         ('C', vec![('B', 1), ('D', 9)]),
    //         ('B', vec![('D', 1)]),
    //         ('D', vec![]),
    //     ].iter());
    //     // ({'D': 3, 'C': 1, 'A': 0, 'B': 2}, {'B': 'C', 'D': 'B', 'C': 'A'})
    //     assert_eq!(djikstra(&g,'A').0.get(&'D'), Some(&3));
    // }    
    // #[test]
    // fn test_shortestpath() {
    //     let sp = FastMap::from([('B', 'C'), ('D', 'B'), ('C', 'A')]);
    //     assert_eq!(shortestpath_from_dj(&sp, 'A', 'D'), Some(vec!['A','C','B','D']));
    // }
}

// djikstra

pub fn djikstra<K>(g:& FastMap<K, Vec<(K, usize)>>, source: K, ignore_weights : bool) -> (FastMap<K, usize>, FastMap<K, K>)
where
    K: Hash + Eq + Copy,
{
    let mut dist: FastMap<K, usize> = FastMap::default();
    dist.insert(source, 0);
    let mut prev: FastMap<K, K> = FastMap::default();
    let mut q: PriorityQueue<K, isize> = PriorityQueue::new();
    q.push(source, 0);
    while let Some((u, _distance)) = q.pop() {
        let distance = -_distance as usize;
        for (v, mut edge_weight) in g.get(&u).unwrap() {
            if ignore_weights { edge_weight = 1;}
            let alt: usize = distance + edge_weight;
            match dist.get(v) {
                None => {
                    q.push(*v, -(alt as isize));
                }
                Some(d) => {
                    if *d <= alt {
                        continue;
                    } else {
                        q.change_priority(v, -(alt as isize));
                    }
                }
            }
            dist.insert(*v, alt);
            prev.insert(*v, u);
        }
    }
    (dist, prev)
}

pub fn shortestpath_from_dj<K>(prev : & FastMap<K, K>, from : K, to : K ) -> Option<Vec<K>> 
where
    K: Eq + Hash + Copy,
{
    let mut v : Vec<K>= vec![to];
    let mut k = to;
    while let Some(kk) = prev.get(&k) {
        v.push(*kk);
        if *kk == from {  v.reverse(); return Some(v)}
        k = *kk;
    }
   None
}

// MyHash
pub struct MyHash<'a, T, U> {
    pub v: Vec<Option<U>>,
    ixf: &'a dyn Fn(&T) -> usize,
}
// BoolHash   
pub type BoolHash<'a, T> = MyHash<'a, T, bool>;
// pub struct BoolHash<'a, T> {
//     pub v: Vec<bool>,
//     ixf: &'a dyn Fn(&T) -> usize,
// }

impl<'a, T, U : Copy > MyHash<'a, T, U> {
    pub fn new(ixf: &'a dyn Fn(&T) -> usize, maxelem: &T) -> MyHash<'a, T, U> {
        MyHash {
            v: vec![None; ixf(maxelem)+1],
            ixf,
        }
    }
    pub fn set(&mut self, n: &T, v :U) {
        let ix = ((*self).ixf)(n);
        self.v[ix] = Some(v);
    }
    pub fn get(&self, n: &T) -> Option<U> {
        let ix = ((*self).ixf)(n);
        self.v[ix]
    }
    pub fn contains(&self, n: &T) -> bool { match self.get(n) { Some(_) => true, None => false} }
}
impl<'a, T> BoolHash<'a, T> {
    pub fn insert(&mut self, n: &T) { self.set(n, true); }

}


#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone, Ord, PartialOrd)]
pub struct Node2 {
    pub pos: Pos,
    pub dir: Dir,
}

impl<'a, U>  MyHash<'_, Node2, U> {
    pub fn ifx(nc : usize, n : &Node2) -> usize {
        ((*n).pos.0 as usize * nc
            + n.pos.1 as usize) * 4
            + match n.dir {
                NORTH => 0,
                EAST => 1,
                SOUTH => 2,
                WEST => 3,
                _ => panic!(""),
            }
    }
    pub fn maxelem(nc : usize) -> Node2 {
        Node2 {  pos: Pos(nc as i32 -1, nc as i32 -1), dir : WEST}
    }
    
}

impl BoolHash<'_, (Pos, Dir)> {

    pub fn ifx(nc : usize, n : &(Pos,Dir)) -> usize {
        ((*n).0.0 as usize * nc
            + n.0.1 as usize) * 4
            + match n.1 {
                NORTH => 0,
                EAST => 1,
                SOUTH => 2,
                WEST => 3,
                _ => panic!(""),
            }
    }
    pub fn maxelem(nc : usize) -> (Pos, Dir) {
        (Pos(nc as i32 -1, nc as i32 -1),  WEST)
    }
}

impl BoolHash<'_, Pos> {
    pub fn ifx(nc : usize, n : &Pos) -> usize {
        ((*n).0 as usize * nc
            + n.1 as usize)
    }
}


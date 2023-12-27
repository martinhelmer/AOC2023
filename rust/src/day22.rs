use itertools::Itertools;
use std::{cmp::{max, min}, collections::HashSet};

use crate::util;
use std::collections::HashMap as MM;

pub const NAME: &str = "Day 22: Sand Slabs";

pub fn example() -> String {
    String::from(
        "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
",
    )
}

pub fn data() -> String {
    util::get_input("day22.txt")
}

#[cfg(test)]
mod test_result {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(data()), 509);
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
        assert_eq!(super::part01(example()), 5);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(example()), 0);
    }
}

#[cfg(test)]
mod test_slab {
    use super::*;
    const SLABz: Brick = Brick {
        id: 0,
        p1: (1, 1, 20),
        p2: (1, 1, 21),
        has_moved : false,
    };

    #[test]
    fn fall() {
        let mut slab = SLABz.clone();
        slab.fall(&Space::from([((1, 1, 1),SLABz)]));
        assert_eq!(
            slab,
             Brick {
                id: 0,
                p1: (1, 1, 2),
                p2: (1, 1, 3),
                has_moved : true,
            }
        );
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(example()), 0);
    }
}


type Space = MM<(i32, i32, i32), Brick>;

#[derive(Debug, Eq, Hash, PartialEq, Ord, PartialOrd, Copy, Clone)]
struct Brick {
    id: usize,
    p1: (i32, i32, i32),
    p2: (i32, i32, i32),
    has_moved : bool,
}

//0,2,3~2,2,3
fn parse_brick(p: (usize, &str)) -> Brick {
    fn parse_into_triple(s: &str) -> (i32, i32, i32) {
        let mut x = s.split(',').map(|c| c.parse::<i32>().unwrap());
        (x.next().unwrap(), x.next().unwrap(), x.next().unwrap())
    }

    let (_from, _to) = p.1.split_once('~').unwrap();
    let p1 = parse_into_triple(_from);
    let p2 = parse_into_triple(_to);
    assert!(p1.2 <= p2.2); // check first point is the lowest
    Brick { id: p.0, p1, p2, has_moved:false }
}

impl Brick {
    pub fn downward(&self) -> Brick {
        Brick {
            id: self.id,
            p1: (self.p1.0, self.p1.1, self.p1.2 - 1),
            p2: (self.p2.0, self.p2.1, self.p2.2 - 1),
            has_moved : true,
        }
    }
    pub fn areabelow(&self) -> Vec<(i32, i32, i32)> {
        self._coordinates(
            (self.p1.0, self.p1.1, self.p1.2 - 1),
            (self.p2.0, self.p2.1, self.p1.2 - 1),
        )
    }
    pub fn areaabove(&self) -> Vec<(i32, i32, i32)> {
        self._coordinates(
            (self.p1.0, self.p1.1, self.p2.2 + 1),
            (self.p2.0, self.p2.1, self.p2.2 + 1),
        )
    }
    pub fn supporters<'a>(&self, s: &'a Space) -> HashSet<Brick> {
        touches(&s, &self.areabelow())
    }
    pub fn fall(&mut self, space: &Space) {
        while is_clear(space, &self.areabelow()) {
            *self = self.downward();
        }
    }
    pub fn height(&self) -> i32 {
        self.p1.2
    }
    pub fn coordinates(&self) -> Vec<(i32, i32, i32)> {
        self._coordinates(self.p1, self.p2)
    }
    fn _coordinates(
        &self,
        (x1, y1, z1): (i32, i32, i32),
        (x2, y2, z2): (i32, i32, i32),
    ) -> Vec<(i32, i32, i32)> {
        let mut v = vec![];
        for z in min(z1, z2)..(max(z1, z2) + 1) {
            for x in min(x1, x2)..(max(x1, x2) + 1) {
                for y in min(y1, y2)..(max(y1, y2) + 1) {
                    v.push((x, y, z))
                }
            }
        }
        v
    }
}
fn is_clear(s: &Space, v: &Vec<(i32, i32, i32)>) -> bool {
    v.iter()
        .all(|(x, y, z)| *z > 0 && !s.contains_key(&(*x, *y, *z)))
}

fn place_brick<'a>(s : & mut Space, b : &'a Brick ) {
    for p in b.coordinates() {
        s.insert(p, *b);
    }
}

fn fall_all<'a>(s : &'a  mut Space, bricks : &  mut  Vec<Brick>) -> &'a  Space {
    for b in bricks  {
        b.fall(s);
        place_brick(s, b);
    }
    s
}
fn touches<'a>(s : &'a Space, cells : &Vec<(i32, i32, i32)>) -> HashSet< Brick> {
    let mut v = HashSet::new();
    for p in cells {
        if let Some(id) = s.get(&p) {
            v.insert(*id);
        }
    }
    v
}

fn num_single_supports_above(s : & Space, b : & Brick) -> usize {
   touches(s, &b.areaabove()).iter().filter(|bid| bid.supporters(&s).len()==1).collect::<Vec<_>>().len()
}
fn above_that_would_fall<'a>(s : & Space, b : &'a Brick) -> Vec<usize> {
    let t = touches(s, &b.areaabove());
    println!("{} {:?}",b.id, t);
    println!("{} {:?}", b.id, b.supporters(&s).len());
    touches(s, &b.areaabove()).iter().filter(|bid| bid.supporters(&s).len()==1).map(|b|b.id).collect::<Vec<_>>()
 }
 

pub fn part01(data: String) -> usize {
    let bricksit = data.lines().enumerate().map(parse_brick);
    let mut bricks: Vec<Brick> = bricksit.sorted_by_key(|brick| brick.height()).collect();
    let mut s = Space::new();
    let s = fall_all(&mut s , &mut bricks);
    let mut disintegrate = HashSet::new();
    for b in bricks {
        if num_single_supports_above(s, &b) == 0 {
            disintegrate.insert(b.id);
        }
    }
    disintegrate.len()
}

pub fn part02(data: String) -> usize {
    let bricksit = data.lines().enumerate().map(parse_brick);
    let mut bricks: Vec<Brick> = bricksit.sorted_by_key(|brick| brick.height()).collect();
    let mut s = Space::new();
    let s = fall_all(&mut s , &mut bricks);
    let mut result = vec![];
    for (ix, test_b) in bricks.iter().enumerate() {
        let mut my_bricks = bricks.clone();
        for mut brick in &mut my_bricks {
            brick.has_moved = false;
        }
        my_bricks.remove(ix);
        let mut s = Space::new();
        fall_all(&mut s , &mut my_bricks);
        result.push(my_bricks.iter().filter(|b| b.has_moved).count());
    }
    println!("{:?}", result);
    result.iter().sum()
}

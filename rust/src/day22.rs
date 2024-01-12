use itertools::Itertools;
use std::{
    cmp::{max, min},
    collections::HashSet,
    sync::{
        mpsc::{self, Sender},
        Arc, Mutex,
    },
    thread::{self, JoinHandle},
};

use crate::util;
use std::collections::HashMap as MM;

pub const NAME: &str = "Day 22: Sand Slabs";

pub fn _example() -> String {
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
        assert_eq!(super::part01(_example()), 5);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(_example()), 0);
    }
}

#[cfg(test)]
mod test_slab {
    use super::*;
    const SLABZ: Brick = Brick {
        id: 0,
        p1: (1, 1, 20),
        p2: (1, 1, 21),
        has_moved: false,
    };

    #[test]
    fn fall() {
        let mut slab = SLABZ.clone();
        slab.fall(&Space::from([((1, 1, 1), SLABZ)]));
        assert_eq!(
            slab,
            Brick {
                id: 0,
                p1: (1, 1, 2),
                p2: (1, 1, 3),
                has_moved: true,
            }
        );
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(_example()), 0);
    }
}

type Space = MM<(i32, i32, i32), Brick>;

#[derive(Debug, Eq, Hash, PartialEq, Ord, PartialOrd, Copy, Clone)]
struct Brick {
    id: usize,
    p1: (i32, i32, i32),
    p2: (i32, i32, i32),
    has_moved: bool,
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
    Brick {
        id: p.0,
        p1,
        p2,
        has_moved: false,
    }
}

impl Brick {
    pub fn downward(&self) -> Brick {
        Brick {
            id: self.id,
            p1: (self.p1.0, self.p1.1, self.p1.2 - 1),
            p2: (self.p2.0, self.p2.1, self.p2.2 - 1),
            has_moved: true,
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
        let mut v = Vec::with_capacity(6);
        for z in z1..(z2 + 1) { // assuming z1 <= z2
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

fn place_brick<'a>(s: &mut Space, b: &'a Brick) {
    for p in b.coordinates() {
        s.insert(p, *b);
    }
}

fn fall_all<'a>(s: &'a mut Space, bricks: &mut Vec<Brick>) {
    for b in bricks {
        b.has_moved = false;
        b.fall(s);
        place_brick(s, b);
    }
}

fn touches<'a>(s: &'a Space, cells: &Vec<(i32, i32, i32)>) -> HashSet<Brick> {
    //let v = HashSet::from_iter(cells.iter().map(|p|s.get(p)).flatten().map(|p|*p));
    let mut v = HashSet::new();
    for p in cells {
        if let Some(id) = s.get(&p) {
            v.insert(*id);
        }
    }
    v
}

fn num_single_supports_above(s: &Space, b: &Brick) -> usize {
    touches(s, &b.areaabove())
        .iter()
        .filter(|bid| bid.supporters(&s).len() == 1)
        .count()
}

pub fn part01(data: String) -> usize {
    let bricksit = data.lines().enumerate().map(parse_brick);
    let mut bricks: Vec<Brick> = bricksit.sorted_by_key(|brick| brick.height()).collect();
    let mut s = Space::with_capacity(1000);
    fall_all(&mut s, &mut bricks);
    let mut disintegrate = HashSet::with_capacity(1000);
    for b in bricks {
        if num_single_supports_above(&s, &b) == 0 {
            disintegrate.insert(b.id);
        }
    }
    disintegrate.len()
}
// 102770
pub fn _part02(data: String) -> usize {
    let bricksit = data.lines().enumerate().map(parse_brick);
    let mut bricks: Vec<Brick> = bricksit.sorted_by_key(|brick| brick.height()).collect();
    let mut s = Space::with_capacity(10000);
    fall_all(&mut s, &mut bricks);
    let mut result = 0;
    for ix in 0..bricks.len() {
        if num_single_supports_above(&s, &bricks[ix]) == 0 {
            continue;
        }
        let mut my_bricks = bricks.clone();
        my_bricks.remove(ix);
        let mut s = Space::new();
        fall_all(&mut s, &mut my_bricks);
        result += my_bricks.iter().filter(|b| b.has_moved).count();
    }
    result
}

pub fn part02(data: String) -> usize {
    let bricksit = data.lines().enumerate().map(parse_brick);
    let  mut bricks:Vec<Brick> = bricksit.sorted_by_key(|brick| brick.height()).collect();
    let mut s = Space::with_capacity(4096);
    fall_all(&mut s, &mut bricks);

    let todo: Vec<usize> = (0..bricks.len())
        .filter(|ix| !(num_single_supports_above(&s, &bricks[*ix]) == 0))
        .collect();

    let (tx, rx) = mpsc::channel();
    let mut_todo: Arc<Mutex<Vec<usize>>> = Arc::new(Mutex::new(todo));
    let mut handles: Vec<JoinHandle<()>> = vec![];
    let shared_bricks = Arc::new(bricks);
    for _ in 0..10 {
        let my_mut_todo = mut_todo.clone();
        let my_tx = tx.clone();
        let sb = shared_bricks.clone();
        handles.push(thread::spawn(move || do_from_todo(my_mut_todo, my_tx, sb)));
    }
    drop(tx);
    rx.iter().sum()
}

fn do_from_todo(todo: Arc<Mutex<Vec<usize>>>, tx: Sender<usize>, bricks: Arc<Vec<Brick>>) {
    while let Some(ix) = {
        let mut unlocked_todo = todo.lock().unwrap();
        unlocked_todo.pop()
    } {
        //println!("doing brick {}", ix);
        let mut my_bricks = (*bricks).clone();
        my_bricks.remove(ix);
        let mut s = Space::with_capacity(4096);
        fall_all(&mut s, &mut my_bricks);
        tx.send(my_bricks.iter().filter(|b| b.has_moved).count())
            .unwrap()
    }
}

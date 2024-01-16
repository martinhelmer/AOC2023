use crate::util::{self, ok_pos, Dir, Pos, EAST, NORTH, SOUTH, WEST, BoolHash};
use array2d::Array2D;
use std::{sync::{mpsc, Arc}, thread};

pub const NAME: &str = "Day 16: (MT, FM) The Floor Will Be Lava";

pub fn _example() -> String {
    String::from(".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")
}

pub fn data() -> String {
    util::get_input("day16.txt")
}

#[cfg(test)]
mod test_result {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(data()), 0);
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
        assert_eq!(super::part01(_example()), 0);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(_example()), 0);
    }
}

fn new_moves(a: &Array2D<char>, this_pos: Pos, this_dir: Dir) -> Vec<(Pos, Dir)> {
    //println!("{:?} {:?}", this_pos, this_pos.as_a2d_index());
    let dirs = match a[this_pos.as_a2d_index()] {
        '.' => vec![this_dir],
        '|' => {
            if this_dir == NORTH || this_dir == SOUTH {
                vec![this_dir]
            } else {
                vec![NORTH, SOUTH]
            }
        }
        '-' => {
            if this_dir == EAST || this_dir == WEST {
                vec![this_dir]
            } else {
                vec![EAST, WEST]
            }
        }
        '/' => match this_dir {
            EAST => vec![NORTH],
            WEST => vec![SOUTH],
            NORTH => vec![EAST],
            SOUTH => vec![WEST],
            _ => panic!("illegal dir"),
        },
        '\\' => match this_dir {
            EAST => vec![SOUTH],
            WEST => vec![NORTH],
            NORTH => vec![WEST],
            SOUTH => vec![EAST],
            _ => panic!("illegal dir"),
        },
        _ => panic!("illegal char"),
    };
    dirs
        .iter()
        .map(|d| (this_pos + *d, *d))
        .filter(|(p, _)| ok_pos(&a, *p))
        .collect()
}
type Bhpd<'a> = BoolHash::<'a, (Pos, Dir)>;

fn calc_energized(a :& Array2D<char>, start_pos : Pos, start_dir : Dir) -> usize {
    let ifx = |n :&_ | {Bhpd::ifx(a.num_columns(),n)};
    let mut bh = Bhpd::new(&ifx, &Bhpd::maxelem(a.num_columns()));
    let mut stack : Vec<(Pos, Dir)> = Vec::with_capacity(1024);
    stack.push((start_pos, start_dir));

    while let Some((mut this_pos, this_dir)) = stack.pop() {
        if bh.contains(&(this_pos, this_dir)) {
            continue;
        }
        loop {
            bh.insert(&(this_pos, this_dir));

            if a[this_pos.as_a2d_index()] == '.' {
                let np = this_pos + this_dir;
                if ok_pos(&a, np) {
                    this_pos = np;
                    continue
                }
            }
            break
        }
        for npos_dir in new_moves(&a, this_pos, this_dir) {
            if !bh.contains(&npos_dir) {
                stack.push(npos_dir);
            }
        }
    }
    bh.v.chunks(4).map(|c| c.iter().any(|b| *b != None )).filter(|p|*p).count()
}
pub fn part01(data: String) -> usize {
    let vvc: Vec<Vec<char>> = data.lines().map(|c| c.chars().collect()).collect();
    let a = Array2D::from_rows(&vvc).unwrap();
    calc_energized(&a, Pos(0,0), EAST)
}

pub fn part02<'b>(data: String) -> usize {
    let vvc: Vec<Vec<char>> = data.lines().map(|c| c.chars().collect()).collect();
    let a = Array2D::from_rows(&vvc).unwrap();
    let num_columns = a.num_columns();

    let aa = Arc::new(a);
    let (tx, rx) = mpsc::channel();
    let tx1 = tx.clone();
    let a1 = aa.clone();
    thread::spawn(move || {
    let r1 = (0..a1.num_rows()).map(|r|calc_energized(&a1, Pos(r as i32,0), EAST)).max().unwrap();
    tx1.send(r1).unwrap();
    });
    
    let tx2 = tx.clone();
    let a2 = aa.clone();
    thread::spawn(move || {
        let r2 = (0..a2.num_rows()).map(|r|calc_energized(&a2, Pos(r as i32,a2.num_columns() as i32-1), WEST)).max().unwrap();
        tx2.send(r2).unwrap();
    });
    
    let tx3 = tx.clone();
    let a3 = aa.clone();
    thread::spawn( move || {
        let r3 = (0..num_columns).map(|c|calc_energized(&a3, Pos(0, c as i32), SOUTH)).max().unwrap();
        tx3.send(r3).unwrap();
    });
    
    let tx4 = tx.clone();
    let a4 = aa.clone();
    thread::spawn( move || {
        let r4 = (0..a4.num_columns()).map(|c|calc_energized(&a4, Pos(a4.num_rows() as i32 -1, c as i32), NORTH)).max().unwrap();
        tx4.send(r4).unwrap();
    });
    drop(tx);
    rx.iter().max().unwrap()
    // for received in rx {
    //     println!("Got: {}", received);
    // }
    // // *([r1,r2,r3,r4].iter().max().unwrap())
    // 0
}

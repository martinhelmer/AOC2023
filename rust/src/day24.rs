
use itertools::Itertools;
use num::{BigInt, BigRational};
use num_bigint::ToBigInt;
// use num_traits::FromPrimitive;
use num_traits::ToPrimitive;

type MyRat = BigRational;

use crate::linalg;
use crate::util;
// use std::u32;

pub const NAME: &str = "Day 24: Never Tell Me The Odds";


pub fn data() -> String {
    util::get_input("day24.txt")
}

#[cfg(test)]
mod test_result {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(data()), 20336);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(data()), 0);
    }
}
// #[cfg(test)]
// mod test_example {
//     use super::*;
//     #[test]
//     fn part01() {
//         assert_eq!(super::part01(_example()), 0);
//     }
//     #[test]
//     fn part02() {
//         assert_eq!(super::part02(_example()), 0);
//     }
// }

#[derive(Debug, PartialEq, PartialOrd, Copy, Clone)]
struct Hailstone2D {
    p: (isize, isize, isize),
    v: (isize, isize, isize),
    dydx: f64,
    c: f64,
}

fn parse_stone(s: &str) -> Hailstone2D {
    let (ps, vs) = s.split_once("@").unwrap();
    let pl: Vec<isize> = ps.split(", ").map(|p| p.trim().parse().unwrap()).collect();
    let vl: Vec<isize> = vs.split(", ").map(|p| p.trim().parse().unwrap()).collect();
    let dydx: f64 = vl[1] as f64 / vl[0] as f64;
    Hailstone2D {
        p: (pl[0], pl[1], pl[2]),
        v: (vl[0], vl[1], vl[2]),
        dydx,
        c: pl[1] as f64 - dydx * pl[0] as f64,
    }
}

fn future_cross_point2d((stone0, stone1): (Hailstone2D, Hailstone2D)) -> Option<(f64, f64)> {
    fn is_future_point(stone: Hailstone2D, x_point: f64) -> bool {
        (stone.v.0 > 0) == (x_point > stone.p.0 as f64)
    }
    if stone0.dydx == stone1.dydx {
        return None;
    };
    let x_intersect = (stone1.c - stone0.c) / (stone0.dydx - stone1.dydx);
    //assert_eq!(y_intersect, stone1.dydx * x_intersect + stone1.c, "just checking..."); // just checking ...

    match is_future_point(stone0, x_intersect) && is_future_point(stone1, x_intersect) {
        true => Some((x_intersect, stone0.dydx * x_intersect + stone0.c)),
        false => None,
    }
}
fn my_tuple_combinations<T: Copy>(v: Vec<T>) -> Vec<(T, T)> {
    let mut u = Vec::with_capacity(v.len() * (v.len() + 1) / 2);
    for i in 0..v.len() {
        for j in (i + 1)..v.len() {
            u.push((v[i], v[j]));
        }
    }
    u
}
fn in_range((x, y): (f64, f64)) -> bool {
    x >= 200000000000000 as i64 as f64
        && x <= 400000000000000 as i64 as f64
        && y >= 200000000000000 as i64 as f64
        && y <= 400000000000000 as i64 as f64
}
// y = mx + c => c = y-mx => c = y0 - dy/dx * x0
//
// m0x+c0 == m1x+c1 <=> (m0-m1)x == (c1-c0) <=> x == (c1-c0) / (m0-m1)
// 20336
pub fn part01(data: String) -> usize {
    let d = data.lines().map(parse_stone).collect_vec();
    let e = my_tuple_combinations(d)
        .iter()
        .map(|i| future_cross_point2d(*i))
        .flatten()
        .filter(|t| in_range(*t))
        .count();
    e
}

// part 2
#[derive(Debug, PartialEq, PartialOrd, Clone)]
struct Hailstone3D {
    m: (MyRat, MyRat, MyRat),
    c: (MyRat, MyRat, MyRat),
}

fn mx(s: &Hailstone3D) -> MyRat {
    s.m.0.clone()
}
fn my(s: &Hailstone3D) -> MyRat {
    s.m.1.clone()
}
fn mz(s: &Hailstone3D) -> MyRat {
    s.m.2.clone()
}
fn cx(s: &Hailstone3D) -> MyRat {
    s.c.0.clone()
}
fn cy(s: &Hailstone3D) -> MyRat {
    s.c.1.clone()
}
fn cz(s: &Hailstone3D) -> MyRat {
    s.c.2.clone()
}

fn parse_stone2(s: &str) -> Hailstone3D {
    let (ps, vs) = s.split_once("@").unwrap();
    let pl: Vec<i64> = ps.split(", ").map(|p| p.trim().parse().unwrap()).collect();
    let vl: Vec<i64> = vs.split(", ").map(|p| p.trim().parse().unwrap()).collect();
    Hailstone3D {
        c: (
            MyRat::from_integer(BigInt::from(pl[0])),
            MyRat::from_integer(BigInt::from(pl[1])),
            MyRat::from_integer(BigInt::from(pl[2])),
        ),
        m: (
            MyRat::from_integer(BigInt::from(vl[0])),
            MyRat::from_integer(BigInt::from(vl[1])),
            MyRat::from_integer(BigInt::from(vl[2])),
        ),
    }
}

fn make_row(s: Hailstone3D) -> Vec<MyRat> {
    [
        MyRat::from_integer(BigInt::from(1)),
        -(cy(&s) + cz(&s)),
        cx(&s),
        (mz(&s) + my(&s)),
        -mx(&s),
        my(&s) * cx(&s) - cy(&s) * mx(&s) + mz(&s) * cx(&s) - cz(&s) * mx(&s),
    ]
    .into_iter()
    .collect_vec()
}

fn br2iv(v: Vec<Vec<MyRat>>) -> Vec<Vec<BigInt>> {
    //println!("{:?}", v );
    v.iter()
        .map(|v2| {
            v2.iter()
                .map(|i| i.to_integer().to_bigint().unwrap())
                .collect_vec()
        })
        .collect_vec()
}

pub fn part02(data: String) -> usize {
    let stones = data.lines().map(parse_stone2).map(make_row).collect_vec();
    let reduction = br2iv(linalg::gauss_jordan_reduction(stones[0..7].to_vec()));
    (reduction[3][5].clone() + reduction[4][5].clone()).to_usize().unwrap()
}

#![allow(unused)]

use num::{BigRational, BigInt};
use num_traits::identities::zero;
use num_traits::{Float, Num, NumOps, Zero};
use std::ops::{Div, Mul, Sub};

pub fn gauss_jordan_reduction<T : Clone + Num>(mut am: Vec<Vec<T>>) -> Vec<Vec<T>> {
    let num_rows = am.len();
    let num_columns = am[0].len();

    let mut top_row = 0;
    for j in 0..num_columns {
        if let Some(non_zero_row) = (top_row..(am.len())).filter(|i| am[*i][j] != zero()).next() {
            am.swap(non_zero_row, top_row);
            let c = am[top_row][j].clone();
            for mut cell in &mut am[top_row] {
                // normalize
                *cell = cell.clone() / c.clone();
            }
            for i in 0..num_rows {
                if i != top_row && am[i][j] != zero() {
                    let f = am[i][j].clone();
                    for jj in 0..num_columns {
                        am[i][jj] = am[i][jj].clone() - am[top_row][jj].clone() * f.clone();
                    }
                }
            }
            top_row += 1;
        }
    }
    am
}
#[cfg(test)]
mod test_g1 {
    use std::fmt::Debug;

    use itertools::Itertools;
    use num_traits::FromPrimitive;

    fn i2brv(v :Vec<Vec<i32>>) -> Vec<Vec<BigRational>> {
        v
            .iter()
            .map(|v2| {
                v2.iter()
                    .map(|i| BigRational::from_i32(*i).unwrap())
                    .collect_vec()
            })
            .collect_vec()
    }
    fn br2iv(v :  Vec<Vec<BigRational>>) -> Vec<Vec<BigInt>> {
        v
        .iter()
        .map(|v2| {
            v2.iter()
                .map(|i| i.to_integer())
                .collect_vec()
        })
        .collect_vec()
    }
    use super::*;
    #[test]
    fn part01() {
        let vbr = i2brv(vec![vec![2, 4, 6, 8], vec![3, 6, 9, 10]]);
        assert_eq!(
            format!("{:?}", br2iv(gauss_jordan_reduction(vbr))),
            "[[1, 2, 3, 0], [0, 0, 0, 1]]"
        );
    }
}
//     #[test]
//     fn part02() {
//         let v = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
//         let vv: Vec<_> = v
//             .iter()
//             .map(|q| q.iter().map(|r| *r as f32).collect::<Vec<_>>())
//             .collect();
//         assert_eq!(
//             format!("{:?}", gauss_jordan_reduction(vv)),
//             "[[1.0, 0.0, -1.0], [-0.0, 1.0, 2.0], [0.0, 0.0, 0.0]]"
//         );
//     }
//         #[test]
//         fn part03() {
//             let v = [[1, 2, -1, 2], [1, 1, -1, 0], [2, -1, 1, 3], [2, -1, 1, 3]];
//             let vv: Vec<_> = v
//                 .iter()
//                 .map(|q| q.iter().map(|r| *r as f32).collect::<Vec<_>>())
//                 .collect();
//             assert_eq!(
//                 format!("{:?}", gauss_jordan_reduction(vv)),
//                 "[[1.0, 0.0, -1.0], [-0.0, 1.0, 2.0], [0.0, 0.0, 0.0]]"
//             );

//     }
// }

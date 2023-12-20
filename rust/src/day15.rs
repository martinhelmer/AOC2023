
use std::collections::HashMap;

use crate::util;
use priority_queue::PriorityQueue;

pub const NAME: &str = "Day 15: Lens Library";

pub fn example() -> String {
    String::from(
        "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7",
    )
}

pub fn data() -> String {
    util::get_input("day15.txt")
}

fn hash(s : &str ) -> usize {
    s.bytes().fold(0, |acc, b| (acc + b as usize) * 17 % 256 )
}

mod test_hash {
    use super::*;
    #[test]
    fn HASH() {
        assert_eq!(super::hash("HASH"), 52);
        assert_eq!(super::hash("rn=1"), 30);
        assert_eq!(super::hash("cm-"), 253); 
    }
}


// 503982 too low 
#[cfg(test)]
mod test_result {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(data()), 504036);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(data()), 295719);
    }
}
#[cfg(test)]
mod test_example {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(example()), 1320);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(example()), 145);
    }
}

pub fn part01(data: String) -> usize {
    // println!("{}",data.lines().collect::<Vec<&str>>().len());
    data.trim().split(",").map(hash).sum()

}

enum Oper { 
    Assign , 
    Remove , 
}
pub fn part02(data: String) -> usize {
    let mut pqs : Vec<PriorityQueue<&str, isize>> = vec![];
    let mut values : HashMap<&str, usize> = HashMap::new();
    for _ in 00..256 {
        let mut pq = PriorityQueue::new();
        pqs.push(pq);
    }
    for (ix, instr) in data.trim().split(",").enumerate() {
        let q : Vec<&str> = instr.split(|c| c == '=' || c == '-').collect();
        let label = q[0];
        // println!("{:?}",q);
        let i : Oper = match instr.find('=') {
            Some(_) => Oper::Assign,
            None => Oper::Remove,
        };
        let pq = &mut pqs[hash(label)];
        match i {
            Oper::Remove => { pq.remove(label); values.remove(label);}
            Oper::Assign => match values.insert(label, q[1].parse().unwrap()) {
                None => {pq.push(label, -(ix as isize));},
                Some(_) => ()
            }
        }
    }
    let mut sum = 0;
    for (boxix, pq ) in pqs.iter().enumerate() {
        for (slotix, (label,_)) in pq.clone().into_sorted_iter().enumerate() {
            sum+= (boxix+1)* (slotix+1) * values.get(&label).unwrap();
        }
        
    }
    // println!("{:?}",pqs);
    // println!("{:?}",values);
    sum
}
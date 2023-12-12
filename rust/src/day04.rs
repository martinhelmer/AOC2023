use crate::util;
use std::num::ParseIntError;

pub const NAME :&str = "Day 4: Scratchcards";

pub fn example() -> String {
    String::from(
        "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11",
    )
}

pub fn data() -> String {
    util::get_input("day04.txt")
}

#[derive(Debug)]
struct Card {
    id: usize,
    wins: usize,
}

fn s_to_ints(s: &str) -> Result<Vec<usize>, ParseIntError> {
    s.split_whitespace().map(|x| x.parse::<usize>()).collect()
}

fn points(c: Card) -> usize {
    match c.wins {
        0 => 0,
        w => 2_usize.pow(w as u32 - 1),
    }
}
// Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
fn do_card(s: &str) -> Card {
    let (card, rest) = s.split_once(":").unwrap();
    let a: Vec<_> = rest.split("|").map(|s| s_to_ints(s).unwrap()).collect();
    let id: usize = card.split(' ').last().unwrap().parse().unwrap();
    let e: Vec<_> = a[1].iter().filter(|i| a[0].contains(i)).collect(); // assuming each number only shows up
    Card { id, wins: e.len() }
}

// 32001
pub fn part01(data: String) -> usize {
    let w: usize = data.lines().map(|s| points(do_card(s))).sum();
    // println!("{:?}", w);
    w
}

// 5037841
pub fn part02(data: String) -> usize{
    let cards: Vec<Card> = data.lines().map(|s| do_card(s)).collect();
    let mut collected: Vec<usize> = vec![1; cards.len()];
    for card in cards.iter() {
        let cardcopies = collected[card.id - 1];
        for ix in 0..(card.wins) {
            // messing around with references in order to learn when all that is necessary is
            //collected[card.id + ix] += cardcopies;
            let r: &mut usize = &mut collected[card.id + ix];
            *r += cardcopies;
        }
    }
    // println!("{}", collected.iter().sum::<usize>());
    collected.iter().sum()
}

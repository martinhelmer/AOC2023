use crate::util;
use std::{cmp::max, usize};

pub const NAME :&str = "Day 2: Cube Conundrum";

pub fn data() -> String {
    let contents: String = util::get_input("day02.txt");
    contents
}

pub fn _example() -> String {
    String::from(
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green",
    )
}

// fn line() -> String {
//     let s = String::from("Game 9: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green");
//     s
// }

#[derive(Debug)]
struct Game {
    number: usize,
    blue: usize,
    red: usize,
    green: usize,
}

// " 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
fn parse_cubes(cubes: &str) -> Game {
    let mut g = Game {
        number: 0,
        blue: 0,
        red: 0,
        green: 0,
    };
    for cube in cubes.split(&[',', ';']).map(|x| x.trim()) {
        let (n, col) = cube.split_once(' ').unwrap();
        let n: usize = n.parse().unwrap();
        match col {
            "blue" => g.blue = max(n, g.blue),
            "red" => g.red = max(n, g.red),
            "green" => g.green = max(n, g.green),
            _ => panic!("oops"),
        }
    }
    g
}

fn parse_gameno(game: &str) -> usize {
    game.split_once(" ").unwrap().1.parse().unwrap()
}

fn parse_game(s: &str) -> Game {
    let (game, cubes) = s.split_once(':').unwrap();
    Game {
        number: parse_gameno(game),
        ..parse_cubes(cubes)
    }
}

fn ok1(g: &Game) -> bool {
    g.blue <= 14 && g.green <= 13 && g.red <= 12
}

// 2512
pub fn part01(data: String) -> usize{
    let s : usize = data
        .lines()
        .map(parse_game)
        .filter(ok1)
        .map(|g| g.number )
        .sum();
    s
}

// 67335
pub fn part02(data: String) -> usize {
    let s : usize = data
        .lines()
        .map(parse_game)
        .map(|g| g.red * g.green * g.blue )
        .sum();
    s
}


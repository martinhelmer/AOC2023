use std::env;
use std::time::Instant;

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day11;
mod day12;
mod day13;
mod day14; 
mod day15;
mod day17;
mod day19;

mod util;

fn runday(e: &String) {
    match e.as_str() {
        "day01" => {
            day01::part01(day01::data());
            day01::part02(day01::data());
        }
        "day01b" => {
            day01::part01b();
            day01::part02b()
        }
        "day01c" => {
            day01::part01c();
            day01::part02c()
        }
        "day02" => {
            day02::part01(day02::data());
            day02::part02(day02::data());
        }
        "ex02" => {
            day02::part01(day02::example());
        }
        "day03" => {
            day03::part01(day03::data());
        }
        "ex03" => {
            day03::part01(day03::example());
        }
        "day04" => {
            day04::part01(day04::data());
            day04::part02(day04::data());
        }
        "ex04" => {
            day04::part01(day04::example());
            day04::part02(day04::example());
        }
        "day05" => {
            day05::part01(day05::data());
            day05::part02(day05::data());
        }
        "day11" => {
            day11::part01(day11::data());
            day11::part02(day11::data());
            ()
        }
        "ex11" => {
            day11::part01(day11::example());
            day11::part02(day11::example());
            ()
        }
        "day12" => {
            println!("{}", day12::part01(day12::data()));
            println!("{}", day12::part02(day12::data()));
            ()
        }
        "day13" => {
            println!("34911 = {}", day13::part01(day13::data()));
            println!("33183 = {}", day13::part02(day13::data()));
            ()
        }
        "day14" => {
            println!("109833 = {}", day14::part01(day14::data()));
            println!("99875 = {}", day14::part02(day14::data()));
            ()
        }
        "day15" => {
            println!("504036 = {}", day15::part01(day15::data()));
            println!("295719 = {}", day15::part02(day15::data()));
            ()
        }
        "ex14" => {
            println!("? = {}", day14::part01(day14::example()));
            println!("64 = {}", day14::part02(day14::example()));
            ()
        }
        "ex17" => {
            println!("102 = {}", day17::part01(day17::example()));
            println!("94 = {}", day17::part02(day17::example()));
            ()
        }
        "day17" => {
            println!("674 = {}", day17::part01(day17::data()));
            println!("773 = {}", day17::part02(day17::data()));
            ()
        }
        "day19" => {
            println!("? = {}", day19::part01(day19::data()));
            println!("? = {}", day19::part02(day19::data()));
            ()
        }
        "ex19" => {
            println!("? = {}", day19::part01(day19::example()));
            println!("? = {}", day19::part02(day19::example()));
            ()
        }
        _ => println!("N/A"),
    }
}

fn get_module_stuff(
    s: &String,
) -> (
    &'static str,
    fn() -> String,
    fn(String) -> usize,
    fn(String) -> usize,
) {
    match s.as_str() {
        "day01" => (day01::NAME, day01::data, day01::part01, day01::part02),
        "day02" => (day02::NAME, day02::data, day02::part01, day02::part02),
        "day03" => (day03::NAME, day03::data, day03::part01, day03::part02),
        "day04" => (day04::NAME, day04::data, day04::part01, day04::part02),
        "day05" => (day05::NAME, day05::data, day05::part01, day05::part02),
        "day11" => (day11::NAME, day11::data, day11::part01, day11::part02),
        "day12" => (day12::NAME, day12::data, day12::part01, day12::part02),
        "day13" => (day13::NAME, day13::data, day13::part01, day13::part02),
        "day14" => (day14::NAME, day14::data, day14::part01, day14::part02),
        "day15" => (day15::NAME, day15::data, day15::part01, day15::part02),
        "day17" => (day17::NAME, day17::data, day17::part01, day17::part02),
        "day19" => (day19::NAME, day19::data, day19::part01, day19::part02),
        _ => panic!("main: no match!"),
    }
}
fn runday2(e: &String) {
    let (n, data, p1, p2) = get_module_stuff(e);
    let p1_now = Instant::now();
    let _p1_result = p1(data());
    let p1_elapsed = p1_now.elapsed();
    let p2_now = Instant::now();
    let _p2_result = p2(data());
    let p2_elapsed = p2_now.elapsed();
    println!(
        "{:<35} | {:>20} | {:>20} |",
        n,
        p1_elapsed.as_micros(),
        p2_elapsed.as_micros()
    );
}
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Running ALL days");
        runday2(&String::from("day01"));
        runday2(&String::from("day02"));
        runday2(&String::from("day03"));
        runday2(&String::from("day04"));
        runday2(&String::from("day05"));
        runday2(&String::from("day11"));
        runday2(&String::from("day12"));
        runday2(&String::from("day13"));
        runday2(&String::from("day14"));
        runday2(&String::from("day15"));
        runday2(&String::from("day17"));
        runday2(&String::from("day19"));
    } else {
        let day = &args[1];
        let now = Instant::now();
        runday(day);
        let elapsed = now.elapsed();
        println!("Elapsed: {:.2?}", elapsed);
    }
}

use std::env;
use std::time::Instant;

mod day01; 
mod day02;
mod day03;
mod util; 

fn runday(e: &String) {
    match e.as_str() {
        "day01" => {day01::part01(); day01::part02()} ,
        "day01b" => {day01::part01b(); day01::part02b()} ,
        "day01c" => {day01::part01c(); day01::part02c()} ,
        "day02" => {day02::part01(&day02::data()); 
                    day02::part02(&day02::data())}
        "ex02" => day02::part01(&day02::example()),
        "day03" => {day03::part01(&day03::data())},
        "ex03" => {day03::part01(&day03::example())},
        _ => println!("N/A"),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Running ALL days") 
    } else
    {let day = &args[1];
     let now = Instant::now();
     runday(day);
     let elapsed = now.elapsed();
     println!("Elapsed: {:.2?}", elapsed); }
}
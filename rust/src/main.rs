use std::env;

fn runday(e: &String) {
    match e.as_str() {
        "day01" => println!("Day 1"),
        "day02" => println!("Day 2"),
        _ => println!("N/A"),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let day = &args[1];
    runday(day);
}
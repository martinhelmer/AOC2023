use std::collections::HashMap;
use std::env;
use std::time::Instant;

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day05b;
mod day05c;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;
mod day20;
mod day21;
mod day22;
mod day23;
mod day24;
mod day25;

mod linalg;
mod util;

fn runday(e: &String) {
    let (n, data, p1, p2) = get_module_stuff(e);
    println!("{}", n);
    let (p1_expected, p2_expected) = get_expected(e);
    let p1_now = Instant::now();
    let _p1_result = p1(data());
    let p1_elapsed = p1_now.elapsed();
    println!(
        "Part1 : {} in {:?}",
        color_result(_p1_result, p1_expected),
        p1_elapsed
    );
    let p2_now = Instant::now();
    let _p2_result = p2(data());
    let p2_elapsed = p2_now.elapsed();
    println!(
        "Part2 : {} in {:?}",
        color_result(_p2_result, p2_expected),
        p2_elapsed
    );
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
        "day05b" => (day05b::NAME, day05b::data, day05b::part01, day05b::part02),
        "day05c" => (day05c::NAME, day05c::data, day05c::part01, day05c::part02),
        "day06" => (day06::NAME, day06::data, day06::part01, day06::part02),
        "day07" => (day07::NAME, day07::data, day07::part01, day07::part02),
        "day08" => (day08::NAME, day08::data, day08::part01, day08::part02),
        "day09" => (day09::NAME, day09::data, day09::part01, day09::part02),
        "day10" => (day10::NAME, day10::data, day10::part01, day10::part02),
        "day11" => (day11::NAME, day11::data, day11::part01, day11::part02),
        "day12" => (day12::NAME, day12::data, day12::part01, day12::part02),
        "day13" => (day13::NAME, day13::data, day13::part01, day13::part02),
        "day14" => (day14::NAME, day14::data, day14::part01, day14::part02),
        "day15" => (day15::NAME, day15::data, day15::part01, day15::part02),
        "day16" => (day16::NAME, day16::data, day16::part01, day16::part02),
        "day17" => (day17::NAME, day17::data, day17::part01, day17::part02),
        "day18" => (day18::NAME, day18::data, day18::part01, day18::part02),
        "ex18" => (day18::NAME, day18::example, day18::part01, day18::part02),
        "day19" => (day19::NAME, day19::data, day19::part01, day19::part02),
        "day20" => (day20::NAME, day20::data, day20::part01, day20::part02),
        "day21" => (day21::NAME, day21::data, day21::part01, day21::part02),
        "day22" => (day22::NAME, day22::data, day22::part01, day22::part02),
        "day23" => (day23::NAME, day23::data, day23::part01, day23::part02),
        "day24" => (day24::NAME, day24::data, day24::part01, day24::part02),
        "day25" => (day25::NAME, day25::data, day25::part01, day25::part02),
        _ => panic!("main: no match!"),
    }
}

fn get_expected(s: &str) -> (Option<usize>, Option<usize>) {
    let h = HashMap::from([
        ("day01", (Some(55386), Some(54824))),
        ("day02", (Some(2512), Some(67335))),
        ("day03", (Some(544664), Some(84495585))),
        ("day04", (Some(32001), Some(5037841))),
        ("day05b", (Some(389056265), Some(137516820))),
        ("day05c", (Some(389056265), Some(137516820))),
        ("day06", (Some(3317888), Some(24655068))),
        ("day07", (Some(251106089), Some(249620106))),
        ("day08", (Some(23147), Some(22289513667691))),
        ("day09", (Some(1898776583), Some(1100))),
        ("day10", (Some(7005), Some(417))),
        ("day11", (Some(10292708), Some(790194712336))),
        ("day12", (Some(7007), Some(3476169006222))),
        ("day13", (Some(34911), Some(33183))),
        ("day14", (Some(109833), Some(99875))),
        ("day15", (Some(504036), Some(295719))),
        ("day16", (Some(7482), Some(7896))),
        ("day17", (Some(674), Some(773))),
        ("day18", (Some(49578), Some(52885384955882))),
        ("day19", (Some(425811), Some(131796824371749))),
        ("day20", (Some(812721756), Some(233338595643977))),
        ("day21", (Some(3768), Some(627960775905777))),
        ("day22", (Some(509), Some(102770))),
        ("day23", (Some(2178), Some(6486))),
        ("day24", (Some(20336), Some(677656046662770))),
        ("day25", (Some(543256), None)),
    ]);
    let l = h.get(s);
    match l {
        None => return (None, None),
        Some(i) => return *i,
    }
}

fn _get_expected(s: &str) -> (Option<usize>, Option<usize>) {
    let h = HashMap::from([
        ("day01", (Some(55386), Some(54824))),
        ("day02", (Some(2512), Some(67335))),
        ("day03", (Some(544664), Some(84495585))),
        ("day04", (Some(32001), Some(5037841))),
        ("day05c", (Some(462648396), Some(2520479))),
        ("day06", (Some(3317888), Some(24655068))),
        ("day07", (Some(251106089), Some(249620106))),
        ("day08", (Some(23147), Some(22289513667691))),
        ("day09", (Some(1898776583), Some(1100))),
        ("day10", (Some(7005), Some(417))),
        ("day11", (Some(10292708), Some(790194712336))),
        ("day12", (Some(7007), Some(3476169006222))),
        ("day14", (Some(109833), Some(99875))),
        ("day15", (Some(504036), Some(295719))),
        ("day16", (Some(7482), Some(7896))),
        ("day17", (Some(722), Some(894))),
        ("day18", (Some(49578), Some(52885384955882))),
        ("day19", (Some(425811), Some(131796824371749))),
        ("day20", (Some(812721756), Some(233338595643977))),
        ("day21", (Some(3768), Some(627960775905777))),
        ("day22", (Some(515), Some(101541))),
        ("day23", (Some(2178), Some(6486))),
        ("day24", (Some(21785), Some(554668916217145))),
        ("day25", (Some(506202), None)),
    ]);
    let l = h.get(s);
    match l {
        None => return (None, None),
        Some(i) => return *i,
    }
}

fn color_result(r: usize, xpect: Option<usize>) -> String {
    let rs = format!("{:>18}", r);
    if let Some(x) = xpect {
        if r == x {
            return ansi_term::Colour::Green.paint(rs).to_string();
        } else {
            return format!("{} ({})", ansi_term::Colour::Red.paint(rs).to_string(), x);
        }
    }
    rs
}

fn color_it(t: usize, expected: Option<usize>, r: usize) -> String {
    let s = format!(
        "{:>8}",
        if r != 0 {
            t.to_string()
        } else {
            "".to_string()
        }
    );
    if let Some(x) = expected {
        if x == r {
            return ansi_term::Colour::Green.paint(s).to_string();
        } else {
            return ansi_term::Colour::Red.paint(s).to_string();
        }
    }
    return s;
}
fn runday2(e: &String) {
    let (n, data, p1, p2) = get_module_stuff(e);
    let (p1_expected, p2_expected) = get_expected(e);
    let p1_now = Instant::now();
    let _p1_result = p1(data());
    let p1_elapsed = p1_now.elapsed();
    let p2_now = Instant::now();
    let _p2_result = p2(data());
    let p2_elapsed = p2_now.elapsed();
    println!(
        "{:<40} | {} | {} |",
        n,
        color_it(p1_elapsed.as_micros() as usize, p1_expected, _p1_result),
        color_it(p2_elapsed.as_micros() as usize, p2_expected, _p2_result),
    );
}
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Running ALL days");
        let t0 = Instant::now();
        [
            "day01", "day02", "day03", "day04", "day05c", "day06", "day07", "day08", "day09",
            "day10", "day11", "day12", "day13", "day14", "day15", "day16", "day17", "day18",
            "day19", "day20", "day21", "day22", "day23", "day24", "day25",
        ]
        .map(|s| runday2(&String::from(s)));
        println!("-----------------------------------------+----------+----------+--");
        println!(
            "                                            {} | {:>8} |",
            ansi_term::Colour::Purple.paint("Total :"),
            ansi_term::Colour::Purple.paint(format!("{:>8}",t0.elapsed().as_micros()))
        );
    } else {
        let day = &args[1];
        let now = Instant::now();
        runday(day);
        let elapsed = now.elapsed();
        println!("Elapsed: {:.2?}", elapsed);
    }
}

use crate::util;
use queues::{IsQueue, Queue};
use std::collections::HashMap as MM;

pub const NAME: &str = "Day 20: Pulse Propagation";

pub fn _example() -> String {
    String::from(
        "broadcaster -> aa
%aa -> inv, con
&inv -> bb
%bb -> con
&con -> output",
    )
}

pub fn data() -> String {
    util::get_input("day20.txt")
}

#[cfg(test)]
mod test_result {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(data()), 812721756);
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
// types
#[derive(Debug, Eq, PartialEq, Clone)]
enum Type {
    Broadcast,
    FlipFlop,
    Conjunction,
}
#[derive(Debug, Eq, PartialEq, Clone)]
struct Module<'a> {
    t: Type,
    id: &'a str,
    state: bool,
    outputs: Vec<&'a str>,
    input_history: MM<&'a str, bool>,
}

#[derive(Debug, Eq, Hash, PartialEq, Ord, PartialOrd, Clone)]
struct Signal<'b> {
    source: &'b str,
    target: &'b str,
    value: bool,
}

// ***************

fn process_signal<'b>(q: &mut Queue<Signal<'b>>, signal: Signal<'b>, m: &mut Module<'b>) {
    match (*m).t {
        Type::Broadcast => {
            for output in &(*m).outputs[0..] {
                q.add(Signal {
                    source: m.id,
                    target: *output,
                    value: signal.value,
                })
                .unwrap();
            }
        }
        Type::Conjunction => {
            let h = m.input_history.get_mut(&signal.source).unwrap();
            *h = signal.value;
            //if m.id == "lv" && signal.value { println!("lv! s:{} v:{}, {:?}", &signal.source, signal.value, m.input_history);};
            //println!("Conjunction  m:  {:?}", m);
            let value = !m.input_history.values().all(|c| *c);
            for output in &(*m).outputs[0..] {
                q.add(Signal {
                    source: m.id,
                    target: *output,
                    value,
                })
                .unwrap();
            }
        }
        Type::FlipFlop => {
            // println!("FlipFlop processing {:?}", signal);
            if !signal.value {
                m.state = !m.state;
                for output in &(*m).outputs[0..] {
                    q.add(Signal {
                        source: m.id,
                        target: *output,
                        value: m.state,
                    })
                    .unwrap();
                }
            }
        }
    }
}
// %qq -> th, gr
// &vc -> gl, tv, pc, qd, tn, dg
fn parse_node(s: &str) -> Module {
    let id = &s[1..].split_ascii_whitespace().next().unwrap();
    let (_, targets) = s.split_once(" -> ").unwrap();
    let outputs: Vec<&str> = targets.split(", ").collect();
    match s.as_bytes()[0] {
        b'%' => Module {
            t: Type::FlipFlop,
            id,
            state: false,
            outputs,
            input_history: MM::new(),
        },
        b'&' => Module {
            t: Type::Conjunction,
            id,
            state: false,
            outputs,
            input_history: MM::new(),
        },
        _ => Module {
            t: Type::Broadcast,
            id: "broadcaster",
            state: false,
            outputs,
            input_history: MM::new(),
        },
    }
}

fn push_button(m: &mut MM<&str, Module>) -> (usize, usize) {
    let mut q: Queue<Signal> = Queue::new();
    q.add(Signal {
        source: "button",
        target: "broadcaster",
        value: false,
    })
    .unwrap();
    let mut lo_count = 0;
    let mut hi_count = 0;

    while let Ok(signal) = q.remove() {
        //println!("{:?}", signal);
        match signal.value {
            true => hi_count += 1,
            false => lo_count += 1,
        }
        if let Some(target_module) = m.get_mut(signal.target) {
            process_signal(&mut q, signal, target_module);
        }
    }
    (lo_count, hi_count)
}

fn push_button2<'b>(nn: usize, m: &mut MM<&'b str, Module<'b>>) -> MM<&'b str, bool> {
    let mut q: Queue<Signal> = Queue::new();
    let mut hh = vec![];
    
    q.add(Signal {
        source: "button",
        target: "broadcaster",
        value: false,
    })
    .unwrap();
    let mut n = 0;
    //let mut print_final = false;
    while let Ok(signal) = q.remove() {
        n += 1;
        if let Some(target_module) = m.get_mut(signal.target) {
            process_signal(&mut q, signal, target_module);
            if target_module.id == "lv" {
                if *target_module.input_history.get("hh").unwrap() {
                    match hh.last() {
                        None => hh.push(nn),
                        Some(l) => if *l != nn { hh.push(n)}
                    }
                }
                if *target_module.input_history.get("dt").unwrap() {
                    // println!("DT {}/{}", nn, n);
                    // print_final = true;
                }
                if *target_module.input_history.get("st").unwrap() {
                    // println!("ST {}/{}", nn, n);
                    //print_final = true;
                }
                if *target_module.input_history.get("tn").unwrap() {
                    // println!("TN {}/{}", nn, n);
                    // print_final = true;
                }
            }
        }
    }
    // if print_final {
    //     println!("FINAL {:?}", m.get("lv").unwrap().input_history);
    // }
    //println!("{}",n);
    let h = m.get("lv").unwrap().input_history.clone();
    h
}

//player_stats.entry("mana").and_modify(|mana| *mana += 200).or_insert(100);
pub fn part01(data: String) -> usize {
    let modules: Vec<_> = data.lines().map(parse_node).collect();
    let mut collected_inputs: MM<&str, Vec<&str>> = MM::new();
    for module in &modules[0..] {
        let k = &module.id;
        for output in &module.outputs {
            collected_inputs
                .entry(output)
                .and_modify(|v| (*v).push(k))
                .or_insert(vec![k]);
        }
    }
    let mut m: MM<&str, Module> = MM::new(); // main module lookup map
    for module in modules {
        if let Some(inp_hist_1) = collected_inputs.get(&module.id) {
            let hm: MM<&str, bool> = MM::from_iter(inp_hist_1.iter().map(|i| (*i, false)));
            m.insert(
                module.id,
                Module {
                    input_history: hm,
                    ..module
                },
            );
        } else {
            m.insert(module.id, module);
        }
    }

    // println!("{:?}", m);
    // run here
    let mut lo_count = 0;
    let mut hi_count = 0;
    for _i in 0..1000 {
        let (lo, hi) = push_button(&mut m);
        lo_count += lo;
        hi_count += hi;
    }
    hi_count * lo_count
}

pub fn part02(data: String) -> usize {
    let modules: Vec<_> = data.lines().map(parse_node).collect();
    let mut collected_inputs: MM<&str, Vec<&str>> = MM::new();
    for module in &modules[0..] {
        let k = &module.id;
        for output in &module.outputs {
            collected_inputs
                .entry(output)
                .and_modify(|v| (*v).push(k))
                .or_insert(vec![k]);
        }
    }
    let mut m: MM<&str, Module> = MM::new(); // main module lookup map
    for module in modules {
        if let Some(inp_hist_1) = collected_inputs.get(&module.id) {
            let hm: MM<&str, bool> = MM::from_iter(inp_hist_1.iter().map(|i| (*i, false)));
            m.insert(
                module.id,
                Module {
                    input_history: hm,
                    ..module
                },
            );
        } else {
            m.insert(module.id, module);
        }
    }

    //println!("{:?}", m);
    // run here
    let mut st = 0;
    let mut tn = 0;
    let mut hh = 0;
    let mut dt = 0;
    for i in 0..100000 {
        let hm = push_button2(i, &mut m);
        //println!("{:?}", hm);
        if st == 0 && *hm.get("st").unwrap() {
            st = i + 1;
            println!("st = {}", st)
        };
        if tn == 0 && *hm.get("tn").unwrap() {
            tn = i + 1;
            println!("tn = {}", tn)
        };
        if hh == 0 && *hm.get("hh").unwrap() {
            hh = i + 1;
            println!("hh = {}", hh)
        };
        if dt == 0 && *hm.get("dt").unwrap() {
            dt = i + 1;
            println!("dt = {}", dt)
        };
    }
    0
}

// &lv -> rx  rx = low -> all inputs of lv is high.
// lv inputs ->
//     &st -> lv
//     &tn -> lv
//     &hh -> lv
//     &dt -> lv

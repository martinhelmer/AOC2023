use crate::util;
use std::cmp::{max, min};
use std::collections::HashMap as MM;
use std::slice;

pub const NAME: &str = "Day 19: Aplenty";

pub fn example() -> String {
    String::from(
        "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}",
    )
}

const _WORKFLOWS: &str = "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}";

pub fn data() -> String {
    util::get_input("day19.txt")
}

#[cfg(test)]
mod test_result {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(data()), 425811);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(data()), 131796824371749);
    }
}
#[cfg(test)]
mod test_example {
    use super::*;
    #[test]
    fn part01() {
        assert_eq!(super::part01(example()), 19114);
    }
    #[test]
    fn part02() {
        assert_eq!(super::part02(example()), 167409079868000);
    }
}

#[derive(Debug, Eq, Hash, PartialEq, Ord, PartialOrd)]
struct Part {
    x: usize,
    m: usize,
    a: usize,
    s: usize,
}

#[derive(Debug, Eq, Hash, PartialEq, Ord, PartialOrd, Clone, Copy)]
struct PartRange {
    x: (usize, usize), // inclusive
    m: (usize, usize),
    a: (usize, usize),
    s: (usize, usize),
}

const START_PR: PartRange = PartRange {
    x: (1, 4000),
    m: (1, 4000),
    a: (1, 4000),
    s: (1, 4000),
};

#[derive(Debug, Eq, Hash, PartialEq, Ord, PartialOrd)]
enum Test {
    Accepted,
    Rejected,
    Goto(String),
    GotoIf((char, char, usize, Box<Test>)),
}

fn target(s: &str) -> Test {
    match s {
        "A" => Test::Accepted,
        "R" => Test::Rejected,
        t => Test::Goto(t.to_string()),
    }
}

fn do_goto_if(p: &Part, attr: char, cmp: char, v: usize) -> bool {
    let pv = match attr {
        'x' => p.x,
        'm' => p.m,
        'a' => p.a,
        's' => p.s,
        _ => panic!("wrong attr"),
    };
    return if cmp == '>' { pv > v } else { pv < v };
}

fn do_rule<'a>(part: &'a Part, rule: &'a Vec<Test>) -> &'a Test {
    for subrule in rule {
        match subrule {
            Test::GotoIf(tup) => {
                if do_goto_if(part, tup.0, tup.1, tup.2) {
                    return &tup.3;
                } else {
                    continue;
                }
            }
            x => return x,
        }
    }
    &Test::Accepted
}
fn process_part(part: &Part, workflows: &MM<String, Vec<Test>>) -> Test {
    let mut rule_id = "in".to_string();
    loop {
        let this_rule = workflows.get(&rule_id).unwrap();
        match do_rule(part, this_rule) {
            Test::Accepted => {
                return Test::Accepted;
            }
            Test::Rejected => {
                return Test::Rejected;
            }
            Test::Goto(rr) => rule_id = rr.to_string(),
            _ => panic!("eehh"),
        }
    }
}

fn parse_workflow(s: &str) -> (String, Vec<Test>) {
    let (id, rest) = s.split_once('{').unwrap();
    let mut subrules: Vec<Test> = vec![];
    for sub in rest.split(",") {
        match sub.strip_suffix('}') {
            Some(t) => {
                subrules.push(target(t));
            }
            None => {
                // a<2006:qkq
                // println!("*{}*", sub);
                let (cond, t) = sub.split_once(':').unwrap();
                let mut ci = cond[0..].chars();
                let c = ci.next().unwrap();
                let op = ci.next().unwrap();
                let num: usize = cond[2..].parse().unwrap();
                subrules.push(Test::GotoIf((c, op, num, Box::new(target(t)))));
            }
        }
    }
    (id.to_string(), subrules)
}

// {x=787,m=2655,a=1222,s=2876}
fn parse_part(s: &str) -> Part {
    let comp: Vec<_> = s
        .strip_prefix('{')
        .unwrap()
        .strip_suffix('}')
        .unwrap()
        .split(|c: char| !c.is_numeric())
        .filter(|r| *r != "")
        .collect();
    Part {
        x: comp[0].parse().unwrap(),
        m: comp[1].parse().unwrap(),
        a: comp[2].parse().unwrap(),
        s: comp[3].parse().unwrap(),
    }
}

fn psum(p: &Part) -> usize {
    p.x + p.m + p.a + p.s
}
pub fn part01(data: String) -> usize {
    let (workflow, parts) = data.split_once("\n\n").unwrap();
    let parts: Vec<Part> = parts.lines().map(parse_part).collect();
    let workflows: MM<String, Vec<Test>> = MM::from_iter(workflow.lines().map(parse_workflow));
    parts
        .iter()
        .filter(|p| process_part(p, &workflows) == Test::Accepted)
        .map(psum)
        .sum()
}

//PART 2

fn cutrange(
    (f, t): (usize, usize),
    attr: char,
    v: usize,
) -> (Option<(usize, usize)>, Option<(usize, usize)>) {
    match attr {
        '<' => {
            if v <= f {
                (None, Some((f, t)))
            } else {
                let rhs = min(t, v - 1);
                (
                    Some((f, rhs)),
                    if rhs == t { None } else { Some((rhs + 1, t)) },
                )
            }
        }
        _ => {
            if v >= t {
                (None, Some((f, t)))
            } else {
                let lhs = max(f, v + 1);
                (
                    Some((lhs, t)),
                    if lhs == f { None } else { Some((f, lhs - 1)) },
                )
            }
        }
    }
}

fn nr(pr: &PartRange, attr: char, r: (usize, usize)) -> PartRange {
    let mut nr: PartRange = *pr;
    match attr {
        'x' => nr.x = r,
        'm' => nr.m = r,
        'a' => nr.a = r,
        's' => nr.s = r,
        _ => panic!("wrong attr"),
    };
    nr
}
fn splitrange(
    pr: &PartRange,
    attr: char,
    cmp: char,
    v: usize,
) -> (Option<PartRange>, Option<PartRange>) {
    let r = match attr {
        'x' => pr.x,
        'm' => pr.m,
        'a' => pr.a,
        's' => pr.s,
        _ => panic!("wrong attr"),
    };
    let (cut1, cut2) = cutrange(r, cmp, v);
    let r1 = match cut1 {
        None => None,
        Some(r) => Some(nr(pr, attr, r)),
    };
    let r2 = match cut2 {
        None => None,
        Some(r) => Some(nr(pr, attr, r)),
    };
    (r1, r2)
}

fn get_list(workflows: &MM<String, Vec<Test>>, r: PartRange, workflow: &[Test]) -> Vec<PartRange> {
    let test = &workflow[0];
    let mut p1: Vec<PartRange>;
    let mut p2: Vec<PartRange>;

    match test {
        Test::Accepted => {
            let mut v = Vec::new();
            v.push(r);
            return v;
        }
        Test::Rejected => {
            return vec![];
        }
        Test::Goto(wsf) => return get_list(workflows, r, workflows.get(wsf).unwrap()),
        Test::GotoIf((attr, cmp, v, test2)) => {
            let (r, rest) = splitrange(&r, *attr, *cmp, *v);
            p1 = match r {
                None => vec![],
                Some(r1) => get_list(workflows, r1, slice::from_ref(test2)),
            };
            p2 = match rest {
                None => vec![],
                Some(r2) => get_list(workflows, r2, &workflow[1..]),
            };
        }
    }

    p1.append(&mut p2);
    p1
}
pub fn part02(data: String) -> usize {
    let (workflow, parts) = data.split_once("\n\n").unwrap();
    let _parts: Vec<Part> = parts.lines().map(parse_part).collect();
    let workflows: MM<String, Vec<Test>> = MM::from_iter(workflow.lines().map(parse_workflow));
    let l = get_list(&workflows, START_PR, workflows.get("in").unwrap());
    // println!("{:?}",l);
    // println!("{}",l.len());
    let s: usize = l.iter().map(rangesize).sum();
    s
}
fn rangesize(p: &PartRange) -> usize {
    (p.x.1 - p.x.0 + 1) * (p.m.1 - p.m.0 + 1) * (p.a.1 - p.a.0 + 1) * (p.s.1 - p.s.0 + 1)
}

#[cfg(test)]
mod test_parse {
    use super::*;
    #[test]
    fn boo() {
        println!("{:?}", parse_workflow("pv{a>1716:R,A}"));
        println!("{:?}", parse_workflow("qs{s>3448:A,lnx}"));
    }
    #[test]
    fn parse_part_test() {
        println!("{:?}", parse_part("{x=787,m=2655,a=1222,s=2876}"));
    }
}

#[cfg(test)]
mod test_get_list {
    use super::*;

    #[test]
    fn boo() {
        let ws = MM::from_iter(_WORKFLOWS.lines().map(parse_workflow));
        println!(
            "{:?}",
            get_list(&ws, START_PR, &parse_workflow("in{s<1351:px,qqz}").1)
        );
    }
}

#[cfg(test)]
mod test_cut {
    use super::*;
    #[test]
    fn cutrange_test() {
        assert_eq!(cutrange((1, 5), '<', 5), (Some((1, 4)), Some((5, 5))));
        assert_eq!(cutrange((1, 5), '<', 1), (None, Some((1, 5))));
        assert_eq!(cutrange((1, 5), '<', 6), (Some((1, 5)), None));
        assert_eq!(cutrange((1, 5), '>', 5), (None, Some((1, 5))));
        assert_eq!(cutrange((1, 5), '>', 1), (Some((2, 5)), Some((1, 1))));
        assert_eq!(cutrange((1, 5), '>', 6), (None, Some((1, 5))));
    }
    #[test]
    fn splitrange_test() {
        println!(
            "{:?}",
            splitrange(
                &PartRange {
                    x: (8, 10),
                    m: (0, 10),
                    a: (20, 30),
                    s: (40, 50)
                },
                'x',
                '<',
                50
            )
        )
    }
}

// r > A = [r]
// r > R = []
// r > (x <> n A) = [r^] +  remainder
// r > (x <> n R) = [] +  remainder
// r > f = f(r)
// (x <> n f ) = f(r^) + remainder

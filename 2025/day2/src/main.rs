use std::{fs::read_to_string};
use fancy_regex::Regex;

fn load_input_file(file_name: &str) -> Vec<String> {
    read_to_string(file_name)
        .unwrap()
        .split(",")
        .map(String::from)
        .collect()
}

fn main() {
    let input = load_input_file("input.txt");
    let sum1 = part1(&input);
    let sum2 = part2(&input);

    println!("Result part 1: {sum1}");
    println!("Result part 2: {sum2}");
}

fn part1(input: &[String]) -> i64 {
    let mut invalids = 0;
    for range in input {
        let (fr, to): (i64, i64) = match range.trim().split_at(range.find("-").unwrap()) {
            (_, "") => continue,
            (f, t) => (f.parse().unwrap(), -t.parse::<i64>().unwrap())
        };

        for n in fr..(to+1) {
            let nn = n.to_string();
            if nn.len() % 2 == 1 { continue }
            let (l, r) = nn.split_at(nn.len()/2);
            if l == r {
                invalids += n;
            }
        }
    }
    invalids
}

fn part2(input: &[String]) -> i64 {
    let rx = Regex::new("^(\\d+)\\1+$").unwrap();
    let mut invalids = 0;
    for range in input {
        let (fr, to): (i64, i64) = match range.trim().split_at(range.find("-").unwrap()) {
            (_, "") => continue,
            (f, t) => (f.parse().unwrap(), -t.parse::<i64>().unwrap())
        };

        for n in fr..(to+1) {
            let nn = n.to_string();
            if rx.is_match(&nn).unwrap() {
                invalids += n;
            }
        }
    }
    invalids
}
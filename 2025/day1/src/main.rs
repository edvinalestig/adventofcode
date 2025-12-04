use std::{fs::read_to_string};

fn load_input_file(file_name: &str) -> Vec<String> {
    read_to_string(file_name)
        .unwrap()
        .lines()
        .map(String::from)
        .collect()
}

fn main() {
    let input = load_input_file("input.txt");
    let hits1 = part1(&input);
    let hits2 = part2(&input);

    println!("Result part 1: {hits1}");
    println!("Result part 2: {hits2}");
}

fn part1(input: &Vec<String>) -> i16 {
    let mut hits: i16 = 0;
    let mut pos: i16 = 50;
    for line in input {
        match line.split_at(1) {
            ("R", n) => pos = (pos + n.parse::<i16>().unwrap()) % 100,
            ("L", n) => pos = (pos - n.parse::<i16>().unwrap()) % 100,
            _ => ()
        };
        if pos == 0 {
            hits += 1;
        }
    }
    hits
}

fn part2(input: &Vec<String>) -> i16 {
    let mut hits: i16 = 0;
    let mut pos: i16 = 50;
    for line in input {
        let forward: bool = line.starts_with("R");
        let new_pos: i16 = match line.split_at(1) {
            ("R", n) => pos + n.parse::<i16>().unwrap(),
            ("L", n) => pos - n.parse::<i16>().unwrap(),
            _ => panic!()
        };
        if forward {
            if new_pos >= 100 {
                hits += new_pos / 100;
            }
            pos = new_pos % 100;
        } else {
            if pos == 0 && new_pos <= -100 {
                hits += new_pos.abs() / 100;
            } else if pos != 0 && new_pos <= 0  {
                hits += new_pos.abs() / 100 + 1;
            }
            pos = new_pos.rem_euclid(100);
        }
    }
    hits
}

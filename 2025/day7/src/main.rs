use std::{collections::HashMap, fs::read_to_string};

fn load_input_file(file_name: &str) -> Vec<Vec<char>> {
    read_to_string(file_name)
        .unwrap()
        .lines()
        .map(|l| l.chars().collect())
        .collect()
}

fn main() {
    let input = load_input_file("input.txt");
    let result1 = part1(&input);
    println!("Result part 1: {result1}");
    let result2 = part2(&input);
    println!("Result part 2: {result2}");
}

fn part1(input: &[Vec<char>]) -> i64 {
    let mut times_split: i64 = 0;
    let mut beams: Vec<bool> = input[0].iter().map(|c| *c == 'S').collect();
    for row in input[1..].iter() {
        let mut new_beams: Vec<bool> = beams.clone();
        for i in 0..row.len() {
            match row[i] {
                '.' => continue,
                '^' => if beams[i] {
                    if let Some(x) = new_beams.get_mut(i-1) { *x = true; }
                    if let Some(x) = new_beams.get_mut(i+1) { *x = true; }
                    new_beams[i] = false;
                    times_split += 1;
                },
                _ => panic!()
            }
        }
        beams = new_beams;
    }
    times_split
}

fn part2(input: &[Vec<char>]) -> i64 {
    let mut cache: HashMap<(usize,usize), i64> = HashMap::new();
    let beam = input[0].iter().position(|c| *c == 'S').unwrap();
    part2_rec(input, &mut cache, 1, beam)
}

fn part2_rec(input: &[Vec<char>], cache: &mut HashMap<(usize,usize),i64>, row: usize, beam: usize) -> i64 {
    if row >= input.len() {
        1
    } else if input[row][beam] == '^' {
        if let Some(x) = cache.get(&(row,beam)) {
            *x
        } else {
            let splits: i64 = part2_rec(input, cache, row+1, beam-1) + part2_rec(input, cache, row+1, beam+1);
            cache.insert((row,beam), splits);
            splits
        }
    } else {
        part2_rec(input, cache, row+1, beam)
    }
}

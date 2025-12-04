use std::fs::read_to_string;

fn load_input_file(file_name: &str) -> Vec<String> {
    read_to_string(file_name)
        .unwrap()
        .lines()
        .map(String::from)
        .collect()
}

fn main() {
    let input = load_input_file("input.txt");
    let p1 = part1(&input);
    println!("Result part 1: {p1}");
    let p2 = part2(&input);
    println!("Result part 2: {p2}");
}

fn part1(input: &Vec<String>) -> i64 {
    let mut sum: i64 = 0;
    for row in input {
        let max = row.chars().take(row.len()-1).max().unwrap();
        let index = row.find(max).unwrap();
        let next_max = row.split_at(index+1).1.chars().max().unwrap();
        sum += (max.to_digit(10).unwrap() as i64) * 10 + (next_max.to_digit(10).unwrap() as i64)
    }
    sum
}

fn part2(input: &Vec<String>) -> i64 {
    let mut sum: i64 = 0;
    for row in input {
        let mut maxs: Vec<i64> = vec![];
        let mut index = 0;
        for i in 0..12 {
            let right_section = row.split_at(index).1;
            let middle_section: String = right_section.chars().take(right_section.len()-(11-i)).collect();
            let max = middle_section.chars().max().unwrap();
            maxs.push(max.to_digit(10).unwrap() as i64);
            index += middle_section.find(max).unwrap()+1;
        }
        sum += maxs.iter().fold(0, |acc, v| acc*10 + v);
    }
    sum
}

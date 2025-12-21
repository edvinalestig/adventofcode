use std::fs::read_to_string;

fn load_input_file_part1(file_name: &str) -> Vec<String> {
    read_to_string(file_name)
        .unwrap()
        .trim()
        .lines()
        .map(String::from)
        .collect()
}

fn load_input_file_part2(file_name: &str) -> Vec<Vec<char>> {
    read_to_string(file_name)
        .unwrap()
        .lines()
        .map(|l| l.chars().collect())
        .collect()
}

fn main() {
    let input1 = load_input_file_part1("input.txt");
    let result1 = part1(&input1);
    println!("Result part 1: {result1}");
    let input2 = load_input_file_part2("input.txt");
    let result2 = part2(&input2);
    println!("Result part 2: {result2}");
}

fn part1(input: &[String]) -> i64 {
    let problem: Vec<Vec<String>> = input.iter().map(|l| l.split_whitespace().map(String::from).collect()).collect();
    let mut sum: i64 = 0;
    for i in 0..problem[0].len() {
        if problem.last().unwrap()[i] == "*" {
            sum += problem[..problem.len()-1].iter().fold(1, |acc, x| acc * x[i].parse::<i64>().unwrap());
        } else {
            sum += problem[..problem.len()-1].iter().fold(0, |acc, x| acc + x[i].parse::<i64>().unwrap());
        }
    }
    sum
}

fn part2(input: &[Vec<char>]) -> i64 {
    let mut total: i64 = 0;
    let mut problem_total: Vec<i64> = Vec::new();
    for i in (0..input[0].len()).rev() {
        let mut column: i64 = 0;
        for row in input {
            match row[i] {
                ' ' => continue,
                '*' => {
                    problem_total.push(column);
                    total += problem_total.iter().product::<i64>();
                },
                '+' => {
                    problem_total.push(column);
                    total += problem_total.iter().sum::<i64>();
                },
                d => column = column * 10 + d.to_digit(10).unwrap() as i64
            }
        }
        if column == 0 {
            problem_total = Vec::new();
        } else  {
            problem_total.push(column);
        }
    }
    total
}

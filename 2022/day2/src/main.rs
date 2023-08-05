use std::fs;
use std::i64;

fn main() {
    part1();
    part2();
}

fn part1() {
    let inputs: String = fs::read_to_string("input.txt").unwrap();
    let mut sum: i64 = 0;

    for strat in inputs.lines() {
        let (op, my) = strat.split_once(" ").unwrap();
        sum += i64::from(my.as_bytes()[0]) - 87; // Ascii :)
        sum += match op {
            "A" => match my {
                "X" => 3,
                "Y" => 6,
                "Z" => 0,
                c   => panic!("Unexpected input: A+{c}")
            },
            "B" => match my {
                "X" => 0,
                "Y" => 3,
                "Z" => 6,
                c   => panic!("Unexpected input: B+{c}")
            },
            "C" => match my {
                "X" => 6,
                "Y" => 0,
                "Z" => 3,
                c   => panic!("Unexpected input: C+{c}")
            },
            c   => panic!("Unexpected input: {c}")
        };
    }

    println!("Sum (part 1): {sum}");
}

fn part2() {
    let inputs: String = fs::read_to_string("input.txt").unwrap();
    let mut sum: i64 = 0;

    for strat in inputs.lines() {
        let (op, goal) = strat.split_once(" ").unwrap();
        sum += match op {
            "A" => match goal { // Rock
                "Z" => 6+2,     // Win,  paper
                "Y" => 3+1,     // Draw, rock
                "X" => 3,       // Lose, scissors
                c   => panic!("Unexpected input: A+{c}")
            },
            "B" => match goal { // Paper
                "Z" => 6+3,     // Win,  scissors
                "Y" => 3+2,     // Draw, paper
                "X" => 1,       // Lose, rock
                c   => panic!("Unexpected input: B+{c}")
            },
            "C" => match goal { // Scissors
                "Z" => 6+1,     // Win,  rock
                "Y" => 3+3,     // Draw, scissors
                "X" => 2,       // Lose, paper
                c   => panic!("Unexpected input: C+{c}")
            },
            c   => panic!("Unexpected input: {c}")
        };
    }
    println!("Sum (part 2): {sum}");
}

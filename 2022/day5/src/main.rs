use std::fs;
use regex::Regex;

fn main() {
    part1();
    part2();
}

fn parse_input() -> (Vec<Vec<char>>, Vec<(i32, i32, i32)>) {
    let inputs: String = fs::read_to_string("input.txt").unwrap();
    let (crates, instructions) = inputs.split_once("\n\n").unwrap();

    // Not original input format. Each stack gets one line each.
    // Parsing the original would be way too much work.
    let mut stacks: Vec<Vec<char>> = vec![];
    for stack in crates.lines() {
        let chars: Vec<char> = stack.chars().collect();
        stacks.push(chars);
    }

    // Extract the values from the instruction list
    let mut instrs: Vec<(i32, i32, i32)> = vec![];
    for instruction in instructions.lines() {
        let re: Regex = Regex::new(r"move (\w+) from (\w+) to (\w+)").unwrap();
        let caps = re.captures(instruction).unwrap();
        let numcrates: &str = &caps[1];
        let from: &str = &caps[2];
        let to: &str = &caps[3];

        // (num crates to move, index from, index to)
        instrs.push((numcrates.parse::<i32>().unwrap(), from.parse::<i32>().unwrap()-1, to.parse::<i32>().unwrap()-1));
    }

    return (stacks, instrs)
}

fn part1() {
    let (mut stacks, instrs) = parse_input();

    // Execute the instructions
    for (num, from, to) in instrs {
        for _ in 0..num {
            let temp: char = stacks[from as usize].pop().unwrap();
            stacks[to as usize].push(temp);
        }
    }
    
    println!("Result (part 1):");
    for stack in stacks {
        print!("{}", stack.last().unwrap());
    }
    println!("");
}

fn part2() {
    let (mut stacks, instrs) = parse_input();
    
    // Execute the instructions
    for (num, from, to) in instrs {
        let split_point: usize = stacks[from as usize].len() - num as usize;
        let mut temp: Vec<char> = stacks[from as usize].split_off(split_point);
        stacks[to as usize].append(&mut temp);
    }

    println!("Result (part 2):");
    for stack in stacks {
        print!("{}", stack.last().unwrap());
    }
    println!("");
}

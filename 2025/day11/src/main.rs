use std::{collections::HashMap, fs::read_to_string};

fn load_input_file(file_name: &str) -> HashMap<String, Vec<String>> {
    read_to_string(file_name)
        .unwrap()
        .lines()
        .map(|s| s.split_once(": ").unwrap())
        .map(|(k, v)| (k.to_string(), v.split_whitespace().map(|item| item.to_string()).collect()))
        .collect()
}

fn main() {
    let input = load_input_file("input.txt");
    let result1 = part1(&input);
    println!("Result part 1: {result1}");
    let result2 = part2(&input);
    println!("Result part 2: {result2}");
}

fn part1(input: &HashMap<String, Vec<String>>) -> i64 {
    recurse1(input, &"you".to_string())
}

fn recurse1(input: &HashMap<String, Vec<String>>, current_state: &String) -> i64 {
    if current_state == "out" {
        1
    } else {
        let next_states = input.get(current_state).unwrap();
        next_states.iter().map(|state| recurse1(input, state)).sum()
    }
}

fn part2(input: &HashMap<String, Vec<String>>) -> i64 {
    let svr = "svr".to_string();
    let fft = "fft".to_string();
    let dac = "dac".to_string();
    let out = "out".to_string();
    
    let svr_fft = start_recurse2(input, &svr, &fft);
    let fft_dac = start_recurse2(input, &fft, &dac);
    let dac_out = start_recurse2(input, &dac, &out);

    let svr_dac = start_recurse2(input, &svr, &dac);
    let dac_fft = start_recurse2(input, &dac, &fft);
    let fft_out = start_recurse2(input, &fft, &out);

    svr_fft*fft_dac*dac_out + svr_dac*dac_fft*fft_out
}

fn start_recurse2(input: &HashMap<String, Vec<String>>, current_state: &String, target: &String) -> i64 {
    let mut visited: HashMap<String, i64> = HashMap::new();
    recurse2(input, &mut visited, current_state, target)
}

fn recurse2(input: &HashMap<String, Vec<String>>, visited: &mut HashMap<String,i64>, current_state: &String, target: &String) -> i64 {
    if current_state == target {
        1
    } else if current_state == "out" {
        0
    } else if let Some(&n) = visited.get(current_state) {
        n
    } else {
        let sum = input
            .get(current_state)
            .unwrap()
            .iter()
            .filter(|state| state == &target || !["fft", "dac"].contains(&state.as_str()))
            .map(|state| recurse2(input, visited, state, target))
            .sum();
        visited.insert(current_state.clone(), sum);
        sum
    }
}
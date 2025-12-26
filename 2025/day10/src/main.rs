use std::{collections::{HashSet, VecDeque}, fs::read_to_string};
use good_lp::{Constraint, Expression, ProblemVariables, Solution, SolverModel, Variable, default_solver, variable};
use regex::Regex;

fn load_input_file(file_name: &str) -> Vec<MachineInstruction> {
    read_to_string(file_name)
        .unwrap()
        .lines()
        .map(MachineInstruction::parse)
        .collect()
}

struct MachineInstruction {
    goal: Vec<bool>,
    buttons: Vec<Vec<usize>>,
    joltages: Vec<i32>
}

impl MachineInstruction {
    fn parse(input: &str) -> MachineInstruction {
        let goal_regex = Regex::new("\\[([\\.#]+)\\]").unwrap();
        let goal: Vec<bool> = goal_regex
            .captures(input)
            .unwrap()
            .get(1)
            .unwrap()
            .as_str()
            .chars()
            .map(|c| c == '#')
            .collect();
        let buttons_regex = Regex::new("\\(([\\d,]+)\\)").unwrap();
        let buttons: Vec<Vec<usize>> = buttons_regex
            .captures_iter(input)
            .map(|cap| cap.extract().1)
            .map(|cap: [&str; 1]| cap[0]
                .split(",")
                .map(|d| d
                    .parse::<usize>()
                    .unwrap())
                .collect())
            .collect();
        let joltages_regex = Regex::new("\\{([\\d,]+)\\}").unwrap();
        let joltages: Vec<i32> = joltages_regex
            .captures(input)
            .unwrap()
            .get(1)
            .unwrap()
            .as_str()
            .split(",")
            .map(|d| d.parse::<i32>().unwrap())
            .collect();
        MachineInstruction { goal, buttons, joltages }
    }
}

fn main() {
    let input = load_input_file("input.txt");
    let result1 = part1(&input);
    println!("Result part 1: {result1}");
    let result2 = part2(&input);
    println!("Result part 2: {result2}");
}

fn part1(input: &[MachineInstruction]) -> i64 {
    let mut total_button_presses = 0;
    for machine in input {
        let init_state: (Vec<bool>, i64) = (vec![false; machine.goal.len()], 0);
        let mut reached_states: HashSet<Vec<bool>> = HashSet::from([init_state.0.clone()]);
        let mut queue: VecDeque<(Vec<bool>, i64)> = VecDeque::from([init_state]);

        while let Some((state, n)) = queue.pop_front() {
            for button in &machine.buttons {
                let mut new_state = state.clone();
                button.iter().for_each(|&i| new_state[i] = !new_state[i]);

                if state == machine.goal {
                    total_button_presses += n;
                    queue.clear();
                    break;
                }
                
                if !reached_states.contains(&new_state) {
                    reached_states.insert(new_state.clone());
                    queue.push_back((new_state, n+1));
                }
            }
        }
    }
    total_button_presses
}

fn part2(input: &[MachineInstruction]) -> i32 {
    let mut total_button_presses: i32 = 0;
    for machine in input {
        // Objective: minimise sum of variables (button presses for each button)
        let mut variables = ProblemVariables::new();
        let vars: Vec<Variable> = machine.buttons
        .iter()
        .map(|_| variables.add(variable().integer().min(0)))
        .collect();
        let objective: Expression = vars.iter().sum();

        // Constraints
        // The sum of button presses for all buttons acting on a joltage
        // must be equal to the required joltage.
        let mut constraints: Vec<Constraint> = Vec::new();
        for (i, &joltage) in machine.joltages.iter().enumerate() {
            let mut exp: Expression = 0.into();
            for (j, b) in machine.buttons.iter().enumerate() {
                if b.contains(&i) {
                    exp += vars[j];
                }
            }
            constraints.push(exp.eq(joltage));
        }

        let solution = variables
            .minimise(objective)
            .using(default_solver)
            .with_all(constraints)
            .solve()
            .unwrap();

        let button_presses = vars.iter().fold(0, |acc, &v| acc + solution.value(v) as i32);

        total_button_presses += button_presses;
    }
    total_button_presses
}

use std::fs;
use std::str;
use std::vec;

fn main() {
    let inputs: String = fs::read_to_string("input.txt").unwrap();
    let mut cals: Vec<i64> = vec![];

    let elves = str::split(&inputs, "\n\n");
    for elf in elves {
        let calories = str::split(&elf, "\n");
        let mut sum: i64 = 0;
        for cal in calories {
            sum += match str::parse::<i64>(&cal) {
                Ok(s) => s,
                Err(_) => 0
            };
        }
        cals.push(sum);
    }
    cals.sort();
    cals.reverse();
    let top3: i64 = cals[0] + cals[1] + cals[2];

    println!("Part 1 (most calories): {:?}", cals[0]);
    println!("Part 2 (top 3): {top3}");
}

use std::fs;

fn main() {
    let inputs: String = fs::read_to_string("input.txt").unwrap();
    let mut cals: Vec<i64> = vec![];

    let elves = inputs.split("\n\n");
    for elf in elves {
        let calories = elf.lines();
        let mut sum: i64 = 0;
        for cal in calories {
            sum += match cal.parse::<i64>() {
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

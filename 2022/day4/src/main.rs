use std::fs;

fn main() {
    let inputs: String = fs::read_to_string("input.txt").unwrap();
    let mut contains = 0;
    let mut overlapping = 0;

    for pair in inputs.lines() {
        let (elf1, elf2) = pair.split_once(",").unwrap();
        let (elf1lows, elf1highs) = elf1.split_once("-").unwrap();
        let (elf2lows, elf2highs) = elf2.split_once("-").unwrap();
        let (elf1low, elf1high) = (elf1lows.parse::<i64>().unwrap(), elf1highs.parse::<i64>().unwrap());
        let (elf2low, elf2high) = (elf2lows.parse::<i64>().unwrap(), elf2highs.parse::<i64>().unwrap());

        // Check if one range contains the other
        if elf1low <= elf2low && elf1high >= elf2high {
            contains += 1;
        } else if elf2low <= elf1low && elf2high >= elf1high {
            contains += 1;
        }

        // Check if there is any overlap at all
        if !(elf1low > elf2high || elf2low > elf1high) {
            overlapping += 1
        }
    }

    println!("Contains (part 1): {contains}");
    println!("Overlapping (part 2): {overlapping}");
}

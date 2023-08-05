use std::fs;

fn main() {
    part1();
    part2();
}

fn translate(c: char) -> u64 {
    // Translate a char to a u64 with a 1 at index 1-52
    // corresponding to its "priority".
    let value: u32;
    if c.is_ascii_lowercase() {
        value = c as u32 - 96; // Range: 1-26
    } else {
        value = c as u32 - 38; // Range: 27-52
    };
    return 1 << value;
}

fn part1() {
    let inputs: String = fs::read_to_string("input.txt").unwrap();
    let mut sum = 0;
    
    for contents in inputs.lines() {
        let n = contents.len();
        let (lefts, rights) = contents.split_at(n/2);
        // Left and right partitions
        let mut left: u64 = 0;
        let mut right: u64 = 0;
        for c in lefts.chars() {
            left |= translate(c);
        }
        for c in rights.chars() {
            right |= translate(c);
        }
        // `left` and `right` contain a 1 at each index of contained items
        // A bitwise AND gives the duplicate and log2 gives the index (priority value).
        sum += (left & right).ilog2();
    }

    println!("Sum: (part 1) {sum}");
}

fn part2() {
    let inputs: String = fs::read_to_string("input.txt").unwrap();
    let mut sum = 0;

    let mut elves: Vec<u64> = vec![];
    // Store all u64s with contained items in a vector
    for contents in inputs.lines() {
        let mut value: u64 = 0;
        for c in contents.chars() {
            value |= translate(c);
        }
        elves.push(value);
    }

    while elves.len() > 0 {
        // Get each triplet of elves and find the shared item
        let shared: u64 = elves.pop().unwrap() & elves.pop().unwrap() & elves.pop().unwrap();
        sum += shared.ilog2();
    }

    println!("Sum: (part 2) {sum}");
}

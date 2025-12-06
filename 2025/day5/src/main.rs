use std::fs::read_to_string;

fn load_input_file(file_name: &str) -> (Vec<(i64,i64)>, Vec<i64>) {
    read_to_string(file_name)
        .unwrap()
        .trim()
        .split_once("\n\n")
        .map(|(r,v)|
            (
                r
                .lines()
                .map(|rr| 
                    rr
                    .split_once("-")
                    .map(|(r1,r2)| (r1.parse().unwrap(), r2.parse().unwrap()))
                    .unwrap()
                )
                .collect(),
                v
                .lines()
                .map(str::parse)
                .map(Result::unwrap)
                .collect(),
            )
        )
        .unwrap()
}

fn main() {
    let input = load_input_file("input.txt");
    let result1 = part1(&input);
    println!("Result part 1: {result1}");
    let result2 = part2(&input.0);
    println!("Result part 2: {result2}");
}

fn part1(input: &(Vec<(i64,i64)>, Vec<i64>)) -> i64 {
    let (ranges, ingredients) = input;
    let mut fresh = 0;

    for ingredient in ingredients {
        if ranges.iter().any(|r| in_range(r, *ingredient)) {
            fresh += 1;
        }
    }
    fresh
}

fn in_range(range: &(i64,i64), ingredient: i64) -> bool {
    ingredient >= range.0 && ingredient <= range.1
}

fn part2(input: &[(i64,i64)]) -> i64 {
    let mut ranges: Vec<(i64,i64)> = vec![];
    for input_range in input {
        let mut colliding_ranges: Vec<(i64,i64)> = ranges
            .extract_if(.., |r| is_colliding(r, input_range))
            .collect();
        colliding_ranges.push(*input_range);
        ranges.push(combine_ranges(&colliding_ranges))
    }
    ranges.iter().fold(0, |acc, (r1,r2)| acc + r2 - r1 + 1)
}

fn is_colliding(r1: &(i64,i64), r2: &(i64,i64)) -> bool {
    (r1.0 <= r2.0 && r2.0 <= r1.1) || (r1.0 <= r2.1 && r2.1 <= r1.1) ||
    (r2.0 <= r1.0 && r1.0 <= r2.1) || (r2.0 <= r1.1 && r1.1 <= r2.1)
}

fn combine_ranges(ranges: &[(i64,i64)]) -> (i64,i64) {
    (ranges.iter().min_by_key(|x| x.0).unwrap().0,
    ranges.iter().max_by_key(|x| x.1).unwrap().1)
}

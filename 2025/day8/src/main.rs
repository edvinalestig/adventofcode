use std::fs::read_to_string;

#[derive(PartialEq, Eq)]
struct JunctionBox {
    x: i64,
    y: i64,
    z: i64
}

impl JunctionBox {
    fn parse(s: &str) -> JunctionBox {
        let coords: Vec<i64> = s.split(",").map(|s| s.parse::<i64>().unwrap()).collect();
        JunctionBox { x: coords[0], y: coords[1], z: coords[2] }
    }
}

fn load_input_file(file_name: &str) -> Vec<JunctionBox> {
    read_to_string(file_name)
        .unwrap()
        .lines()
        .map(JunctionBox::parse)
        .collect()
}

fn main() {
    let input = load_input_file("input.txt");
    let result1 = part1(&input);
    println!("Result part 1: {result1}");
    let result2 = part2(&input);
    println!("Result part 2: {result2}");
}

fn distance(j1: &JunctionBox, j2: &JunctionBox) -> f64 {
    f64::sqrt(((j2.x-j1.x).pow(2) + (j2.y-j1.y).pow(2) + (j2.z-j1.z).pow(2)) as f64)
}

fn junction_box_pairs_sorted_by_distance(input: &[JunctionBox]) -> Vec<(&JunctionBox, &JunctionBox, f64)> {
    let mut distances: Vec<(&JunctionBox, &JunctionBox, f64)> = Vec::new();
    for i in 0..input.len() {
        for j in (i+1)..input.len() {
            if i == j { continue }
            distances.push((&input[i], &input[j], distance(&input[i], &input[j])));
        }
    }
    // Need to multiply by large number because sort cannot operate on f64, only integers
    distances.sort_by_key(|d| (d.2 * 1_000_000.0) as i64);
    distances
}

fn part1(input: &[JunctionBox]) -> i64 {
    let distances = junction_box_pairs_sorted_by_distance(input);
    let mut circuits: Vec<Vec<&JunctionBox>> = Vec::new();
    for (j1, j2, _) in distances.iter().take(1000) {
        let i1 = circuits.iter().position(|c| c.contains(j1));
        let i2 = circuits.iter().position(|c| c.contains(j2));
        match (i1,i2) {
            (None, None) => circuits.push(vec![j1, j2]),
            (Some(x), None) => circuits.get_mut(x).unwrap().push(j2),
            (None, Some(y)) => circuits.get_mut(y).unwrap().push(j1),
            (Some(x), Some(y)) if x == y => continue,
            (Some(x), Some(y)) => {
                // Merge the circuits. Always remove the higher index.
                let other = circuits.remove(if x < y { y } else { x });
                circuits.get_mut(if x < y { x } else { y }).unwrap().extend(other);
            }
        }
    }
    let mut lengths: Vec<i64> = circuits.iter().map(|c| c.len() as i64).collect();
    lengths.sort();
    lengths.reverse();
    lengths.iter().take(3).product()
}

fn part2(input: &[JunctionBox]) -> i64 {
    let distances = junction_box_pairs_sorted_by_distance(input);
    let mut circuits: Vec<Vec<&JunctionBox>> = Vec::new();
    for (j1, j2, _) in distances {
        let i1 = circuits.iter().position(|c| c.contains(&j1));
        let i2 = circuits.iter().position(|c| c.contains(&j2));
        match (i1,i2) {
            (None, None) => circuits.push(vec![j1, j2]),
            (Some(x), None) => circuits.get_mut(x).unwrap().push(j2),
            (None, Some(y)) => circuits.get_mut(y).unwrap().push(j1),
            (Some(x), Some(y)) if x == y => continue,
            (Some(x), Some(y)) => {
                // Merge the circuits. Always remove the higher index.
                let other = circuits.remove(if x < y { y } else { x });
                circuits.get_mut(if x < y { x } else { y }).unwrap().extend(other);
            }
        }
        if circuits.len() == 1 && circuits[0].len() == input.len() {
            return j1.x * j2.x;
        }
    }
    unreachable!()
}

use std::fs::read_to_string;

#[derive(Copy, Clone)]
struct Coord {
    x: i64,
    y: i64
}

impl Coord {
    fn parse(s: &str) -> Coord {
        let coords: Vec<i64> = s.split(",").map(|s| s.parse::<i64>().unwrap()).collect();
        Coord { x: coords[0], y: coords[1] }
    }
}

fn load_input_file(file_name: &str) -> Vec<Coord> {
    read_to_string(file_name)
        .unwrap()
        .lines()
        .map(Coord::parse)
        .collect()
}

fn main() {
    let input = load_input_file("input.txt");
    let result1 = part1(&input);
    println!("Result part 1: {result1}");
    let result2 = part2(&input);
    println!("Result part 2: {result2}");
}

fn part1(input: &[Coord]) -> i64 {
    let mut max_area = 0;
    for i in 0..input.len() {
        for j in (i+1)..input.len() {
            max_area = max_area.max(area(input[i], input[j]))
        } 
    }
    max_area
}

fn area(c1: Coord, c2: Coord) -> i64 {
    ((c2.x - c1.x).abs() + 1) * ((c2.y - c1.y).abs() + 1)
}

#[derive(Clone, Copy)]
enum Border {
    Horizontal(i64, i64, i64),
    Vertical(i64, i64, i64)
}

impl Border {
    fn create(c1: &Coord, c2: &Coord) -> Border {
        if c1.x == c2.x {
            Self::Vertical(c1.x, c1.y.min(c2.y), c1.y.max(c2.y))
        } else {
            Self::Horizontal(c1.y, c1.x.min(c2.x), c1.x.max(c2.x))
        }
    }

    fn crosses(self, c1: &Coord, c2: &Coord) -> bool {
        let x_min = c1.x.min(c2.x);
        let x_max = c1.x.max(c2.x);
        let y_min = c1.y.min(c2.y);
        let y_max = c1.y.max(c2.y);
        match self {
            Self::Horizontal(by, bx1, bx2) => 
                if y_min < by && by < y_max && (
                    (x_min < bx1 && bx1 < x_max) || 
                    (x_min < bx2 && bx2 < x_max) || 
                    (bx1 <= x_min && x_max <= bx2)) {
                    return true
                },
            Self::Vertical(bx, by1, by2) => 
                if x_min < bx && bx < x_max && (
                    (y_min < by1 && by1 < y_max) || 
                    (y_min < by2 && by2 < y_max) ||
                    (by1 <= y_min && y_max <= by2)) {
                    return true
                }
        }
        false
    }
}

fn part2(input: &[Coord]) -> i64 {
    let mut borders: Vec<Border> = Vec::new();
    for i in 1..input.len() {
        borders.push(Border::create(&input[i-1], &input[i]));
    }
    borders.push(Border::create(input.last().unwrap(), &input[0]));

    let mut max_area = 0;
    for i in 0..input.len() {
        for j in (i+1)..input.len() {
            if borders.iter().any(|b| b.crosses(&input[i], &input[j])) {
                continue;
            }
            max_area = max_area.max(area(input[i], input[j]));
        } 
    }
    max_area
}

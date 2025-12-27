use std::fs::read_to_string;
use regex::Regex;

#[derive(Clone)]
struct Problem {
    shapes: Vec<Shape>,
    bins: Vec<Bin>
}

impl Problem {
    fn parse(input: String) -> Problem {
        let chunks = input.split("\n\n");
        let mut shapes: Vec<Shape> = Vec::new();
        for block in chunks.clone().take(6).map(str::lines) {
            let grid: Vec<Vec<bool>> = block
                .skip(1)
                .map(|l| l
                    .chars()
                    .map(|c| c == '#')
                    .collect())
                .collect::<Vec<Vec<bool>>>();
            shapes.push(Shape::new(grid));
        }
        let mut bins: Vec<Bin> = Vec::new();
        let bin_regex = Regex::new("(\\d+)x(\\d+): (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (\\d+)").unwrap();
        for bin in chunks.last().unwrap().lines() {
            let captures: [usize; 8]  = bin_regex.captures(bin).unwrap().extract().1.map(|m| m.parse().unwrap());
            bins.push(Bin {
                cols: captures[0],
                rows: captures[1],
                shapes: captures.into_iter().skip(2).collect()
            })
        }
        Problem {
            shapes,
            bins
        }
    }
}

#[allow(dead_code)]
#[derive(Clone)]
struct Shape {
    grid: Vec<Vec<bool>>, // Never used with this simplified solution
    area: usize,
}

impl Shape {
    fn new(grid: Vec<Vec<bool>>) -> Shape {
        let area = grid.iter().flatten().filter(|&&c| c).count();
        Shape { grid, area }
    }
}

#[derive(Clone, Debug)]
struct Bin {
    cols: usize,
    rows: usize,
    shapes: Vec<usize>
}

fn load_input_file(file_name: &str) -> Problem {
    Problem::parse(read_to_string(file_name).unwrap())
}

fn main() {
    let input = load_input_file("input.txt");
    let result1 = part1(input);
    println!("Result: {result1}");
}

fn part1(problem: Problem) -> i64 {
    let mut shapes_fit: i64 = 0;
    for bin in problem.bins {
        let bin_area = bin.cols * bin.rows;
        let shapes_combined_area = problem.shapes.iter().enumerate().fold(0, |acc,(i,s)| acc + s.area * bin.shapes[i]);
        if shapes_combined_area > bin_area {
            // Bin too small
            continue;
        }
        let shapes_area = bin.shapes.iter().fold(0, |acc, n| acc + 9*n);
        let bin_minimised_area = (bin.cols - (bin.cols % 3)) * (bin.rows - (bin.rows % 3));
        if shapes_area <= bin_minimised_area {
            // No interweaving of shapes required
            shapes_fit += 1;
            continue;
        }
        
        // When running with the real input, this is never reached
        unreachable!()
    }
    shapes_fit
}

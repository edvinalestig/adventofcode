use std::fs::read_to_string;

fn load_input_file(file_name: &str) -> Vec<String> {
    read_to_string(file_name)
        .unwrap()
        .trim()
        .lines()
        .map(String::from)
        .collect()
}

fn main() {
    let input = load_input_file("input.txt");
    let result1 = part1(&input);
    println!("Result part 1: {result1}");
    let result2 = part2(&input);
    println!("Result part 2: {result2}");
}

fn part1(input: &[String]) -> i32 {
    let grid: Vec<Vec<char>> = input.iter().map(|a| a.chars().collect()).collect();
    
    let mut accessible: i32 = 0;
    for x in 0..grid.len() {
        for y in 0..grid[x].len() {
            if grid[x][y] == '@' && is_accessible(&grid, x as i32, y as i32) {
                accessible += 1;
            }
        }
    }
    accessible
}

fn is_accessible(grid: &[Vec<char>], x: i32, y: i32) -> bool {
    let mut s = 0;
    for i in (-1)..2 {
        for j in (-1)..2 {
            if i == 0 && j == 0 { continue }
            s += get_index(grid, x+i, y+j);
        }
    }
    s < 4
}

fn get_index(grid: &[Vec<char>], x: i32, y: i32) -> i32 {
    if x < 0 || y < 0 { return 0; }
    grid.get(x as usize).map(|v| v.get(y as usize).map(|c| if *c == '@' { 1 } else { 0 }).unwrap_or(0)).unwrap_or(0)
}

fn part2(input: &[String]) -> i32 {
    let mut grid: Vec<Vec<char>> = input.iter().map(|a| a.chars().collect()).collect();
    let mut total_removed = 0;

    loop {
        let (removed, new_grid) = update_grid(&grid);
        total_removed += removed;
        grid = new_grid;
        if removed == 0 { break }
    }
    total_removed
}

fn update_grid(grid: &[Vec<char>]) -> (i32, Vec<Vec<char>>) {
    let mut new_grid = grid.to_vec();
    let mut removed = 0;

    for x in 0..grid.len() {
        for y in 0..grid[x].len() {
            if grid[x][y] == '@' && is_accessible(grid, x as i32, y as i32) {
                removed += 1;
                new_grid[x][y] = '.'
            }
        }
    }
    (removed, new_grid)
}
mod day1;
mod day2;

fn main() {
    println!("Day1 Part1: {}", day1::solve(day1::read_data()));
    println!("Day1 Part2: {}", day1::solve2(day1::read_data()));
    println!("Day2 Part1: {}", day2::solve(day2::read_data()));
    println!("Day2 Part2: {}", day2::solve2(day2::read_data()));
}

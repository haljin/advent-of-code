use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;

pub fn read_data() -> Vec<i64> {
    let mut f = File::open("./data/day1").expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("bla");

    let data: Vec<i64> = contents
        .split("\n")
        .into_iter()
        .map(|el| el.parse::<i64>().unwrap())
        .collect();
    data
}
pub fn solve(data: Vec<i64>) -> i64 {
    let result = data.iter().fold(0, |acc, el| acc + el);
    result
}

pub fn solve2(data: Vec<i64>) -> i64 {
    let mut cycling = data.iter().cycle();
    let mut seen = HashMap::new();
    let mut freq = 0;

    while let Some(val) = cycling.next() {
        seen.insert(freq, true);
        freq = freq + val;
        if let Some(_v) = seen.get(&freq) {
            return freq;
        }
        seen.insert(freq, true);
    }
    panic!("cannot find duplicate")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        assert_eq!(solve(vec![1, 1, 1]), 3);
        assert_eq!(solve(vec![1, 1, -2]), 0);
        assert_eq!(solve(vec![-1, -2, -3]), -6);
    }

    #[test]
    fn test_solve2() {
        assert_eq!(solve2(vec![1, -1]), 0);
        assert_eq!(solve2(vec![3, 3, 4, -2, -4]), 10);
        assert_eq!(solve2(vec![-6, 3, 8, 5, -6]), 5);
        assert_eq!(solve2(vec![7, 7, -2, -7, -4]), 14);
    }
}

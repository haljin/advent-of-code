use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;

pub fn read_data() -> Vec<String> {
    let mut f = File::open("./data/day2").unwrap();

    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();

    let data: Vec<String> = contents.split('\n').map(|e| e.to_owned()).collect();
    data
}

pub fn solve(data: Vec<String>) -> i64 {
    let mapped = data.iter().map(|id| parse_id(id));
    let mut double_count = 0;
    let mut triple_count = 0;

    for map in mapped {
        if map.values().any(|char_count| char_count == &2) {
            double_count += 1;
        }
        if map.values().any(|char_count| char_count == &3) {
            triple_count += 1;
        }
    }

    double_count * triple_count
}

fn parse_id(id: &String) -> HashMap<char, i64> {
    let mut map = HashMap::new();
    for ch in id.chars() {
        let new_val;
        match map.get(&ch) {
            Some(count) => {
                new_val = count + 1;
            }
            None => {
                new_val = 1;
            }
        }
        map.insert(ch, new_val);
    }
    map
}

pub fn solve2(data: Vec<String>) -> String {
    let mut ids = data.iter();

    while let Some(id1) = ids.next() {
        let mut rest = ids.clone();
        while let Some(id2) = rest.next() {
            if compare_ids(id1, id2) == 1 {
                let other_chars: String = id1
                    .chars()
                    .zip(id2.chars())
                    .filter_map(|(ch1, ch2)| if ch1 == ch2 { Some(ch1) } else { None })
                    .collect();
                return other_chars.to_owned();
            }
        }
    }
    "dupa".to_owned()
}

fn compare_ids(id1: &String, id2: &String) -> i64 {
    let mut chars1 = id1.chars();
    let mut chars2 = id2.chars();
    let mut diffs = 0;
    while let (Some(ch1), Some(ch2)) = (chars1.next(), chars2.next()) {
        if ch1 != ch2 {
            diffs += 1;
        }
    }
    diffs
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        assert_eq!(
            solve(vec![
                "abcdef".to_owned(),
                "bababc".to_owned(),
                "abbcde".to_owned(),
                "abcccd".to_owned(),
                "aabcdd".to_owned(),
                "abcdee".to_owned(),
                "ababab".to_owned()
            ]),
            12
        );
    }
    #[test]
    fn test_solve2() {
        assert_eq!(
            solve2(vec![
                "abcde".to_owned(),
                "fghij".to_owned(),
                "klmno".to_owned(),
                "pqrst".to_owned(),
                "fguij".to_owned(),
                "axcye".to_owned(),
                "wvxyz".to_owned()
            ]),
            "fgij"
        );
    }
}

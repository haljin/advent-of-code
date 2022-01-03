use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;

pub struct Cut {
    id: i64,
    x: i64,
    y: i64,
    width: i64,
    height: i64,
}

pub fn read_data() -> Vec<Cut> {
    let mut f = File::open("./data/day3").unwrap();

    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();

    let data: Vec<Cut> = contents.lines().map(|e| read_line(e.to_owned())).collect();
    data
}

fn read_line(line: String) -> Cut {
    let parts: Vec<_> = line
        .split(|c| c == '@' || c == ',' || c == ':' || c == 'x' || c == '#')
        .collect();

    Cut {
        id: parts[1].trim().parse::<i64>().unwrap(),
        x: parts[2].trim().parse::<i64>().unwrap(),
        y: parts[3].trim().parse::<i64>().unwrap(),
        width: parts[4].trim().parse::<i64>().unwrap(),
        height: parts[5].trim().parse::<i64>().unwrap(),
    }
}

pub fn solve(data: Vec<Cut>) -> i64 {
    let data_iter = data.iter();
    let max_x_cut = data_iter
        .clone()
        .max_by(|c1, c2| (c1.x + c1.width).cmp(&(c2.x + c2.width)))
        .unwrap();
    let max_y_cut = data_iter
        .clone()
        .max_by(|c1, c2| (c1.y + c1.height).cmp(&(c2.y + c2.height)))
        .unwrap();

    let max_x_size = max_x_cut.x + max_x_cut.width + 1;
    let max_y_size = max_y_cut.y + max_y_cut.height + 1;

    let mut fabric = vec![vec![0; max_x_size as usize]; max_y_size as usize];

    for cut in data_iter.clone() {
        for x in cut.x..(cut.x + cut.width) {
            for y in cut.y..(cut.y + cut.height) {
                fabric[x as usize][y as usize] += 1;
            }
        }
    }
    let mut conflicts = 0;

    for cols in fabric {
        for field in cols {
            if field > 1 {
                conflicts += 1;
            }
        }
    }

    conflicts
}

pub fn solve2(data: Vec<Cut>) -> i64 {
    let data_iter = data.iter();
    let max_x_cut = data_iter
        .clone()
        .max_by(|c1, c2| (c1.x + c1.width).cmp(&(c2.x + c2.width)))
        .unwrap();
    let max_y_cut = data_iter
        .clone()
        .max_by(|c1, c2| (c1.y + c1.height).cmp(&(c2.y + c2.height)))
        .unwrap();

    let max_x_size = max_x_cut.x + max_x_cut.width + 1;
    let max_y_size = max_y_cut.y + max_y_cut.height + 1;

    let mut fabric = vec![vec![vec![]; max_x_size as usize]; max_y_size as usize];

    for cut in data_iter.clone() {
        for x in cut.x..(cut.x + cut.width) {
            for y in cut.y..(cut.y + cut.height) {
                fabric[x as usize][y as usize].push(cut.id);
            }
        }
    }
    let mut all_ids: HashSet<_> = data_iter.clone().map(|c| c.id).collect();

    for cols in fabric {
        for field in cols {
            if field.len() > 1 {
                for id in field {
                    all_ids.remove(&id);
                }
            }
        }
    }

    *all_ids.iter().next().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        assert_eq!(
            solve(vec![
                Cut {
                    id: 1,
                    x: 1,
                    y: 3,
                    width: 4,
                    height: 4
                },
                Cut {
                    id: 2,
                    x: 3,
                    y: 1,
                    width: 4,
                    height: 4
                },
                Cut {
                    id: 3,
                    x: 5,
                    y: 5,
                    width: 2,
                    height: 2
                }
            ]),
            4
        );
    }

    #[test]
    fn test_solve2() {
        assert_eq!(
            solve2(vec![
                Cut {
                    id: 1,
                    x: 1,
                    y: 3,
                    width: 4,
                    height: 4
                },
                Cut {
                    id: 2,
                    x: 3,
                    y: 1,
                    width: 4,
                    height: 4
                },
                Cut {
                    id: 3,
                    x: 5,
                    y: 5,
                    width: 2,
                    height: 2
                }
            ]),
            3
        );
    }
}

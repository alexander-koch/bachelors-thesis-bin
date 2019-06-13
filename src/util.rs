use prettytable::{Cell, Table};
use std::collections::HashMap;
use std::char;

pub trait ToPrettyTable {
    fn to_pretty_table(&self) -> Table;
}

pub fn format_row<T>(indices: &Vec<String>, item: &HashMap<String, T>) -> Vec<Cell>
where
    T: ToString,
{
    let mut vec = vec![Cell::new(""); indices.len()];
    for (key, val) in item.iter() {
        if let Some(idx) = indices.iter().position(|x| x == key) {
            vec[idx] = Cell::new(&val.to_string());
        }
    }
    vec
}

// Thanks to Ivan Sagalaev
// http://softwaremaniacs.org/blog/2015/05/28/ijson-in-rust-unescape/en/
pub fn unescape(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(ch) = chars.next() {
        result.push(
            if ch != '\\' {
                ch
            } else {
                match chars.next() {
                    Some('u') => {
                        let value = chars.by_ref().take(4).fold(0, |acc, c| acc * 16 + c.to_digit(16).unwrap());
                        char::from_u32(value).unwrap()
                    }
                    Some('b') => '\x08',
                    Some('f') => '\x0c',
                    Some('n') => '\n',
                    Some('r') => '\r',
                    Some('t') => '\t',
                    Some('"') => '\"',
                    Some(ch) => ch,
                    _ => panic!("Malformed escape")
                }
            }
        )
    }
    result
}
use prettytable::{Table, Cell};
use std::collections::HashMap;

pub trait ToPrettyTable {
    fn to_pretty_table(&self) -> Table;
}

pub fn format_row<T>(indices: &Vec<String>, item: &HashMap<String, T>) -> Vec<Cell> 
    where T: ToString {
    let mut vec = vec![Cell::new(""); indices.len()];
    for (key, val) in item.iter() {
        if let Some(idx) = indices.iter().position(|x| x == key) {
            vec[idx] = Cell::new(&val.to_string());
        }
    }
    vec
}
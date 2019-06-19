use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::fmt;
use std::error;

use crate::util::{format_row, ToPrettyTable};
use prettytable::format;
use prettytable::{Cell, Row, Table};

use crate::ebnf::Grammar;

pub type LLTable = HashMap<String, HashMap<String, usize>>;

impl ToPrettyTable for LLTable {
    fn to_pretty_table(&self) -> Table {
        let mut table = Table::new();
        table.set_format(*format::consts::FORMAT_NO_BORDER_LINE_SEPARATOR);

        let symbols: HashSet<String> = self.iter().fold(HashSet::new(), |acc, x| {
            acc.union(&x.1.iter().map(|y| y.0.clone()).collect::<HashSet<String>>())
                .cloned()
                .collect::<HashSet<String>>()
        });

        let indices: Vec<String> = symbols.into_iter().collect();

        // let heading: Vec<Cell> = ll_table.iter().map(|x| Cell::new(x.0)).collect();
        let mut heading = vec![Cell::new("")];
        heading.extend(indices.iter().map(Cell::from));
        table.set_titles(Row::new(heading));

        for (k1, v1) in self.iter() {
            let mut row = vec![Cell::new(k1)];
            row.extend(format_row(&indices, &v1).into_iter());

            table.add_row(Row::new(row));
        }

        table
    }
}

#[derive(Debug, Clone)]
pub struct LLTableError {
    pub loc: (String, String),
    pub found: usize,
    pub expected: usize
}

impl fmt::Display for LLTableError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LL conflict {}[\"{}\"] = {}/{}", 
            self.loc.0,
            self.loc.1, 
            self.found,
            self.expected)
    }
}

impl error::Error for LLTableError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct FFSets {
    pub grammar: Rc<Grammar>,
    pub known_firsts: HashMap<String, HashSet<String>>,
    pub known_follows: HashMap<String, HashSet<String>>,
}

impl FFSets {
    pub fn new(grammar: &Rc<Grammar>) -> FFSets {
        FFSets {
            grammar: grammar.clone(),
            known_firsts: HashMap::new(),
            known_follows: HashMap::new(),
        }
    }

    pub fn scan_over(&mut self, tokens: &Vec<(String, bool)>) -> HashSet<String> {
        let mut first_set = HashSet::new();
        if tokens.is_empty() {
            first_set.insert("".to_owned());
        } else {
            for (item, term) in tokens.iter() {
                if *term {
                    first_set.insert(item.clone());
                    break;
                } else {
                    let x = self.first(item);
                    // Found terminal symbol, stopping, remove epsilon
                    if !x.contains("") && first_set.contains("") {
                        first_set.remove("");
                    }

                    first_set = first_set.union(&x).cloned().collect();
                    if !first_set.contains("") {
                        break;
                    }
                }
            }
        }
        first_set
    }

    pub fn first(&mut self, symbol: &str) -> HashSet<String> {
        if let Some(val) = self.known_firsts.get(symbol) {
            val.clone()
        } else {
            let gr = self.grammar.clone();
            self.known_firsts.insert(symbol.to_owned(), HashSet::new());

            let first_set = gr
                .iter()
                .filter(|x| x.head == symbol)
                .fold(HashSet::new(), |acc, x| {
                    acc.union(&self.scan_over(&x.body)).cloned().collect()
                });

            self.known_firsts
                .insert(symbol.to_owned(), first_set.clone());
            first_set
        }
    }

    /// Given rule of the form: A -> alpha B beta
    /// 1. Rule: Add ending symbol to FOLLOW(S)
    /// 2. Rule: If beta exists, then add FIRST(beta) - eps to FOLLOW(B)
    /// 3. a) Rule: If FIRST(beta) contained epsilon, add FOLLOW(A) to FOLLOW(B)
    /// 3. b) Rule: If beta does not exist, add FOLLOW(A) to FOLLOW(B)
    pub fn follow(&mut self, symbol: &str) -> HashSet<String> {
        if let Some(val) = self.known_follows.get(symbol) {
            val.clone()
        } else {
            let mut follow_set = HashSet::new();
            let gr = self.grammar.clone();

            // Start with an empty set to avoid infinite recursion
            self.known_follows.insert(symbol.to_owned(), HashSet::new());

            // 1. Rule: Add ending symbol to FOLLOW(S)
            if self.grammar[0].head == symbol {
                follow_set.insert("$".to_owned());
            }

            for rule in gr.iter() {
                let ix = rule.body.iter().enumerate().filter_map(|(i, x)| {
                    if x.0 == symbol && !x.1 {
                        Some(i)
                    } else {
                        None
                    }
                });
                for i in ix {
                    // 2. Rule: If beta exists, then add FIRST(beta) - eps to FOLLOW(B)

                    // Check if beta is not epsilon
                    if let Some(_) = rule.body.get(i + 1) {
                        // Calculate FIRST(beta)
                        let mut y = self.scan_over(&rule.body[i + 1..].to_vec());

                        // 3. a) Rule: If FIRST(beta) contained epsilon, add FOLLOW(A) to FOLLOW(B)
                        if y.contains("") {
                            y.remove("");
                            follow_set = follow_set
                                .union(&self.follow(&rule.head))
                                .cloned()
                                .collect();
                        }

                        follow_set = follow_set.union(&y).cloned().collect();
                    } else {
                        // 3. b) Rule: If beta does not exist, add FOLLOW(A) to FOLLOW(B)
                        follow_set = follow_set
                            .union(&self.follow(&rule.head))
                            .cloned()
                            .collect();
                    }
                }
            }

            self.known_follows
                .insert(symbol.to_owned(), follow_set.clone());
            follow_set
        }
    }

    // For each production A -> alpha do
    //   add A -> alpha to M[A, alpha] for each terminal in FIRST(alpha)
    //   If eps in FIRST(alpha) then
    //     add each M[A, b] for each terminal b in FOLLOW(A)
    pub fn construct_ll_table(&mut self) -> Result<LLTable, LLTableError> {
        let mut table = HashMap::new();

        let gr = self.grammar.clone();
        for (i, rule) in gr.iter().enumerate() {
            let mut set = self.scan_over(&rule.body);
            if set.contains("") {
                set.remove("");
                set = set.union(&self.follow(&rule.head)).cloned().collect();
            }

            for symbol in set.iter() {
                match table
                    .entry(rule.head.clone())
                    .or_insert_with(HashMap::new)
                    .entry(symbol.clone())
                {
                    Entry::Occupied(o) => return Err(LLTableError {
                        loc: (rule.head.clone(), symbol.clone()),
                        found: *o.get(),
                        expected: i
                    }),
                    Entry::Vacant(v) => v.insert(i),
                };
            }
        }
        Ok(table)
    }
}

pub fn parse_ll(
    grammar: &Grammar,
    table: &LLTable,
    input: &Vec<&str>,
) -> Result<Vec<usize>, ()> {
    let mut stack = vec![("$", true), (&grammar[0].head, false)];
    let mut steps = Vec::new();

    let mut i = 0;
    loop {
        let word: &str = input.get(i).unwrap_or(&"$");
        let (symbol, term) = stack.last().ok_or(())?;
        //println!("Current word: {}, stack: {:?}", words[i], stack);

        if *term {
            if word != *symbol {
                return Err(());
            } else if word == "$" {
                return Ok(steps);
            } else {
                stack.pop();
                i += 1;
            }
        } else {
            let index = table.get(*symbol).and_then(|x| x.get(word)).ok_or(())?;
            stack.pop();
            // println!("Apply: ({}) {}", *index, grammar[*index]);
            steps.push(*index);
            for (symbol, term) in grammar[*index].body.iter().rev() {
                stack.push((&symbol, *term));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ebnf;

    fn construct_table(path: &str) -> (Rc<Grammar>, LLTable) {
        let grammar = ebnf::parse_grammar(path);
        assert!(grammar.is_ok());
        let grammar = Rc::new(grammar.ok().unwrap());
        let mut ff = FFSets::new(&grammar);
        let table = ff.construct_ll_table();
        assert!(table.is_ok());
        (grammar, table.ok().unwrap())
    }

    #[test]
    fn test_apple_john() {
        let (grammar, table) = construct_table("grammars/ll1/applejohn.txt");

        assert!(parse_ll(&grammar, &table, &vec!["the", "apple"]).is_ok());
        assert!(parse_ll(&grammar, &table, &vec!["John"]).is_ok());
        assert!(!parse_ll(&grammar, &table, &vec!["the", "John"]).is_ok());
        assert!(!parse_ll(&grammar, &table, &vec!["the"]).is_ok());
    }

    #[test]
    fn test_arith() {
        let (grammar, table) = construct_table("grammars/ll1/arith.txt");

        assert!(parse_ll(&grammar, &table, &vec!["a", "+", "a", "*", "a"]).is_ok());
        assert!(parse_ll(&grammar, &table, &vec!["a", "*", "a", "+", "a"]).is_ok());
        assert!(parse_ll(&grammar, &table, &vec!["(", "a", "+", "a", ")", "*", "a"]).is_ok());
        assert!(!parse_ll(&grammar, &table, &vec!["(", ")"]).is_ok());
        assert!(!parse_ll(&grammar, &table, &vec!["+", "a"]).is_ok());
    }

    #[test]
    fn test_even_zeros() {
        let (grammar, table) = construct_table("grammars/ll1/even_zeros.txt");
    
        assert!(parse_ll(&grammar, &table, &vec!["1", "0", "0", "1"]).is_ok());
        assert!(!parse_ll(&grammar, &table, &vec!["1", "1", "0", "1"]).is_ok());

        assert!(parse_ll(&grammar, &table, &vec![
                "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "0", "1", "1",
                "1", "1", "1", "1", "1", "0", "1", "0", "1", "1", "1", "1", "1", "1", "1", "1",
                "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "0", "1", "1", "1", "0", "1",
                "1", "1", "1", "1", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1"
            ]).is_ok());

        assert!(!parse_ll(&grammar, &table, &vec![
                "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "0", "1", "1",
                "1", "1", "1", "1", "1", "0", "1", "0", "1", "1", "1", "1", "1", "1", "1", "1",
                "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "0", "1", "1", "1", "0", "1",
                "1", "1", "1", "1", "0", "1", "1", "1", "1", "1", "1", "1", "1", "0"
            ]).is_ok());   
    }

    #[test]
    fn test_lr() {
        let grammar = ebnf::parse_grammar("grammars/lr1/lr.txt");
        assert!(grammar.is_ok());
        let grammar = Rc::new(grammar.ok().unwrap());
        let mut ff = FFSets::new(&grammar);
        let table = ff.construct_ll_table();
        assert!(table.is_err());
    }
}
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::ebnf::Grammar;

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
    //   Add A -> alpha to M[A, alpha] for each terminal in FIRST(alpha)
    //   If eps in FIRST(alpha) then
    //     Add A -> eps to M[A, b] for each terminal b in FOLLOW(A)
    //     If $ is in FOLLOW(A) then
    //       Add A -> alpha to M[A, $]
    pub fn construct_ll_table(&mut self) -> HashMap<String, HashMap<String, usize>> {
        let mut table = HashMap::new();

        let gr = self.grammar.clone();
        for (i, rule) in gr.iter().enumerate() {
            let mut set = self.scan_over(&rule.body);
            if set.contains("") {
                set.remove("");
                set = set.union(&self.follow(&rule.head)).cloned().collect();
            }

            for symbol in set.iter() {
                table
                    .entry(rule.head.clone())
                    .or_insert_with(HashMap::new)
                    .insert(symbol.clone(), i);
            }
        }
        table
    }
}

pub fn parse_ll(
    grammar: &Grammar,
    table: &HashMap<String, HashMap<String, usize>>,
    input: &Vec<&str>,
) -> bool {
    let mut stack = vec![("$", true), (&grammar[0].head, false)];

    let mut words = input.clone();
    words.push("$");

    let mut i = 0;
    loop {
        //println!("Current word: {}, stack: {:?}", words[i], stack);

        if let Some((symbol, term)) = stack.last() {
            if *term {
                if &words[i] != symbol {
                    return false;
                } else if words[i] == "$" {
                    return true;
                } else {
                    stack.pop();
                    i += 1;
                }
            } else {
                if let Some(index) = table.get(*symbol).and_then(|x| x.get(words[i])) {
                    stack.pop();
                    println!("Apply: {}", grammar[*index]);
                    for (symbol, term) in grammar[*index].body.iter().rev() {
                        stack.push((&symbol, *term));
                    }
                } else {
                    return false;
                }
            }
        }
    }
}

use std::collections::{HashSet, HashMap};
use std::rc::Rc;

use crate::ebnf::Grammar;

#[derive(Debug, Clone)]
pub struct FFSets {
    grammar: Rc<Grammar>,
    known_firsts: HashMap<String, HashSet<String>>,
    known_follows: HashMap<String, HashSet<String>>
}

impl FFSets {

    pub fn new(grammar: &Rc<Grammar>) -> FFSets {
        FFSets {
            grammar: grammar.clone(),
            known_firsts: HashMap::new(),
            known_follows: HashMap::new()
        }
    }

    fn scan_over(&mut self, tokens: &Vec<(String, bool)>) -> HashSet<String> {
        let mut first_set = HashSet::new();
        if tokens.is_empty() {
            first_set.insert("".to_owned());
        } else {
            for (item, term) in tokens.iter() {
                if *term {
                    first_set.insert(item.clone());
                    break
                } else {
                    let x = self.first(item);
                    // Found terminal symbol, stopping, remove epsilon
                    if !x.contains("") && first_set.contains("") {
                        first_set.remove("");
                    }

                    first_set = first_set.union(&x).cloned().collect();
                    if !first_set.contains("") {
                        break
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
            let first_set = gr.iter()
                .filter(|x| x.head == symbol)
                .fold(HashSet::new(), |acc, x| {
                    acc.union(&self.scan_over(&x.body)).cloned().collect()
                });
            
            self.known_firsts.insert(symbol.to_owned(), first_set.clone());
            first_set
        }
    }

    pub fn follow(&mut self, symbol: &str) -> HashSet<String> {
        if let Some(val) = self.known_follows.get(symbol) {
            val.clone()
        } else {
            let mut follow_set = HashSet::new();
            // println!("{} == {} ?", self.grammar[0].head, symbol);
            if self.grammar[0].head == symbol {
                follow_set.insert("$".to_owned());
            }
            let gr = self.grammar.clone();
            // let mut follow_set = HashSet::new();

            println!("Looking for {}", symbol);

            for rule in gr.iter() {
                let ix = rule.body.iter().enumerate().filter_map(|(i, x)| if x.0 == symbol && !x.1 { Some(i) } else { None });
                for i in ix {
                    println!("Found {} at {} in {}", symbol, i, rule);
                    if let Some(x) = rule.body.get(i+1) {
                        
                        // 1. A -> alpha B beta: Add First(beta) to Follow(B)
                        let mut y = self.scan_over(&rule.body[i+1..].to_vec());//self.first(&item);
                        // println!("First {:?} is {:?}", rule.body[i+1], y);
            
                        if y.contains("") {
                            y.remove("");
                            // 3. Same as 2.

                            follow_set = follow_set.union(&y.union(&self.follow(&rule.head)).cloned().collect()).cloned().collect();

                            // follow_set = follow_set.union(&self.follow(&rule.head)).cloned().collect();
                        } else {
                            
                            follow_set = follow_set.union(&y).cloned().collect();
                        }
                    } else {
                        // 2. A -> alpha B: Add Follow(A) to Follow(B)
                        follow_set = follow_set.union(&self.follow(&rule.head)).cloned().collect();
                    }

                    // follow_set = follow_set.union(&).cloned().collect();

                    // 1. first of next
                    // 2. if eps UNION with follow of next
                }
            }

            self.known_follows.insert(symbol.to_owned(), follow_set.clone());
            follow_set
        }
        //HashSet::new()
    }
}
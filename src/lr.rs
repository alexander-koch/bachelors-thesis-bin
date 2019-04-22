use crate::ebnf::Grammar;
use std::collections::{HashSet, BTreeSet};
use crate::ll::FFSets;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LR0Item {
    pub rule_index: usize,
    pub dot: usize
}

impl LR0Item {
    pub fn new(rule_index: usize, dot: usize) -> LR0Item {
        LR0Item {
            rule_index: rule_index,
            dot: dot
        }
    }
}

pub fn is_final(grammar: &Grammar, item: &LR0Item) -> bool {
    let len = grammar[item.rule_index].body.len();
    item.dot >= len || len == 0
}

pub trait LRParser {
    fn closure(&mut self, set: &BTreeSet<(LR0Item, String)>) -> BTreeSet<(LR0Item, String)>;
    fn goto_state(&mut self, set: &BTreeSet<(LR0Item, String)>, x: &str) -> BTreeSet<(LR0Item, String)>;
    fn compute_states(&mut self);
}

impl LRParser for FFSets {

    fn closure(&mut self, set: &BTreeSet<(LR0Item, String)>) -> BTreeSet<(LR0Item, String)> {
        let mut q = set.clone();
        
        loop {
            let mut updates = BTreeSet::new();

            // Iterate our found set of rules
            for (item, a) in q.iter() {
                let gr = self.grammar.clone();
                let rule = &gr[item.rule_index];

                // Get the current non-terminal we are looking at
                if let Some((current, term)) = rule.body.get(item.dot) {
                    // Assure that it is a non-terminal
                    if *term {
                        continue
                    }
                    // Get all follow-up tokens
                    let mut tokens = rule.body[item.dot+1..].to_vec();
                    tokens.push((a.clone(), true));

                    // Find all rules that begin with said symbol
                    for i in gr.iter().enumerate().filter(|(_, x)| x.head == *current).map(|(i, _)| i) {
                        // Calculate first set
                        for token in self.scan_over(&tokens) {
                            let new_item = (LR0Item::new(i, 0), token);
                            if !q.contains(&new_item) {
                                updates.insert(new_item);
                            }
                        }
                    }
                }                
            }

            if updates.is_empty() {
                break
            } else {
                q = q.union(&updates).cloned().collect();
            }
        }
        q
    }

    // Basically a completer
    fn goto_state(&mut self, set: &BTreeSet<(LR0Item, String)>, x: &str) -> BTreeSet<(LR0Item, String)> {
        let mut result = BTreeSet::new();
        for (item, a) in set.iter() {

            let rule = &self.grammar[item.rule_index];
            if let Some((token, _)) = rule.body.get(item.dot) {
                if token == x {
                    result.insert((LR0Item::new(item.rule_index, item.dot + 1), a.clone()));
                }
            }
        }
        self.closure(&result)
    }

    fn compute_states(&mut self) {
        let mut set = BTreeSet::new();
        set.insert((LR0Item::new(0, 0), "$".to_owned()));
        let q0 = self.closure(&set);

        let mut qs = BTreeSet::new();
        qs.insert(q0);

        let symbols = self.grammar.get_symbols();

        loop {
            let mut updates = BTreeSet::new();
            for q in qs.iter() {
                println!("Q: {:?}", q);

                for item in symbols.iter() {
                    let cls = self.goto_state(q, item);
                    println!("Updates: {} -> {:?}", item, cls);
                    if !qs.contains(&cls) && !cls.is_empty() {
                        updates.insert(cls);
                    }
                }

                // println!("Changes: {:?}", updates);
            }

            if updates.is_empty() {
                break
            } else {
                qs = qs.union(&updates).cloned().collect();
            }
        }

        println!("QS: {:?}", qs);
    }
}
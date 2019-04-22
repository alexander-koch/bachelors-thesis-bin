use crate::ebnf::Grammar;
use std::collections::{HashSet, BTreeSet};
use crate::ll::FFSets;

use log::debug;

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

#[derive(Debug, Clone)]
pub enum LRTableEntry {
    Action(Action),
    Goto(usize)
}

#[derive(Debug, Clone)]
pub enum Action {
    Shift(usize),
    Reduce(usize),
    Acc
}

pub trait LRParser {
    fn closure(&mut self, set: &BTreeSet<(LR0Item, String)>) -> BTreeSet<(LR0Item, String)>;
    fn goto_state(&mut self, set: &BTreeSet<(LR0Item, String)>, x: (String, bool)) -> BTreeSet<(LR0Item, String)>;
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
    fn goto_state(&mut self, set: &BTreeSet<(LR0Item, String)>, x: (String, bool)) -> BTreeSet<(LR0Item, String)> {
        let mut result = BTreeSet::new();
        for (item, a) in set.iter() {

            let rule = &self.grammar[item.rule_index];
            if let Some(y) = rule.body.get(item.dot) {
                if *y == x {
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

        debug!("q0: {:?}", q0);

        // Global state set Q
        let mut qs = Vec::new();
        qs.push(q0);

        let terminals = self.grammar.terminals.clone();
        let nonterminals = self.grammar.nonterminals.clone();

        // The table transitions
        let mut final_transitions = Vec::new();

        // Current working set
        let mut current = qs.clone();
        loop {
            let mut updates = Vec::new();
            let mut transitions: Vec<(usize, LRTableEntry, String)> = Vec::new();
            let top = qs.len();
            let mut idx = top;

            // Only iterate the current set instead of the whole set Q
            for (k, q) in current.iter().enumerate() {
                let i = top-current.len()+k;

                // Add terminal shift transitions
                for symbol in terminals.iter() {
                    let cls = self.goto_state(&q, (symbol.clone(), true));
                    if !cls.is_empty() {
                        debug!("q{}: {:?}", idx, cls);
                        debug!("q{} -> q{} via {}", i, idx, symbol);

                        // Add the new state
                        updates.push(cls);

                        // Add a shift transition
                        transitions.push((i, LRTableEntry::Action(Action::Shift(idx)), symbol.clone()));
                        idx += 1;
                    }
                }

                // Add non-terminal goto transitions
                for symbol in nonterminals.iter() {
                    let cls = self.goto_state(&q, (symbol.clone(), false));
                    if !cls.is_empty() {
                        debug!("q{}: {:?}", idx, cls);
                        updates.push(cls);
                        transitions.push((i, LRTableEntry::Goto(idx), symbol.clone()));
                        idx += 1;
                    }
                }

                // Reduce transitions
                for (item, symbol) in q.into_iter() {
                    if is_final(&self.grammar, &item) {
                        if item.rule_index == 0 && symbol == "$" {
                            transitions.push((i, LRTableEntry::Action(Action::Acc), symbol.clone()));
                        } else {
                            let len = self.grammar[item.rule_index].body.len();
                            transitions.push((i, LRTableEntry::Action(Action::Reduce(len)), symbol.clone()));
                        }
                    }
                }
            }

            // Finish loop or update and repeat
            if updates.is_empty() && transitions.is_empty() {
                break
            } else {
                qs.extend(updates.iter().cloned());
                final_transitions.extend(transitions.iter().cloned());
                current = updates;
            }
        }

        // for (i, e, s) in final_transitions.iter()

        // debug!("Transitions: {:?}", final_transitions);
    }
}
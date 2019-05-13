use crate::ebnf::Grammar;
use crate::ll::FFSets;
use std::collections::{BTreeSet, HashMap, HashSet};

use std::collections::hash_map::Entry;
use std::fmt;

use log::debug;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LR0Item {
    pub rule_index: usize,
    pub dot: usize,
}

impl LR0Item {
    pub fn new(rule_index: usize, dot: usize) -> LR0Item {
        LR0Item {
            rule_index: rule_index,
            dot: dot,
        }
    }
}

pub fn is_final(grammar: &Grammar, item: &LR0Item) -> bool {
    let len = grammar[item.rule_index].body.len();
    item.dot >= len
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Action {
    Shift(usize),
    Reduce(usize),
    Acc,
}

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Action::Shift(s) => write!(f, "s{}", s),
            Action::Reduce(r) => write!(f, "r{}", r),
            Action::Acc => write!(f, "acc")
        }
    }
}

#[derive(Debug, Clone)]
pub struct LRTable {
    action_table: Vec<HashMap<String, Action>>,
    goto_table: Vec<HashMap<String, usize>>,
}

pub trait LRParser {
    fn closure(&mut self, set: &BTreeSet<(LR0Item, String)>) -> BTreeSet<(LR0Item, String)>;
    fn goto_state(
        &mut self,
        set: &BTreeSet<(LR0Item, String)>,
        x: &(String, bool),
    ) -> BTreeSet<(LR0Item, String)>;
    fn compute_states(&mut self) -> LRTable;
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
                if let Some(current) = rule.body.get(item.dot).filter(|x| !x.1).map(|x| &x.0) {

                    // Get all follow-up tokens
                    let mut tokens = rule.body[item.dot + 1..].to_vec();
                    tokens.push((a.clone(), true));

                    // Find all rules that begin with said symbol
                    for i in gr
                        .iter()
                        .enumerate()
                        .filter(|(_, x)| x.head == *current)
                        .map(|(i, _)| i)
                    {
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
                break;
            } else {
                q = q.union(&updates).cloned().collect();
            }
        }
        q
    }

    // Basically a completer
    fn goto_state(
        &mut self,
        set: &BTreeSet<(LR0Item, String)>,
        x: &(String, bool),
    ) -> BTreeSet<(LR0Item, String)> {
        let mut result = BTreeSet::new();
        for (item, a) in set.iter() {
            let rule = &self.grammar[item.rule_index];
            if let Some(y) = rule.body.get(item.dot) {
                if y == x {
                    result.insert((LR0Item::new(item.rule_index, item.dot + 1), a.clone()));
                }
            }
        }
        self.closure(&result)
    }

    fn compute_states(&mut self) -> LRTable {
        // Compute initial state set Q
        let mut set = BTreeSet::new();
        set.insert((LR0Item::new(0, 0), "$".to_owned()));
        let q0 = self.closure(&set);
        let mut qs = vec![q0];

        let terminals = self
            .grammar
            .terminals
            .iter()
            .map(|x| (x.clone(), true))
            .collect::<HashSet<(String, bool)>>();
        let nonterminals = self
            .grammar
            .nonterminals
            .iter()
            .map(|x| (x.clone(), false))
            .collect::<HashSet<(String, bool)>>();
        let symbols: HashSet<(String, bool)> = terminals.union(&nonterminals).cloned().collect();

        // The table transitions
        let mut actions = HashSet::new();
        let mut gotos = HashSet::new();

        let mut changes = true;
        while changes {
            changes = false;

            // Only iterate the current set instead of the whole set Q
            for (i, q) in qs.clone().iter().enumerate() {
                // Add terminal shift transitions
                for symbol in symbols.iter() {
                    let cls = self.goto_state(&q, symbol);
                    if cls.is_empty() {
                        continue;
                    }

                    // Find index or append
                    let j = qs.iter().position(|x| x == &cls).unwrap_or_else(|| {
                        qs.push(cls.clone());
                        changes = true;
                        qs.len() - 1
                    });

                    // Add shift or goto based on symbol type
                    if symbol.1 {
                        actions.insert((i, Action::Shift(j), symbol.0.clone()));
                    } else {
                        gotos.insert((i, j, symbol.0.clone()));
                    }
                }

                // Reduce actions
                for (item, symbol) in q.into_iter() {
                    if is_final(&self.grammar, &item) {
                        if item.rule_index == 0 && symbol == "$" {
                            actions.insert((i, Action::Acc, symbol.clone()));
                        } else {
                            // let len = self.grammar[item.rule_index].body.len();
                            actions.insert((i, Action::Reduce(item.rule_index), symbol.clone()));
                        }
                    }
                }
            }
        }

        let mut action_table = vec![HashMap::new(); qs.len()];
        let mut goto_table = vec![HashMap::new(); qs.len()];

        for (i, action, sym) in actions {
            match action_table[i].entry(sym.clone()) {
                Entry::Occupied(o) => panic!("LR shift/reduce conflict {}:{} = {}/{}", i, sym, o.get(), action),
                Entry::Vacant(v) => v.insert(action)
            };
        }

        for (i, action, sym) in gotos {
            match goto_table[i].entry(sym.clone()) {
                Entry::Occupied(o) => panic!("LR goto conflict {}:{} = {}/{}", i, sym, o.get(), action),
                Entry::Vacant(v) => v.insert(action)
            };
        }

        // debug!("states: {:?}", qs);
        // debug!("actions: {:?}", actions);

        println!("action_table: {:?}", action_table);
        println!("goto_table: {:?}", goto_table);

        LRTable {
            action_table: action_table,
            goto_table: goto_table,
        }
    }
}

pub fn parse_lr(grammar: &Grammar, table: &LRTable, input: &Vec<&str>) -> bool {
    let mut stack = vec![0];
    let mut i = 0;
    let mut words = input.clone();
    words.push("$");

    loop {
        let word = words[i];
        let state = *stack.last().unwrap();

        //println!("State: {:?}, Word: {}", stack, word);

        match table.action_table[state].get(word) {
            Some(Action::Shift(s)) => {
                debug!("Shift");
                stack.push(*s);
                i += 1;
            }
            Some(Action::Reduce(i)) => {
                debug!("Reduce");
                let k = grammar[*i].body.len();
                stack.truncate(stack.len() - k);

                let rule = &grammar[*i];
                println!("Apply: ({}) {}", *i, rule);

                stack.push(table.goto_table[*stack.last().unwrap()][&rule.head]);
            }
            Some(Action::Acc) => return true,
            _ => return false,
        }
    }
}
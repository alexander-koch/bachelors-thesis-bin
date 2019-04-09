use std::collections::HashSet;
use crate::ebnf::{Grammar, Rule};
use log::{debug, trace};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct State {
    pub rule_index: usize,
    pub dot: usize,
    pub start: usize,
}

impl State {
    pub fn new(rule_index: usize, dot: usize, start: usize) -> State {
        State {
            rule_index: rule_index,
            dot: dot,
            start: start,
        }
    }
}

fn is_final_state(grammar: &Rc<Grammar>, state: &State) -> bool {
    state.dot >= grammar[state.rule_index].body.len()
}

pub type StateSetList = Vec<HashSet<State>>;

#[derive(Debug)]
pub struct EarleyParser {
    grammar: Rc<Grammar>,
    states: StateSetList,
    cursor: usize,
}

impl EarleyParser {
    // TODO prepend root rule
    pub fn new(grammar: &Rc<Grammar>) -> EarleyParser {
        EarleyParser {
            grammar: grammar.clone(),
            states: Vec::new(),
            cursor: 0,
        }
    }

    /// Returns a set of dotted rules that are of the form
    ///
    /// B -> * zeta
    /// given a rule of the form
    /// A -> alpha * B beta
    fn predict(&self, state: &State, start: usize) -> HashSet<State> {
        trace!("predict");
        let mut new_rules = HashSet::new();
        let rule = &self.grammar[state.rule_index];
        let (token, _) = &rule.body[state.dot];

        for (i, rule) in self.grammar.iter().enumerate() {
            if &rule.head == token {
                new_rules.insert(State::new(i, 0, start));
            }
        }
        new_rules
    }

    /// Increments the dotted rule, only if the current terminal symbol may be scanned
    ///
    /// If the production is of the form
    /// A -> alpha * a beta
    /// and the current word matches a
    fn scan(&self, state: &State, word: &str) -> Option<State> {
        trace!("scan");
        let rule = &self.grammar[state.rule_index];
        let (token, _) = &rule.body[state.dot];
        if word == token {
            Some(State::new(state.rule_index, state.dot + 1, state.start))
        } else {
            None
        }
    }

    /// Returns a set of completed rules
    ///
    /// for each rule of the form
    /// A -> zeta *
    /// increment the rules of the form
    /// B -> alpha * A beta
    fn complete(&self, state: &State) -> HashSet<State> {
        trace!("complete");
        let mut rules = HashSet::new();
        let base_rule = &self.grammar[state.rule_index];

        for s in &self.states[state.start] {
            let rule = &self.grammar[s.rule_index];
            if !is_final_state(&self.grammar, s) && rule.body[s.dot].0 == base_rule.head {
                rules.insert(State::new(s.rule_index, s.dot + 1, s.start));
            }
        }
        rules
    }

    /// Processes a dotted rule state based on the current word and cursor
    fn process_state(&mut self, state: &State, word: &str) -> Option<HashSet<State>> {
        trace!("process_state: {:?}", state);

        if !is_final_state(&self.grammar, state) {
            let rule = &self.grammar[state.rule_index];
            let (_, terminal) = rule.body[state.dot];
            if !terminal {
                // k
                Some(self.predict(state, self.cursor))
            } else {
                // k+1
                if let Some(state) = self.scan(state, &word) {
                    self.states[self.cursor + 1].insert(state);
                }
                None
            }
        } else {
            // k
            Some(self.complete(state))
        }
    }

    fn process_state_set(&mut self, states: HashSet<State>, word: &str) {
        trace!("process_state_set: {:?}", states);
        let mut updates = HashSet::new();

        // Calculate the update set
        for state in states.iter() {
            if let Some(set) = self.process_state(state, word) {
                updates = updates.union(&set).cloned().collect();
            }
        }

        debug!("updates: {:?}", updates);
        let required_updates = updates
            .difference(&self.states[self.cursor])
            .cloned()
            .collect();

        // Update in original set
        self.states[self.cursor] = self.states[self.cursor]
            .union(&required_updates)
            .cloned()
            .collect();

        // Perform next iteration
        if !updates.is_empty() {
            self.process_state_set(required_updates, word);
        }
    }

    pub fn analyze(&mut self, words: &Vec<&str>) -> StateSetList {
        self.states = vec![HashSet::new(); words.len() + 1];
        self.states[0].insert(State::new(0, 0, 0));

        for i in 0..(words.len() + 1) {
            self.cursor = i;
            let word = if i < words.len() { words[i] } else { "" };

            debug!("current word: {}", word);

            let set = self.states[i].clone();
            self.process_state_set(set, word);
        }

        self.states.clone()
    }

    pub fn accepts(states: &StateSetList, words: &Vec<&str>) -> bool {
        let expected = State::new(0, 1, 0);
        states[words.len()].contains(&expected)
    }
}

pub fn fmt_state_set(grammar: &Rc<Grammar>, states: &HashSet<State>) -> String {
    format!("{{ {} }}", states.iter()
        .map(|x| grammar[x.rule_index].fmt_dot(x.dot))
        .collect::<Vec<String>>()
        .join(", "))
}

pub fn fmt_state_set_list(grammar: &Rc<Grammar>, states: &StateSetList) -> String {
    states.iter()
        .enumerate()
        .map(|(i, set)| format!("Set({}): {}", i, fmt_state_set(grammar, set)))
        .collect::<Vec<String>>()
        .join("\n")
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SPPFNode {
    pub rule_index: usize,
    pub start: usize,
    pub end: usize
}

#[derive(Debug, Clone)]
pub struct ForestBuilder {
    visited: HashSet<SPPFNode>,
}


impl ForestBuilder {
    pub fn new() -> ForestBuilder {
        ForestBuilder {
            visited: HashSet::new()
        }
    }

    fn build_tree(&mut self, grammar: &Rc<Grammar>, node: &SPPFNode, state: &State) {
        debug!("State: {}", grammar[state.rule_index].fmt_dot(state.dot));

        let rule = &grammar[state.rule_index];

        if state.dot > 0 {
            // Index for the previous symbol
            let index = state.dot - 1;
            if rule.body[index].1 {
                // Terminal symbol
                if index == 0 {
                    // Last symbol

                } else {

                }
            } else {
                // Non-terminal symbol
                if index == 0 {
                    // Last symbol

                } else {

                }
            }        
        }
    }

    pub fn build_forest(&mut self, grammar: &Rc<Grammar>, states: &StateSetList) {
        let n = states.len()-1;
        let start_node = SPPFNode { 
            rule_index: 0, 
            start: 0, 
            end: n 
        };

        let start_rule: &Rule = &grammar[0];
        for state in states[n].iter()
            .filter(|x| is_final_state(grammar, x) && grammar[x.rule_index].head == start_rule.head) {
            self.build_tree(grammar, &start_node, state);
        }
    }
}
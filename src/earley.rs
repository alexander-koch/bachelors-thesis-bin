use crate::ebnf::{Grammar, Rule};
use log::{debug, trace};
use std::collections::HashSet;
use std::rc::Rc;

use crate::lr::LR0Item;

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

fn is_final_state(grammar: &Grammar, state: &State) -> bool {
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
    format!(
        "{{ {} }}",
        states
            .iter()
            .map(|x| format!("({}, {})", grammar[x.rule_index].fmt_dot(x.dot), x.start))
            .collect::<Vec<String>>()
            .join(", ")
    )
}

pub fn fmt_state_set_list(grammar: &Rc<Grammar>, states: &StateSetList) -> String {
    states
        .iter()
        .enumerate()
        .map(|(i, set)| format!("Set({}): {}", i, fmt_state_set(grammar, set)))
        .collect::<Vec<String>>()
        .join("\n")
}

pub fn fmt_tex_state_set(grammar: &Rc<Grammar>, states: &HashSet<State>) -> String {
    format!(
        "$ {} $",
        states
            .iter()
            .map(|x| format!("({}, {})", grammar[x.rule_index].fmt_tex_dot(x.dot), x.start))
            .collect::<Vec<String>>()
            .join(" $ \\\\ $ ")
    )
}

pub fn fmt_tex_state_set_list(grammar: &Rc<Grammar>, states: &StateSetList) -> String {
    states
        .iter()
        .map(|set| format!("\\makecell[l]{{ {} }}", fmt_tex_state_set(grammar, set)))
        .collect::<Vec<String>>()
        .join("\n&")
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SPPFKind {
    Epsilon,

    /// (x, j, i) - x symbol, j left extent, i right extent
    Symbol(String, usize, usize),

    /// (B ::= a * B b, i, j), 
    Intermediate(LR0Item, usize, usize),
}

impl SPPFKind {
    fn fmt(&self, grammar: &Grammar) -> String {
        match self {
            SPPFKind::Epsilon => "eps".to_owned(),
            SPPFKind::Symbol(a, i, j) => {
                format!("{}, {}, {}", a, i, j)
            },
            SPPFKind::Intermediate(item, i, j) => {
                format!("{}, {}, {}", grammar[item.rule_index].fmt_dot(item.dot), i, j)
            }
        }
    }
}

use std::collections::HashMap;
use std::collections::BTreeSet;
use std::cmp::Ordering;

#[derive(Debug, Clone, Hash)]
pub enum FamilyNode {
    Node(usize),
    Group(usize, usize)
}

impl Ord for FamilyNode {
    fn cmp(&self, other: &FamilyNode) -> Ordering {
        //self.height.cmp(&other.height)
        match self {
            FamilyNode::Node(s) => match other {
                FamilyNode::Node(x) => s.cmp(x),
                FamilyNode::Group(x, _) => s.cmp(x) 
            },
            FamilyNode::Group(w, v) => match other {
                FamilyNode::Node(s) => w.cmp(s),
                FamilyNode::Group(x, y) => {
                    if w == x && v == y || w == y && v == x {
                        Ordering::Equal
                    } else {
                        w.cmp(x).then(v.cmp(y))
                    }
                }
            }
        }
    }
}

impl PartialOrd for FamilyNode {
    fn partial_cmp(&self, other: &FamilyNode) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for FamilyNode {
    fn eq(&self, other: &FamilyNode) -> bool {
        match self {
            FamilyNode::Node(s) => {
                match other {
                    FamilyNode::Node(d) => s == d,
                    FamilyNode::Group(_, _) => false
                }
            },
            FamilyNode::Group(w, v) => {
                match other {
                    FamilyNode::Node(_) => false,
                    FamilyNode::Group(x, y) => {
                        x == w && y == v || y == w && x == v
                    } 
                }
            }
        }
    }
}
impl Eq for FamilyNode {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SPPFNode {
    pub kind: SPPFKind,
    pub family: BTreeSet<FamilyNode>
}

impl SPPFNode {
    pub fn eps() -> SPPFNode {
        SPPFNode {
            kind: SPPFKind::Epsilon,
            family: BTreeSet::new()
        }
    }

    pub fn add_family_node(&mut self, node: FamilyNode) {
        // if (x, y) is in the set then (y, x) is already
        // Groups should be symmetric
        if self.family.contains(&node) {
            return
        }

        if self.family.len() == 2 {
            println!("{:?} to add {:?}", self.family, node);
            let mut idx = [0; 2];
            for (i, n) in self.family.iter().enumerate() {
                match n {
                    FamilyNode::Node(s) => idx[i] = *s,
                    FamilyNode::Group(_, _) => panic!("Group error")
                }
            }

            self.family.clear();
            self.family.insert(FamilyNode::Group(idx[0], idx[1]));
            self.family.insert(node);
        } else {
            match node {
                FamilyNode::Node(_) => self.family.insert(node),
                FamilyNode::Group(w, v) => {
                    if self.family.len() > 0 {
                        self.family.insert(node)
                    } else {
                        self.family.insert(FamilyNode::Node(w));
                        self.family.insert(FamilyNode::Node(v))
                    }
                }
            };
        }
    }
}

#[derive(Debug, Clone)]
pub struct ForestBuilder {
    visited: HashSet<(State, usize)>,
    predecessors: Rc<HashMap<(State, usize), Vec<(State, usize)>>>,
    reductions: Rc<HashMap<(State, usize), Vec<(State, usize)>>>,
    
    known: HashMap<SPPFKind, usize>,
    nodes: Vec<SPPFNode>,
}

impl ForestBuilder {
    pub fn new() -> ForestBuilder {
        ForestBuilder {
            visited: HashSet::new(),
            predecessors: Rc::new(HashMap::new()),
            reductions: Rc::new(HashMap::new()),
            known: HashMap::new(),
            nodes: Vec::new()
        }
    }

    fn make_node(&mut self, kind: SPPFKind) -> usize {
        if let Some(idx) = self.known.get(&kind) {
            *idx
        } else {
            let idx = self.nodes.len();
            let node = SPPFNode {
                kind: kind.clone(),
                family: BTreeSet::new()
            };
            self.nodes.push(node.clone());
           // println!("Creating node: u{} = {:?}", idx, node);
            //let idx = self.nodes.len()-1;
            self.known.insert(kind, idx);
            idx
        }
    }

    fn build_tree(&mut self, grammar: &Grammar, start_node: usize, set_index: usize, state: &State) {
        if let Some(_) = self.visited.get(&(state.clone(), set_index)) {
            return
        } else {
            self.visited.insert((state.clone(), set_index));
        }

        //println!("E{}: State: ({}, {})", set_index, grammar[state.rule_index].fmt_dot(state.dot), state.start);

        // Current rule and index
        let rule = &grammar[state.rule_index];

        if state.dot > 0 {
            // Index for the previous symbol
            let index = state.dot - 1;
            let (sym, term) = &rule.body[index];

            if index == 0 {
                // Last symbol
                if *term {
                    //println!("Rule 1");
                    let v = self.make_node(SPPFKind::Symbol(sym.clone(), set_index-1, set_index));
                    self.nodes[start_node].add_family_node(FamilyNode::Node(v));
                } else {
                    //println!("Rule 2");
                    let v = self.make_node(SPPFKind::Symbol(sym.clone(), state.start, set_index));

                    if let Some(vec) = self.reductions.clone().get(&(state.clone(), set_index)) {
                        for q in vec.iter().filter(|(_, j)| *j == state.start).map(|(x, _)| x) {
                            self.build_tree(grammar, v, set_index, q);
                        }
                    }
                    self.nodes[start_node].add_family_node(FamilyNode::Node(v));
                }
            } else {
                if *term {
                    //println!("Rule 3");
                    let v = self.make_node(SPPFKind::Symbol(sym.clone(), set_index-1, set_index));
                    let w = self.make_node(SPPFKind::Intermediate(LR0Item::new(state.rule_index, state.dot-1), state.start, set_index-1));

                    if let Some(vec) = self.predecessors.clone().get(&(state.clone(), set_index)) {
                        for (q, i) in vec.iter().filter(|(_, j)| *j == set_index-1) {//.map(|(x, _)| x) {
                            self.build_tree(grammar, w, *i, q);
                        }
                    }

                    self.nodes[start_node].add_family_node(FamilyNode::Group(w, v))
                } else {
                    //println!("Rule 4");
                    if let Some(vec) = self.reductions.clone().get(&(state.clone(), set_index)) {
                        for (q, l) in vec {
                            //println!("--- {:?} {:?} ", q, l);
                            let v = self.make_node(SPPFKind::Symbol(sym.clone(), *l, set_index));
                            self.build_tree(grammar, v, set_index, q);

                            let w = self.make_node(SPPFKind::Intermediate(LR0Item::new(state.rule_index, state.dot-1), state.start, *l));
                            if let Some(vec) = self.predecessors.clone().get(&(state.clone(), set_index)) {
                                for (p, i) in vec.iter().filter(|(_, j)| *j == *l) {//.map(|(x, _)| x) {
                                    self.build_tree(grammar, w, *i, p);
                                }
                            }

                            self.nodes[start_node].add_family_node(FamilyNode::Group(w, v));
                        }
                    }
                }
            }
        } else if rule.body.is_empty() {
            //println!("Rule 0");
            println!("Empty: {}, start: {:?}", rule, self.nodes[start_node]);
            let v = self.make_node(SPPFKind::Symbol(rule.head.clone(), set_index, set_index));
            let eps = self.make_node(SPPFKind::Epsilon);
            self.nodes[v].add_family_node(FamilyNode::Node(eps));

            if start_node != v {
                self.nodes[start_node].add_family_node(FamilyNode::Node(v));
            }
        }
    }

    pub fn build_forest(&mut self, grammar: &Grammar, states: &StateSetList) {
        let n = states.len() - 1;
        let start_node = self.make_node(SPPFKind::Symbol(grammar[0].head.clone(), 0, n));

        let mut reductions = HashMap::new();
        let mut predecessors = HashMap::new();

        for i in 0..states.len() {
            for t in states[i].iter() {

                if is_final_state(grammar, t) {
                    let t_rule = &grammar[t.rule_index];
                    for q in states[t.start].iter() {
                        if let Some((token, term)) = grammar[q.rule_index].body.get(q.dot) {
                            if !is_final_state(&grammar, q) && !term && *token == t_rule.head {
                                let p = State::new(q.rule_index, q.dot + 1, q.start);
                                
                                reductions.entry((p.clone(), i)).or_insert_with(Vec::new).push((t.clone(), t.start));
                                // Tau != eps
                                if q.dot > 0 {
                                    predecessors.entry((p, i)).or_insert_with(Vec::new).push((q.clone(), t.start));
                                }
                            }
                        }
                    }
                }

                // Scan
                if t.dot > 1 {
                    for prev in states[i-1].iter()
                        .filter(|x| x.start == t.start 
                            && x.rule_index == t.rule_index
                            && x.dot == t.dot-1
                            && grammar[x.rule_index].body.get(x.dot).map(|x| x.1).unwrap_or(false)) {
                        predecessors.entry((t.clone(), i)).or_insert_with(Vec::new).push((prev.clone(), i-1));
                    }
                }
            }
        }

        self.reductions = Rc::new(reductions);
        self.predecessors = Rc::new(predecessors);

        //println!("Pred: {:?}", self.predecessors);
        //println!("Redu: {:?}", self.reductions);

        println!("Reductions");
        for ((k, i), v) in self.reductions.iter() {
            for (s, n) in v.iter() {
                println!("({}, {}) from E{} ->{} ({}, {})", grammar[k.rule_index].fmt_dot(k.dot), k.start, i, n, grammar[s.rule_index].fmt_dot(s.dot), s.start);
            }
        }

        println!("Predecessors");
        for ((k, i), v) in self.predecessors.iter() {
            for (s, n) in v.iter() {
                println!("({}, {}) from E{} ->{} ({}, {})", grammar[k.rule_index].fmt_dot(k.dot), k.start, i, n, grammar[s.rule_index].fmt_dot(s.dot), s.start);
            }
        }

        let start_rule: &Rule = &grammar[0];
        for state in states[n]
            .iter()
            .filter(|x| is_final_state(grammar, x) && grammar[x.rule_index].head == start_rule.head && x.start == 0)
        {
            self.build_tree(grammar, start_node, n, state);
        }

        //self.nodes.insert(start_node.clone());

       //‚ println!("SPPFNodes: {:?}", self.nodes);

        for (i, node) in self.nodes.iter().enumerate() {
            let style = match node.kind {
                SPPFKind::Intermediate(_, _, _) => "shape=box",
                _ => "shape=box, style=rounded"
            };

            println!("{} [label=\"{}\", {}];", i, node.kind.fmt(grammar), style);
        }

        let mut j = self.nodes.len();
        for (i, node) in self.nodes.iter().enumerate() {
           // println!("({}) = {:?}", i, node);

            //let node_name = node.kind.fmt(grammar);

            for child in &node.family {
                match child {
                    FamilyNode::Node(s) => println!("{} -> {};", i, s),
                    FamilyNode::Group(w, v) => {
                        println!("{} [shape=circle, fixedsize=true, width=0.15, height=0.15, label=\"\"]", j);
                        println!("{} -> {};", i, j);
                        println!("{} -> {};", j, w);
                        println!("{} -> {};", j, v);
                        j += 1;
                    }
                }
            }
        }

        //println!("Start: {:?}", self.nodes[start_node]);

        /*/or fam in start_node.family {
            println!("Sub: {:?}", fam.kind);
            for fam in fam.family {
                println!("--- {:?}", fam.kind);
            }
        }*/
    }
}

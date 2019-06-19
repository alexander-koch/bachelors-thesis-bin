use crate::ebnf::Grammar;
use crate::ll::FFSets;
use std::collections::{BTreeSet, HashMap, HashSet};

use prettytable::format;
use prettytable::{Cell, Row, Table};
use std::collections::hash_map::Entry;
use std::fmt;
use std::error;

use crate::util::{format_row, ToPrettyTable};

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
            Action::Acc => write!(f, "acc"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LRTable {
    action_table: Vec<HashMap<String, Action>>,
    goto_table: Vec<HashMap<String, usize>>,
}

impl ToPrettyTable for LRTable {
    fn to_pretty_table(&self) -> Table {
        // Collect table keys
        let action_keys: HashSet<String> =
            self.action_table.iter().fold(HashSet::new(), |acc, x| {
                acc.union(&x.iter().map(|x| x.0.clone()).collect::<HashSet<String>>())
                    .cloned()
                    .collect::<HashSet<String>>()
            });

        let goto_keys: HashSet<String> = self.goto_table.iter().fold(HashSet::new(), |acc, x| {
            acc.union(&x.iter().map(|x| x.0.clone()).collect::<HashSet<String>>())
                .cloned()
                .collect::<HashSet<String>>()
        });

        let action_indices: Vec<String> = action_keys.into_iter().collect();
        let goto_indices: Vec<String> = goto_keys.into_iter().collect();

        // let action_indices = action_keys.iter().enumerate().map(|(i, x)| (x.clone(), i)).collect::<HashMap<String, usize>>();
        // let goto_indices = goto_keys.iter().enumerate().map(|(i, x)| (x.clone(), i)).collect::<HashMap<String, usize>>();

        // println!("{:?}", action_indices);

        let mut table = Table::new();
        table.set_format(*format::consts::FORMAT_NO_BORDER_LINE_SEPARATOR);

        let action_cells: Vec<Cell> = action_indices.iter().map(Cell::from).collect();
        let goto_cells: Vec<Cell> = goto_indices.iter().map(Cell::from).collect();
        let mut header = vec![Cell::new("")];
        header.extend(action_cells.into_iter());
        header.extend(goto_cells.into_iter());

        table.set_titles(Row::new(header));

        for (idx, (x, y)) in self
            .action_table
            .iter()
            .zip(self.goto_table.iter())
            .enumerate()
        {
            let x_row = format_row(&action_indices, x);
            let y_row = format_row(&goto_indices, y);

            let mut row = vec![Cell::new(&idx.to_string())];
            row.extend(x_row.into_iter());
            row.extend(y_row.into_iter());

            table.add_row(Row::new(row));
        }
        table
    }
}

#[derive(Debug, Clone)]
pub enum LRTableErrorKind {
    ActionError(Action, Action),
    GotoError(usize, usize)
}

impl fmt::Display for LRTableErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LRTableErrorKind::ActionError(x, y) => write!(f, "{}/{}", x, y),
            LRTableErrorKind::GotoError(x, y) => write!(f, "{}/{}", x, y)
        }
    }
}

#[derive(Debug, Clone)]
pub struct LRTableError {
    pub loc: (usize, String),
    pub kind: LRTableErrorKind,
}

impl fmt::Display for LRTableError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self.kind { 
            LRTableErrorKind::ActionError(_, _) => "shift/reduce",
            LRTableErrorKind::GotoError(_, _) => "goto",
        };
        write!(f, "LR {} conflict {}:{} = {}", name, self.loc.0, self.loc.1, self.kind)
    }
}

impl error::Error for LRTableError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

pub trait LRParser {
    fn closure(&mut self, set: &BTreeSet<(LR0Item, String)>) -> BTreeSet<(LR0Item, String)>;
    fn goto_state(
        &mut self,
        set: &BTreeSet<(LR0Item, String)>,
        x: &(String, bool),
    ) -> BTreeSet<(LR0Item, String)>;
    fn compute_states(&mut self) -> Result<LRTable, LRTableError>;
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

    fn compute_states(&mut self) -> Result<LRTable, LRTableError> {
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

        let mut action_table: Vec<HashMap<String, Action>> = vec![HashMap::new(); qs.len()];
        let mut goto_table = vec![HashMap::new(); qs.len()];

        for (i, action, sym) in actions {
            match action_table[i].entry(sym.clone()) {
                Entry::Occupied(o) => return Err(LRTableError {
                    loc: (i, sym),
                    kind: LRTableErrorKind::ActionError(o.get().clone(), action)
                }),
                Entry::Vacant(v) => v.insert(action),
            };
        }

        for (i, action, sym) in gotos {
            match goto_table[i].entry(sym.clone()) {
                Entry::Occupied(o) => return Err(LRTableError {
                    loc: (i, sym),
                    kind: LRTableErrorKind::GotoError(*o.get(), action)
                }),
                Entry::Vacant(v) => v.insert(action),
            };
        }

        // for (i, state) in qs.iter().enumerate() {
        //     println!("State {}: {:?}", i, state);
        // }

        //println!("states: {:?}", qs);
        // debug!("actions: {:?}", actions);

        // println!("action_table: {:?}", action_table);
        // println!("goto_table: {:?}", goto_table);

        Ok(LRTable {
            action_table: action_table,
            goto_table: goto_table,
        })
    }
}

pub fn parse_lr(grammar: &Grammar, table: &LRTable, input: &Vec<&str>) -> Result<Vec<usize>, ()> {
    let mut steps = Vec::new();
    let mut stack = vec![0];
    let mut i = 0;

    loop {
        let word: &str = input.get(i).unwrap_or(&"$");
        let state: &usize = stack.last().ok_or(())?;
        let entry: &Action = table.action_table.get(*state)
            .and_then(|x| x.get(word))
            .ok_or(())?;

        match entry {
            Action::Shift(s) => {
                stack.push(*s);
                i += 1;
            }
            Action::Reduce(i) => {
                steps.push(*i);
                let rule = &grammar[*i];
                let k = rule.body.len();
                stack.truncate(stack.len() - k);

                let top_state: &usize = stack.last().ok_or(())?;
                let new_entry: &usize = table.goto_table.get(*top_state)
                    .and_then(|x| x.get(&rule.head))
                    .ok_or(())?;
                
                stack.push(*new_entry);
            }
            Action::Acc => return Ok(steps),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ebnf;
    use std::rc::Rc;

    fn construct_table(path: &str) -> (Rc<Grammar>, LRTable) {
        let grammar = ebnf::parse_grammar(path);
        assert!(grammar.is_ok());
        let grammar = Rc::new(grammar.ok().unwrap());
        let mut ff = FFSets::new(&grammar);
        let table = ff.compute_states();
        assert!(table.is_ok());
        (grammar, table.ok().unwrap())
    }

    #[test]
    fn test_lr() {
        let (grammar, table) = construct_table("grammars/lr1/lr.txt");
    
        assert!(parse_lr(&grammar, &table, &vec!["a"]).is_ok());
        assert!(parse_lr(&grammar, &table, &vec!["a", "a"]).is_ok());
        assert!(parse_lr(&grammar, &table, &vec!["a", "a", "a"]).is_ok());
        assert!(!parse_lr(&grammar, &table, &vec!["b"]).is_ok());
        assert!(!parse_lr(&grammar, &table, &vec![]).is_ok());
    }

    #[test]
    fn test_arith_left_rec() {
        let (grammar, table) = construct_table("grammars/lr1/arith_left_rec.txt");

        assert!(parse_lr(&grammar, &table, &vec!["a", "+", "a", "*", "a"]).is_ok());
        assert!(parse_lr(&grammar, &table, &vec!["a", "*", "a", "+", "a"]).is_ok());
        assert!(!parse_lr(&grammar, &table, &vec!["(", "a", "+", "a", ")", "*", "a"]).is_ok());
        assert!(!parse_lr(&grammar, &table, &vec!["(", ")"]).is_ok());
        assert!(!parse_lr(&grammar, &table, &vec!["+", "a"]).is_ok());
    }

    #[test]
    fn test_english() {
        let (grammar, table) = construct_table("grammars/lr1/english.txt");
    
        assert!(parse_lr(&grammar, &table, &vec!["I", "prefer", "a", "morning", "flight"]).is_ok());
        assert!(parse_lr(&grammar, &table, &vec!["the", "answer", "is", "42"]).is_ok());
        assert!(!parse_lr(&grammar, &table, &vec!["is", "this", "it"]).is_ok());
    }

}
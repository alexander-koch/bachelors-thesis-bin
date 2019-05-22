use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::ebnf::{Grammar, Rule};
use crate::ll::FFSets;
use crate::lr::{is_final, LR0Item};

use log::debug;

pub fn fmt_tex_lr0_set(grammar: &Grammar, states: &HashSet<LR0Item>) -> String {
    format!(
        "$ {} $",
        states
            .iter()
            .map(|x| grammar[x.rule_index].fmt_tex_dot(x.dot))
            .collect::<Vec<String>>()
            .join(" $ \\\\ $ ")
    )
}

pub fn fmt_tex_lr0_matrix(grammar: &Grammar, t: Vec<Vec<HashSet<LR0Item>>>) -> String {
    t.iter()
        //.enumerate()
        .map(|column| {
            column
                .iter()
                .map(|x| format!("\\makecell[l]{{ {} }}", fmt_tex_lr0_set(grammar, x)))
                .collect::<Vec<String>>()
                .join("\n&")
        })
        .collect::<Vec<String>>()
        .join("\\\\ \\hline \n")
}

pub fn find_rule_reductions(ff: &mut FFSets, rule: &Rule) -> HashSet<String> {
    let mut series = HashSet::new();
    let mut terminal = None;

    let mut body = rule.body.iter();
    while let Some((sym, term)) = body.next() {
        //debug!("({}, {})", sym, term);
        if *term {
            return HashSet::new();
        } else {
            if ff.first(sym).contains("") {
                series.insert(sym.clone());
            } else {
                terminal = Some(sym);
                break;
            }
        }
    }

    if let Some(sym) = terminal {
        let tail = ff.scan_over(&body.cloned().collect());
        if tail.contains("") {
            let mut set = HashSet::new();
            if *sym != rule.head {
                set.insert(sym.clone());
            }
            set
        } else {
            HashSet::new()
        }
    } else {
        series.remove(&rule.head);
        series
    }
}

pub type ReductionMap = HashMap<String, HashSet<String>>;
pub type SymbolMapping = HashMap<String, HashSet<LR0Item>>;

pub fn calculate_reductions(ff: &mut FFSets) -> ReductionMap {
    let mut mapping = HashMap::new();

    let gr = ff.grammar.clone();
    for rule in gr.iter() {
        for n in find_rule_reductions(ff, rule) {
            mapping
                .entry(n)
                .or_insert_with(HashSet::new)
                .insert(rule.head.clone());
        }
    }

    mapping
}

#[derive(Debug, Clone)]
pub struct HarrisonParser {
    grammar: Rc<Grammar>,
    ff: FFSets,
    derivation_map: SymbolMapping,
    reduction_map: ReductionMap,
}

impl HarrisonParser {
    pub fn new(grammar: &Rc<Grammar>) -> HarrisonParser {
        let mut ff = FFSets::new(grammar);
        let reduction_map = calculate_reductions(&mut ff);

        //println!("{:?}", ff);
        // for symbol in ff.grammar.clone().nonterminals.iter() {
        //     ff.first(symbol);
        //     ff.follow(symbol);
        // }

        let mut parser = HarrisonParser {
            grammar: grammar.clone(),
            ff: ff,
            reduction_map: HashMap::new(),
            derivation_map: HashMap::new()
        };
        parser.init(reduction_map);
        parser
    }

    fn init(&mut self, reduction_map: ReductionMap) {
        let gr = self.grammar.clone();
        for v in gr.nonterminals.iter() {
            let derivations = self.find_derivations(v);
            self.derivation_map.entry(v.clone())
                .and_modify(|x| *x = x.union(&derivations).cloned().collect())
                .or_insert(derivations);

            let reductions = self.find_reductions(&reduction_map, v);
            self.reduction_map.entry(v.clone())
                .and_modify(|x| *x = x.union(&reductions).cloned().collect())
                .or_insert(reductions);
        }
    }

    fn find_derivations(&mut self, symbol: &str) -> HashSet<LR0Item> {
        let start = self
            .grammar
            .iter()
            .enumerate()
            .filter(|(_, x)| x.head == symbol)
            .map(|(i, _)| LR0Item::new(i, 0))
            .collect::<HashSet<LR0Item>>();

        let mut result = start;

        loop {
            let mut update = HashSet::new();
            for (sym, term) in result
                .iter()
                .flat_map(|x| self.grammar[x.rule_index].body.get(x.dot))
            {
                update = update
                    .union(
                        &self
                            .grammar
                            .iter()
                            .enumerate()
                            .filter(|(_, x)| x.head == *sym && !term)
                            .map(|(i, _)| LR0Item::new(i, 0))
                            .collect(),
                    )
                    .cloned()
                    .collect();
            }
            update = update
                .union(&self.complete(&result, &result, false))
                .cloned()
                .collect();

            if update.is_subset(&result) {
                break;
            } else {
                result = result.union(&update).cloned().collect();
            }
        }
        result
    }

    /// Finds reductions for a given production name
    ///
    /// # Arguments
    ///
    /// * `symbol` - Name of the production
    fn find_reductions(&self, reduction_map: &ReductionMap, symbol: &str) -> HashSet<String> {
        let mut current = HashSet::new();
        let mut result = HashSet::new();
        current.insert(symbol.to_owned());

        loop {
            let mut updates = HashSet::new();

            for sym in current {
                if let Some(set) = reduction_map.get(&sym) {
                    updates = updates.union(&set).cloned().collect();
                }
            }

            if updates.is_empty() {
                break;
            }

            result = result.union(&updates).cloned().collect();
            current = updates;
        }

        result
    }

    /// Returns all advancements for a given LR(0)-item
    /// if the following symbol is derivable to epsilon
    ///
    /// # Arguments
    ///
    /// * `item` - The item to be skipped over
    fn skip_epsilon(&mut self, item: LR0Item) -> HashSet<LR0Item> {
        let max = self.grammar[item.rule_index].body.len();
        let mut result = HashSet::new();
        result.insert(item.clone());

        for i in item.dot..max {
            if let Some((sym, term)) = self.grammar[item.rule_index].body.get(i) {
                if !term && self.ff.first(sym).contains("") {
                    result.insert(LR0Item::new(item.rule_index, i + 1));
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        result
    }

    /// Completes items in Q by using the finished ones in R
    ///
    /// non-chained:
    /// Q x R = {A -> alpha B beta • | A -> alpha • B beta, B -> lambda • in R}
    ///
    /// chained:
    /// Q * R = {A -> alpha B beta • | A -> alpha • B beta, B derivable to C, C -> lambda • in R}
    ///
    /// # Arguments
    ///
    /// * `q` - Set of items to be processed
    /// * `r` - Set of finished items
    /// * `chained` - Allow indirect completion
    fn complete(
        &mut self,
        q: &HashSet<LR0Item>,
        r: &HashSet<LR0Item>,
        chained: bool,
    ) -> HashSet<LR0Item> {
        let mut result = HashSet::new();
        let gr = self.grammar.clone();

        // Find all finished rules A in R
        // If chained, then find everything that is derivable to A
        // Map to the production names
        let terminated_items = r.iter().filter(|x| is_final(&gr, x));
        let terminated_symbols: HashSet<String> = if chained {
            terminated_items
                .flat_map(|x| {
                    //let mut set = self.find_reductions(gr[x.rule_index].head.clone());
                    let v = &gr[x.rule_index].head;

                    let mut set: HashSet<String> = self.reduction_map.get(v).cloned().unwrap_or(HashSet::new());
                    set.insert(gr[x.rule_index].head.clone());
                    set
                })
                .collect()
        } else {
            terminated_items
                .map(|x| gr[x.rule_index].head.clone())
                .collect()
        };

        // For every finished symbol in R and every unfinished rule in Q
        // Skip the entries where the current symbol and the terminated ones match
        // When found, try to skip any items that can be derived to epsilon
        for terminated_item in terminated_symbols {
            for unfinished_item in q.iter().filter(|x| !is_final(&gr, x)) {
                // Retrieve the token that is currently read
                if let Some((token, term)) =
                    &gr[unfinished_item.rule_index].body.get(unfinished_item.dot)
                {
                    // If the tokens match
                    if !*term && *token == terminated_item {
                        result = result
                            .union(&self.skip_epsilon(LR0Item::new(
                                unfinished_item.rule_index,
                                unfinished_item.dot + 1,
                            )))
                            .cloned()
                            .collect();
                    }
                }
            }
        }

        result
    }

    fn predict(&mut self, input: &HashSet<String>) -> HashSet<LR0Item> {
        input
            .iter()
            .fold(HashSet::new(), |acc, x| acc.union(self.derivation_map.get(x)
                .unwrap_or(&HashSet::new())).cloned().collect())
    }

    fn scan(&mut self, previous: &HashSet<LR0Item>, word: &str) -> HashSet<LR0Item> {
        let mut result = HashSet::new();
        for item in previous.iter() {
            let rule = &self.grammar[item.rule_index];
            if let Some((token, term)) = &rule.body.get(item.dot) {
                if word == token && *term {
                    result = result
                        .union(&self.skip_epsilon(LR0Item::new(item.rule_index, item.dot + 1)))
                        .cloned()
                        .collect();
                }
            }
        }
        result
    }

    pub fn accepts(&mut self, words: &Vec<&str>) -> bool {
        let n = words.len();
        let mut t: Vec<Vec<HashSet<LR0Item>>> = vec![vec![HashSet::new(); n + 1]; n + 1];
        let mut set = HashSet::new();
        set.insert(self.grammar[0].head.clone());
        t[0][0] = self.predict(&set);
        debug!("t[0][0] = {:?}", t[0][0]);

        for j in 1..(n + 1) {
            // Scan
            for i in 0..j {
                t[i][j] = self.scan(&t[i][j - 1], words[j - 1]);
                debug!("scan: t[{}, {}] = {:?}", i, j, t[i][j]);
            }

            // Complete
            for k in (0..j).rev() {
                let result = self.complete(&t[k][k], &t[k][j], true);

                t[k][j] = t[k][j].union(&result).cloned().collect();
                for i in (0..k).rev() {
                    let result = self.complete(&t[i][k], &t[k][j], false);
                    t[i][j] = t[i][j].union(&result).cloned().collect();
                }

                debug!("complete: t[{}, {}] = {:?}", k, j, t[k][j]);
            }

            // Predict
            let mut ts = HashSet::new();
            for i in 0..j {
                ts = ts.union(&t[i][j]).cloned().collect();
            }
            debug!("Ts: {:?}", ts);

            t[j][j] = self.predict(
                &ts.iter()
                    .flat_map(|x| self.grammar[x.rule_index].body.get(x.dot))
                    .filter(|(_, term)| !*term)
                    .map(|(sym, _)| sym.clone())
                    .collect(),
            );

            debug!("predict: t[{}, {}] = {:?}", j, j, t[j][j]);
        }

        // !("{}", fmt_tex_lr0_matrix(grammar, t.clone()));

        let start_symbol = &self.grammar[0].head;
        self.grammar
            .iter()
            .enumerate()
            .filter(|(_, x)| x.head == *start_symbol)
            .map(|(i, _)| LR0Item::new(i, self.grammar[i].body.len()))
            .any(|x| t[0][n].contains(&x))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ebnf;

    fn harrison_recognize(path: &str, words: &Vec<&str>) -> bool {
        let grammar = ebnf::parse_grammar(path);
        assert!(grammar.is_ok());
        let grammar = Rc::new(grammar.ok().unwrap());
        let mut parser = HarrisonParser::new(&grammar);
        parser.accepts(words)
    }

    #[test]
    fn test_harrison1() {
        assert!(harrison_recognize("examples/harrison.txt", &vec!["a"]));
        assert!(!harrison_recognize("examples/harrison.txt", &vec!["b"]));
    }

    #[test]
    fn test_harrison2() {
        assert!(harrison_recognize(
            "examples/harrison2.txt",
            &vec!["a", "+", "a", "*", "a"]
        ));
        assert!(!harrison_recognize(
            "examples/harrison2.txt",
            &vec!["a", "+", "+"]
        ));
    }

    #[test]
    fn test_dyck1() {
        assert!(harrison_recognize(
            "examples/dyck1.txt",
            &vec!["(", "(", ")", ")"]
        ));
        assert!(!harrison_recognize(
            "examples/dyck1.txt",
            &vec!["(", "(", "(", "("]
        ));
    }

    #[test]
    fn test_even_zeros() {
        assert!(harrison_recognize(
            "examples/even_zeros.txt",
            &vec!["1", "0", "0", "1"]
        ));
        assert!(!harrison_recognize(
            "examples/even_zeros.txt",
            &vec!["1", "1", "0", "1"]
        ));
        assert!(harrison_recognize(
            "examples/even_zeros.txt",
            &vec!["1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "0", "1", "1", "1", "1", "1", "1", "1", "0", "1", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "0", "1", "1", "1", "0", "1", "1", "1", "1", "1", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1"]
        ));
        assert!(!harrison_recognize(
            "examples/even_zeros.txt",
            &vec!["1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "0", "1", "1", "1", "1", "1", "1", "1", "0", "1", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "0", "1", "1", "1", "0", "1", "1", "1", "1", "1", "0", "1", "1", "1", "1", "1", "1", "1", "1", "0"]
        ));
    }
}

use std::collections::HashSet;
use crate::ebnf::Grammar;

use crate::lr::{LR0Item, is_final};

fn complete(grammar: &Grammar, q: &HashSet<LR0Item>, r: &HashSet<LR0Item>, single: bool) -> HashSet<LR0Item> {
    let mut result = HashSet::new();
    let mut unfinished: HashSet<LR0Item> = q.clone();
    let mut terminating = r.clone();

    loop {
        let mut updates = HashSet::new();
        
        // Iterate every final rule in R
        for symbol in terminating.iter().filter(|x| is_final(grammar, &x)).map(|x| &grammar[x.rule_index].head) {
            // Iterate every rule in Q that can possibly be completed by any given rule in R
            for i in unfinished.iter() {
                // Check that the rule in Q is not finished and currently looks at the symbol
                // that is finished by the rule in R
                if !is_final(&grammar, &i) && grammar[i.rule_index].body[i.dot].0 == *symbol {
                    let item = LR0Item::new(i.rule_index, i.dot+1);
                    //result.insert(item.clone());
                    if !result.contains(&item) {
                        updates.insert(item);
                    }
                }
            }
        }

        if updates.is_empty() {
            break
        }

        // Find possible epsilon completions
        updates = predict_items(grammar, &updates).union(&updates).cloned().collect::<HashSet<LR0Item>>().difference(&q).cloned().collect();
        result = result.union(&updates).cloned().collect();

        if single {
            break
        }

        // Rip updates apart
        for rule in updates.iter() {
            if is_final(grammar, &rule) {
                terminating.insert(rule.clone());
            } else {
                unfinished.insert(rule.clone());
            } 
        }
    }

    result
}

// Update needs to contain finished rules in order to complete rule
fn predict_items(grammar: &Grammar, rules: &HashSet<LR0Item>) -> HashSet<LR0Item> {
    let mut result = HashSet::new();//rules.clone();
    let mut current_set = rules.clone();

    loop {
        // Stores all updates for this round
        //let mut update_set = HashSet::new();
        //let mut eps_set = HashSet::new();
        let mut update_set = HashSet::new();

        // Check for epsilon rules
        for item in current_set.iter() {
            let rule = &grammar[item.rule_index];
            if let Some((token, _)) = rule.body.get(item.dot) {
                for (i, rule) in grammar.iter().enumerate() {
                    if &rule.head == token {
                        let item = LR0Item::new(i, 0);
                        //if !result.contains(&item) {
                           // eps_set.insert(item.clone());
                            update_set.insert(item);
                        //}
                    }
                }
            }     
        }

        for item in update_set.clone().iter() {
            // Is one of the rules eps and therefore final?
            if is_final(grammar, item) {
                let symbol = &grammar[item.rule_index].head;
                for i in rules.union(&result).cloned().collect::<HashSet<LR0Item>>().iter() {
                    if !is_final(&grammar, &i) && grammar[i.rule_index].body[i.dot].0 == *symbol {
                        update_set.insert(LR0Item::new(i.rule_index, i.dot+1));
                    }
                }
            }
        }

        if update_set.is_empty() {
            break
        }

        result = result.union(&update_set).cloned().collect();
        current_set = update_set;
    }
   //result.difference(&rules).cloned().collect()
   result
}

pub fn predict(grammar: &Grammar, input: &HashSet<String>) -> HashSet<LR0Item> {    
    let new_rules = grammar.iter()
        .enumerate()
        .filter(|(_, x)| input.contains(&x.head))
        .map(|(i, _)| LR0Item::new(i, 0))
        .collect::<HashSet<LR0Item>>();
    predict_items(grammar, &new_rules).union(&new_rules).cloned().collect()
}

fn skip_epsilon(grammar: &Grammar, items: &HashSet<LR0Item>) -> HashSet<LR0Item> {
    let allowed_indices: HashSet<usize> = items.iter().map(|x| x.rule_index).collect();
    predict_items(grammar, items).iter()
        .filter(|x| allowed_indices.contains(&x.rule_index))
        .cloned()
        .collect()
}

fn scan(grammar: &Grammar, previous: &HashSet<LR0Item>, word: &str) -> HashSet<LR0Item> {
    let mut result = HashSet::new();
    for item in previous.iter() {
        let rule = &grammar[item.rule_index];
        if let Some((token, _)) = &rule.body.get(item.dot) {
            if word == token {
                result.insert(LR0Item::new(item.rule_index, item.dot+1));
            }
        }
    }
    skip_epsilon(grammar, &result).union(&result).cloned().collect()
}

pub fn parse(grammar: &Grammar, words: &Vec<&str>) -> bool {
    let n = words.len();
    let mut t: Vec<Vec<HashSet<LR0Item>>> = vec![vec![HashSet::new(); n+1]; n+1];
    let mut set = HashSet::new();
    set.insert(grammar[0].head.clone());
    t[0][0] = predict(&grammar, &set);
    println!("t[0][0] = {:?}", t[0][0]);

    for j in 1..(n+1) {
        // Scan
        for i in 0..j {
            t[i][j] = scan(grammar, &t[i][j-1], words[j-1]);
            println!("t[{}, {}] = {:?}", i, j, t[i][j]);
        }

        // Complete
        for k in (0..j).rev() {
            let result = complete(grammar, &t[k][k], &t[k][j], false);

            t[k][j] = t[k][j].union(&result).cloned().collect();
            for i in (0..k).rev() {
                let result = complete(grammar, &t[i][k], &t[k][j], true);
                t[i][j] = t[i][j].union(&result).cloned().collect();
            }

            println!("t[{}, {}] = {:?}", k, j, t[k][j]);
        }

        // Predict
        let mut ts = HashSet::new();
        for i in 0..j {
            ts = ts.union(&t[i][j]).cloned().collect();
        }
        println!("Ts: {:?}", ts);
        t[j][j] = predict_items(grammar, &ts);
        println!("t[{}, {}] = {:?}", j, j, t[j][j]);
    }

    let start_symbol = &grammar[0].head;
    grammar.iter()
        .enumerate()
        .filter(|(_, x)| x.head == *start_symbol)
        .map(|(i, _)| LR0Item::new(i, grammar[i].body.len()))
        .any(|x| t[0][n].contains(&x))
}
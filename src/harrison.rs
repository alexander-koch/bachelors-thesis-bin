use crate::ebnf::Grammar;
use std::collections::HashSet;

use crate::lr::{is_final, LR0Item};

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
    t
        .iter()
        //.enumerate()
        .map(|column| column.iter()
            .map(|x| format!("\\makecell[l]{{ {} }}", fmt_tex_lr0_set(grammar, x)))
            .collect::<Vec<String>>()
            .join("\n&")
        )
        .collect::<Vec<String>>()
        .join("\\\\ \\hline \n")
}

fn skip_epsilon(grammar: &Grammar, item: LR0Item) -> LR0Item {

    /*let mut input = HashSet::new();
    if let Some((token, term)) = grammar[item.rule_index].body.get(item.dot)

    input.insert()

    predict(grammar, input: &HashSet<String>)*/

    // TODO

    /*let allowed_indices: HashSet<usize> = items.iter().map(|x| x.rule_index).collect();
    predict_lr0_items(grammar, items)
        .iter()
        .filter(|x| allowed_indices.contains(&x.rule_index))
        .cloned()
        .collect()*/

    item
}

// B =>* C strict
// B =>* Cn not strict

fn find_reductions(grammar: &Grammar, symbol: String) -> HashSet<String> {
    let mut result = HashSet::new();

    // TODO

    result
}

fn find_derivations(grammar: &Grammar, symbol: String) -> HashSet<LR0Item> {
    //let mut result: HashSet<LR0Item> = HashSet::new();

    //grammar.iter().filter(|x| 
    let start = grammar.iter()
        .enumerate()
        .filter(|(_, x)| x.head == symbol)
        .map(|(i, _)| LR0Item::new(i, 0))
        .collect::<HashSet<LR0Item>>();

    let mut result = start;

    loop {
        let mut update = HashSet::new();
        for (sym, term) in result.iter().flat_map(|x| grammar[x.rule_index].body.get(x.dot)) {
            update = update.union(&grammar.iter().enumerate().filter(|(_, x)| x.head == *sym && !term)
                .map(|(i, _)| LR0Item::new(i, 0)).collect()).cloned().collect();
        }
        update = update.union(&complete(grammar, &result, &result, false)).cloned().collect();
        
        if update.is_subset(&result) {
            break
        } else {
            result = result.union(&update).cloned().collect();
        }
    }
    result
}

fn complete(
    grammar: &Grammar,
    q: &HashSet<LR0Item>,
    r: &HashSet<LR0Item>,
    chained: bool,
) -> HashSet<LR0Item> {
    let mut result = HashSet::new();

    let terminated_items = r.iter().filter(|x| is_final(grammar, x));
    let terminated_symbols: HashSet<String> = if chained {
        terminated_items.flat_map(|x| {
            let mut set = find_reductions(grammar, grammar[x.rule_index].head.clone());
            set.insert(grammar[x.rule_index].head.clone());
            set
        }).collect()
    } else {
        terminated_items.map(|x| grammar[x.rule_index].head.clone()).collect()
    };

    for terminated_item in terminated_symbols {   
        for unfinished_item in q.iter().filter(|x| !is_final(grammar, x)) {
            // Retrieve the token that is currently read
            if let Some((token, term)) = &grammar[unfinished_item.rule_index].body.get(unfinished_item.dot) {
                // If the tokens match
                if !*term && *token == terminated_item {
                    result.insert(skip_epsilon(grammar, LR0Item::new(unfinished_item.rule_index, unfinished_item.dot + 1)));
                }
            }
        }
    }

    result
}

pub fn predict(grammar: &Grammar, input: &HashSet<String>) -> HashSet<LR0Item> {
    input.iter().flat_map(|x| find_derivations(grammar, x.clone())).collect()
}

fn scan(grammar: &Grammar, previous: &HashSet<LR0Item>, word: &str) -> HashSet<LR0Item> {
    let mut result = HashSet::new();
    for item in previous.iter() {
        let rule = &grammar[item.rule_index];
        if let Some((token, _)) = &rule.body.get(item.dot) {
            if word == token {
                result.insert(skip_epsilon(grammar, LR0Item::new(item.rule_index, item.dot + 1)));
            }
        }
    }
    result
}

pub fn parse(grammar: &Grammar, words: &Vec<&str>) -> bool {
    let n = words.len();
    let mut t: Vec<Vec<HashSet<LR0Item>>> = vec![vec![HashSet::new(); n + 1]; n + 1];
    let mut set = HashSet::new();
    set.insert(grammar[0].head.clone());
    t[0][0] = predict(&grammar, &set);
    println!("t[0][0] = {:?}", t[0][0]);

    for j in 1..(n + 1) {
        // Scan
        for i in 0..j {
            t[i][j] = scan(grammar, &t[i][j - 1], words[j - 1]);
            println!("t[{}, {}] = {:?}", i, j, t[i][j]);
        }

        // Complete
        for k in (0..j).rev() {
            let result = complete(grammar, &t[k][k], &t[k][j], true);

            t[k][j] = t[k][j].union(&result).cloned().collect();
            for i in (0..k).rev() {
                let result = complete(grammar, &t[i][k], &t[k][j], false);
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

        t[j][j] = predict(grammar, &ts.iter()
            .flat_map(|x| grammar[x.rule_index].body.get(x.dot))
            .filter(|(_, term)| !*term)
            .map(|(sym, _)| sym.clone()).collect());

        println!("t[{}, {}] = {:?}", j, j, t[j][j]);
    }

    println!("{}", fmt_tex_lr0_matrix(grammar, t.clone()));

    let start_symbol = &grammar[0].head;
    grammar
        .iter()
        .enumerate()
        .filter(|(_, x)| x.head == *start_symbol)
        .map(|(i, _)| LR0Item::new(i, grammar[i].body.len()))
        .any(|x| t[0][n].contains(&x))
}

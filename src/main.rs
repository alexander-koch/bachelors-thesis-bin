use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::rc::Rc;

pub mod ebnf;
pub mod lexer;

pub mod harrison;
pub mod ll;
pub mod lr;

use lr::LRParser;

pub mod earley;
use earley::EarleyParser;

use std::collections::HashSet;

use docopt::Docopt;
use serde::Deserialize;

const VERSION: Option<&'static str> = option_env!("CARGO_PKG_VERSION");
const USAGE: &'static str = "
thesis_bin.
Implementation of context-free parsing algorithms.
Copyright (c) Alexander Koch 2019

Usage:
  thesis_bin earley <grammar>
  thesis_bin ll1 <grammar>
  thesis_bin lr1 <grammar>
  thesis_bin harrison <grammar>
  thesis_bin firstfollow <grammar>
  thesis_bin (-h | --help)
  thesis_bin --version

Options:
  -h --help     Show this screen.
  --version     Show version.
";

#[derive(Debug, Deserialize)]
struct Args {
    arg_grammar: Option<String>,
    cmd_earley: bool,
    cmd_ll1: bool,
    cmd_lr1: bool,
    cmd_harrison: bool,
    cmd_firstfollow: bool,
    flag_version: bool,
}

fn main() {
    env_logger::init();

    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());

    if args.flag_version {
        println!("thesis_bin v{}", VERSION.unwrap_or("-unknown"));
        std::process::exit(0);
    }

    let grammar_path = args.arg_grammar.unwrap();
    let grammar = match ebnf::parse_grammar(&grammar_path) {
        Ok(x) => Rc::new(x),
        Err(x) => panic!("{:?}", x),
    };

    for (i, rule) in grammar.iter().enumerate() {
        println!("{}. {}", i, rule);
    }

    if args.cmd_firstfollow {
        let mut ff = ll::FFSets::new(&grammar);
        let symbols = &grammar
            .iter()
            .map(|x| x.head.clone())
            .collect::<HashSet<String>>();
        for symbol in symbols.iter() {
            let first = ff.first(&symbol);
            println!("FIRST({}) = {:?}", symbol, first);
        }

        for symbol in symbols.iter() {
            let follow = ff.follow(&symbol);
            println!("FOLLOW({}) = {:?}", symbol, follow);
        }
        std::process::exit(0);
    }

    let mut earley_parser = if args.cmd_earley {
        Some(EarleyParser::new(&grammar))
    } else {
        None
    };

    let mut ll_table = if args.cmd_ll1 {
        let mut ff = ll::FFSets::new(&grammar);
        let table = ff.construct_ll_table();
        println!("Table: {:?}", table);
        Some(table)
    } else {
        None
    };

    let mut lr_table = if args.cmd_lr1 {
        let mut ff = ll::FFSets::new(&grammar);
        let table = ff.compute_states();
        println!("Table: {:?}", table);
        Some(table)
    } else {
        None
    };

    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let words: Vec<&str> = line.split_whitespace().collect();

                println!("{:?}", words);

                let result = if args.cmd_earley {
                    let states = earley_parser.as_mut().unwrap().analyze(&words);
                    println!("{}", earley::fmt_state_set_list(&grammar, &states));

                    //let mut fb = earley::ForestBuilder::new();
                    //fb.build_forest(&grammar, &states);

                    EarleyParser::accepts(&states, &words)
                } else if args.cmd_ll1 {
                    ll::parse_ll(&grammar, ll_table.as_mut().unwrap(), &words)
                } else if args.cmd_lr1 {
                    lr::parse_lr(&grammar, lr_table.as_mut().unwrap(), &words)
                } else {
                    harrison::parse(&grammar, &words)
                };

                println!("w in L(G): {}", result);
            }
            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}

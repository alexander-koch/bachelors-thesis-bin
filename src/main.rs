use clap::{App, Arg};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::collections::HashSet;
use std::rc::Rc;
use std::time::Instant;

pub mod ebnf;
pub mod lexer;

pub mod earley;
pub mod harrison;
pub mod ll;
pub mod lr;
pub mod sppf;
pub mod util;

use earley::EarleyParser;
use ebnf::Grammar;
use harrison::HarrisonParser;
use lr::LRParser;
use util::ToPrettyTable;

const VERSION: Option<&'static str> = option_env!("CARGO_PKG_VERSION");

#[derive(Debug, Clone)]
enum ParsingEngine {
    Earley(Box<EarleyParser>),
    Harrison(Box<HarrisonParser>),
    LL1(Box<ll::LLTable>),
    LR1(Box<lr::LRTable>),
}

#[derive(Debug, Clone)]
struct ParsingContext {
    engine: ParsingEngine,
    output: Option<String>,
}

fn eval_steps(grammar: &Grammar, steps: &Result<Vec<usize>, ()>) -> bool {
    match steps {
        Ok(steps) => {
            for index in steps.iter() {
                println!("Apply: ({}) {}", index, grammar[*index]);
            }
            true
        }
        Err(()) => false,
    }
}

fn check_word(grammar: &Grammar, ctx: &mut ParsingContext, word: &str) {
    let words: Vec<&str> = word.split_whitespace().collect();
    println!("{:?}", words);

    let now = Instant::now();

    let result = match &mut ctx.engine {
        ParsingEngine::Earley(earley_parser) => {
            let states = earley_parser.as_mut().analyze(&words);
            //println!("{}", earley::fmt_tex_state_set_list(grammar, &states));

            if let Some(ref output) = ctx.output {
                let mut fb = sppf::ForestBuilder::new();
                let forest = fb.build_forest(grammar, &states);

                if let Err(x) = sppf::render_sppf(&output, grammar, &forest) {
                    println!("{}", x);
                }
            }

            EarleyParser::accepts(&states, &words)
        }
        ParsingEngine::Harrison(harrison_parser) => harrison_parser.as_mut().accepts(&words),
        ParsingEngine::LL1(ll_table) => {
            eval_steps(grammar, &ll::parse_ll(grammar, ll_table.as_mut(), &words))
        }
        ParsingEngine::LR1(lr_table) => {
            eval_steps(grammar, &lr::parse_lr(&grammar, lr_table.as_mut(), &words))
        }
    };

    let duration = now.elapsed();

    println!("w in L(G): {}", result);
    println!("Finished in {:?}", duration);
}

fn run_repl(grammar: &Grammar, ctx: &mut ParsingContext) {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_ref());
                check_word(grammar, ctx, &line);
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

fn main() {
    env_logger::init();
    let version_str = format!("v{}", VERSION.unwrap_or("-unknown"));

    let matches = App::new("thesis_bin")
        .version(version_str.as_str())
        .author("Alexander Koch <kochalexander@gmx.net")
        .about("Implementation of context-free parsing algorithms.")
        .arg(
            Arg::with_name("parser")
                .short("p")
                .long("parser")
                .value_name("parser")
                .help("Select the parsing engine to use")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("grammar")
                .short("g")
                .long("grammar")
                .value_name("grammar")
                .help("Grammar file to use")
                .required(true)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("input")
                .help("Input word")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .value_name("output")
                .help("Output file to write Graphviz DOT file")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("firstfollow")
                .short("f")
                .long("firstfollow")
                .value_name("firstfollow")
                .help("Compute the FIRST and FOLLOW sets")
                .takes_value(false),
        )
        .get_matches();

    let grammar_path = matches.value_of("grammar").unwrap();
    let grammar = match ebnf::parse_grammar(&grammar_path) {
        Ok(x) => Rc::new(x),
        Err(x) => {
            println!("{}", x);
            std::process::exit(1);
        }
    };

    for (i, rule) in grammar.iter().enumerate() {
        println!("{}. {}", i, rule);
    }

    if matches.is_present("firstfollow") {
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
    }

    let parser = matches.value_of("parser").unwrap_or("earley");
    let engine = match parser {
        "earley" => ParsingEngine::Earley(Box::new(EarleyParser::new(&grammar))),
        "harrison" => ParsingEngine::Harrison(Box::new(HarrisonParser::new(&grammar))),
        "ll1" => {
            let mut ff = ll::FFSets::new(&grammar);
            let table = ff.construct_ll_table();

            let prettytable = table.to_pretty_table();
            prettytable.printstd();

            ParsingEngine::LL1(Box::new(table))
        }
        "lr1" => {
            let mut ff = ll::FFSets::new(&grammar);
            let table = ff.compute_states();

            let prettytable = table.to_pretty_table();
            prettytable.printstd();

            ParsingEngine::LR1(Box::new(table))
        }
        _ => panic!("Unknown parsing engine"),
    };

    let mut ctx = ParsingContext {
        engine: engine,
        output: matches.value_of("output").map(String::from),
    };

    if let Some(input) = matches.value_of("input") {
        check_word(&grammar, &mut ctx, input);
    } else {
        run_repl(&grammar, &mut ctx);
    }
}

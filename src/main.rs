
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::rc::Rc;

pub mod lexer;
pub mod ebnf;

pub mod earley;
use earley::EarleyParser;

fn main() {
    env_logger::init();

    let grammar = Rc::new(ebnf::parse_grammar("examples/grammar5.txt").ok().unwrap());
    for (i, rule) in grammar.iter().enumerate() {
        println!("{}. {}", i+1, rule);
    }

    let mut parser = EarleyParser::new(&grammar);

    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let words: Vec<&str> = line
                    .split_whitespace()
                    .collect();
                
                let states = parser.analyze(&words);
                println!("{}", earley::fmt_state_set_list(&grammar, &states));

                let result = EarleyParser::accepts(&states, &words);
                println!("w in L(G): {}", result);
            },
            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
}

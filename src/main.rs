
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::rc::Rc;

pub mod lexer;
pub mod ebnf;

pub mod ll;

pub mod earley;
use earley::EarleyParser;

fn main() {
    env_logger::init();

    let grammar = match ebnf::parse_grammar("arith.txt") {
        Ok(x) => Rc::new(x),
        Err(x) => { panic!("{:?}", x) }
    };

    for (i, rule) in grammar.iter().enumerate() {
        println!("{}. {}", i, rule);
    }

    let mut ll = ll::FFSets::new(&grammar);
    /*let symbols = grammar.iter().map(|x| x.head.clone()).collect::<HashSet<String>>();
    for symbol in symbols.iter() {
        let first = ll.first(&symbol);
        println!("FIRST({}) = {:?}", symbol, first);
    }

    for symbol in symbols.iter() {
        let follow = ll.follow(&symbol);
        println!("FOLLOW({}) = {:?}", symbol, follow);
    }*/

    let table = ll.construct_ll_table();
    println!("Table: {:?}", table);

    //let accept = lr::parse_ll(&grammar, &table, vec!["a", "+", "a"]);

    //let mut parser = EarleyParser::new(&grammar);

    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let words: Vec<&str> = line
                    .split_whitespace()
                    .collect();

                println!("{:?}", words);
                
                /*let states = parser.analyze(&words);
                println!("{}", earley::fmt_state_set_list(&grammar, &states));

                let result = EarleyParser::accepts(&states, &words);*/

                let result = ll::parse_ll(&grammar, &table, &words);

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

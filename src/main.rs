
use std::io;
use std::fs::File;
use std::io::Read;

pub mod lexer;
use lexer::Lexer;

pub mod parser;
use parser::Parser;

#[derive(Debug)]
enum ParseError {
    IOError(io::Error),
    LexisError(lexer::Error),
}

impl From<io::Error> for ParseError {
    fn from(err: io::Error) -> ParseError {
        ParseError::IOError(err)
    }
}

impl From<lexer::Error> for ParseError {
    fn from(err: lexer::Error) -> ParseError {
        ParseError::LexisError(err)
    }
}

fn parse_grammar(path: &str) -> Result<(), ParseError> {
    let mut file = File::open(path)?;
    let mut content = String::new();
    try!(file.read_to_string(&mut content));

    let mut lexer = Lexer::new(&content);
    let tokens = try!(lexer.run());

    for token in tokens.clone() {
        println!("{}", token);
    }

    let mut parser = Parser::new(tokens.into_iter());
    let grammar = try!(parser.parse());
    for rule in grammar {
        println!("{}", rule);
    }

    Ok(())
}

fn main() {
    let grammar = parse_grammar("grammar2.txt");
    println!("{:?}", grammar);
}

//! Parser for Extended Backus-Naur Form
use crate::lexer::{Error, Lexer, Position, Token, TokenType};
use log::debug;
use std::fmt;
use std::fs;
use std::io;
use std::iter::Peekable;

use std::collections::HashSet;
use std::ops::Index;

#[derive(Debug, Clone)]
pub struct Rule {
    pub head: String,
    pub body: Vec<(String, bool)>,
}

fn terminalize(s: &str, t: bool) -> String {
    if t {
        format!("'{}'", s)
    } else {
        s.to_owned()
    }
}

impl Rule {
    pub fn fmt_dot(&self, dot: usize) -> String {
        let rhs = self
            .body
            .iter()
            .map(|(s, t)| terminalize(s, *t))
            .enumerate()
            .map(|(i, x)| if i == dot { format!("•{}", x) } else { x })
            .collect::<Vec<String>>()
            .join(" ");

        let last = if dot >= self.body.len() { "•" } else { "" };

        format!("{} -> {}{}", self.head, rhs, last)
    }

    pub fn fmt_tex_dot(&self, dot: usize) -> String {
        let rhs = self
            .body
            .iter()
            .enumerate()
            .map(|(i, x)| {
                if i == dot {
                    format!("\\bigdot {}", x.0)
                } else {
                    x.0.clone()
                }
            })
            .collect::<Vec<String>>()
            .join(" ");

        let last = if dot >= self.body.len() {
            " \\bigdot"
        } else {
            ""
        };

        format!("{} \\rightarrow {}{}", self.head, rhs, last)
    }
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} -> {} .",
            self.head,
            self.body
                .iter()
                .map(|(s, t)| terminalize(s, *t))
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

#[derive(Debug, Clone)]
pub struct Grammar {
    pub nonterminals: HashSet<String>,
    pub terminals: HashSet<String>,
    pub rules: Vec<Rule>,
}

impl Grammar {
    pub fn new(rules: Vec<Rule>) -> Grammar {
        Grammar {
            nonterminals: rules.iter().map(|x| x.head.clone()).collect(),
            terminals: rules
                .iter()
                .flat_map(|x| x.body.iter().filter(|(_, t)| *t).map(|(x, _)| x.clone()))
                .collect(),
            rules: rules,
        }
    }

    pub fn get_symbols(&self) -> HashSet<String> {
        self.nonterminals.union(&self.terminals).cloned().collect()
    }

    pub fn iter(&self) -> std::slice::Iter<Rule> {
        self.rules.iter()
    }
}

impl IntoIterator for Grammar {
    type Item = Rule;
    type IntoIter = std::vec::IntoIter<Rule>;

    fn into_iter(self) -> Self::IntoIter {
        self.rules.into_iter()
    }
}

impl Index<usize> for Grammar {
    type Output = Rule;

    fn index(&self, index: usize) -> &Rule {
        &self.rules[index]
    }
}

// pub type Grammar = Vec<Rule>;
pub type ParsingResult<T> = ::std::result::Result<T, Error>;

/// Parser for the extended Backus-Naur form (EBNF)
pub struct EBNFParser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
    current: Token,
}

impl<T: Iterator<Item = Token>> EBNFParser<T> {
    /// Creates a new parser based on an iterator of tokens.
    pub fn new(it: T) -> EBNFParser<T> {
        let mut peek = it.peekable();
        let start = peek.next().unwrap_or(Token {
            typ: TokenType::Eof,
            value: None,
            position: Position::new(-1, -1),
        });
        EBNFParser {
            tokens: peek,
            current: start,
        }
    }

    /// Generates an error object.
    ///
    /// # Arguments
    /// * `message` - Error message do display
    fn err(&self, message: String) -> Error {
        Error {
            message: message,
            position: self.current.position,
        }
    }

    /// 'Bumps' the parser to read the next token.
    /// If there is no token, Token EOF is returned.
    fn bump(&mut self) {
        self.current = self.tokens.next().unwrap_or(Token {
            typ: TokenType::Eof,
            value: None,
            position: Position::new(-1, -1),
        })
    }

    /// If the current token contains a string value it is returned.
    /// Otherwise an empty string is returned.
    fn get_current_value(&self) -> Result<String, Error> {
        self.current
            .value
            .as_ref()
            .map(|x| x.to_owned())
            .ok_or(self.err("Trying to unwrap a reserved token".to_owned()))
    }

    fn expect_type(&mut self, t: TokenType) -> ParsingResult<()> {
        if self.current.typ == t {
            self.bump();
            Ok(())
        } else {
            Err(self.err(format!(
                "Unexpected token `{:?}`, expected: `{:?}`",
                self.current.typ, t
            )))
        }
    }

    /// Production     = Variable "=" Expression "." .
    /// Expression     = Group | Group "|" Expression .
    /// Group          = Term | Term Group | .
    /// Term           = Variable | Literal .
    fn parse_rule(&mut self) -> ParsingResult<Vec<Rule>> {
        let name = self.get_current_value()?;
        self.expect_type(TokenType::Variable)?;
        self.expect_type(TokenType::Assign)?;

        let mut rules = Vec::new();
        let mut body = Vec::new();
        loop {
            match self.current.typ {
                TokenType::Variable => body.push((self.get_current_value()?, false)),
                TokenType::Literal => body.push((self.get_current_value()?, true)),
                TokenType::Alternative => {
                    rules.push(Rule {
                        head: name.clone(),
                        body: body.clone(),
                    });
                    body.clear()
                }
                TokenType::Dot => break,
                _ => return Err(self.err("Invalid token".to_owned())),
            }

            self.bump()
        }

        self.bump();
        rules.push(Rule {
            head: name,
            body: body,
        });

        Ok(rules)
    }

    // While there are rules, parse them
    fn parse_rules(&mut self) -> ParsingResult<Vec<Rule>> {
        let mut rules = Vec::new();
        while self.current.typ != TokenType::Eof {
            let mut rule = self.parse_rule()?;
            rules.append(&mut rule);
        }
        Ok(rules)
    }

    pub fn parse(&mut self) -> ParsingResult<Grammar> {
        let rules = self.parse_rules()?;
        self.expect_type(TokenType::Eof)?;
        Ok(Grammar::new(rules))
    }
}

#[derive(Debug)]
pub enum ParseError {
    IOError(io::Error),
    LexisError(Error),
}

impl From<io::Error> for ParseError {
    fn from(err: io::Error) -> ParseError {
        ParseError::IOError(err)
    }
}

impl From<Error> for ParseError {
    fn from(err: Error) -> ParseError {
        ParseError::LexisError(err)
    }
}

pub fn parse_grammar(path: &str) -> Result<Grammar, ParseError> {
    let content = fs::read_to_string(path)?;
    let mut lexer = Lexer::new(&content);
    let tokens = lexer.run()?;

    for token in tokens.clone() {
        debug!("{}", token);
    }

    let mut parser = EBNFParser::new(tokens.into_iter());
    let grammar = parser.parse()?;
    for (i, rule) in grammar.clone().into_iter().enumerate() {
        debug!("{}. {}", i, rule);
    }

    Ok(grammar)
}

//! Parser for Extended Backus-Naur Form
use crate::lexer::{Error, Position, Token, TokenType, Lexer};
use std::fmt;
use std::iter::Peekable;
use std::fs;
use std::io;
use log::debug;

#[derive(Debug, Clone)]
pub struct Rule {
    pub head: String,
    pub body: Vec<(String, bool)>,
}

fn terminalize(s: &str, t: bool) -> String {
    if t {
        format!("\"{}\"", s)
    } else {
        s.to_owned()
    }
}

impl Rule {
    pub fn fmt_dot(&self, dot: usize) -> String {
        let rhs = self.body
                .iter()
                .map(|(s, t)| terminalize(s, *t))
                .enumerate()
                .map(|(i, x)| if i == dot {
                    format!("•{}", x)
                } else { x })
                .collect::<Vec<String>>()
                .join(" ");

        let last = if dot >= self.body.len() {
            "•"
        } else {
            ""
        };

        format!(
            "{} -> {}{}",
            self.head,
            rhs,
            last
        )
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

pub type Grammar = Vec<Rule>;
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
    fn get_current_value(&mut self) -> String {
        String::from(match self.current.value {
            Some(ref v) => &**v,
            None => "",
        })
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

    // a = "a" a | "a" .
    fn parse_rule(&mut self) -> ParsingResult<Vec<Rule>> {
        let name = self.get_current_value();
        self.expect_type(TokenType::Identifier)?;
        self.expect_type(TokenType::Assign)?;

        let mut rules = Vec::new();
        let mut body = Vec::new();
        loop {
            match self.current.typ {
                TokenType::Identifier => body.push((self.get_current_value(), false)),
                TokenType::Terminal => body.push((self.get_current_value(), true)),
                TokenType::Alternative => {
                    rules.push(Rule {
                        head: name.clone(),
                        body: body.clone(),
                    });
                    body.clear()
                }
                TokenType::Dot => break,
                _ => return Err(self.err("Expected Ident, Terminal or Dot".to_owned())),
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
        //debug!("Parsing");
        let rules = self.parse_rules()?;
        self.expect_type(TokenType::Eof)?;
        Ok(rules)
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
    for (i, rule) in grammar.clone().iter().enumerate() {
        debug!("{}. {}", i, rule);
    }

    Ok(grammar)
}
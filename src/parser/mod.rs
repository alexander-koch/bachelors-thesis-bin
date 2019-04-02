//! Syntax analysis.
use std::iter::Peekable;
use lexer::{Error, Position, Token, TokenType};
use std::fmt;

#[derive(Debug)]
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

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} -> {} .", self.head, self.body
            .iter()
            .map(|(s, t)| terminalize(s, *t))
            .collect::<Vec<String>>()
            .join(" "))
    }
}

pub type Grammar = Vec<Rule>;
pub type ParsingResult<T> = ::std::result::Result<T, Error>;

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
    current: Token
}

impl<T: Iterator<Item = Token>> Parser<T> {
    /// Creates a new parser based on an iterator of tokens.
    pub fn new(it: T) -> Parser<T> {
        let mut peek = it.peekable();
        let start = peek.next().unwrap_or(Token {
            typ: TokenType::Eof,
            value: None,
            position: Position::new(-1, -1),
        });
        Parser {
            tokens: peek,
            current: start
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
        try!(self.expect_type(TokenType::Identifier));
        try!(self.expect_type(TokenType::Assign));

        let mut rules = Vec::new();
        let mut body = Vec::new();
        loop {
            match self.current.typ {
                TokenType::Identifier => body.push((self.get_current_value(), false)),
                TokenType::Terminal => body.push((self.get_current_value(), true)),
                TokenType::Alternative => {
                    rules.push(Rule {
                        head: name.clone(),
                        body: body.clone()
                    });
                    body.clear()
                },
                TokenType::Dot => break,
                _ => return Err(self.err("Expected Ident, Terminal or Dot".to_owned()))
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
            let mut rule = try!(self.parse_rule());
            rules.append(&mut rule);
        }
        Ok(rules)
    }

    pub fn parse(&mut self) -> ParsingResult<Grammar> {
        //debug!("Parsing");
        let rules = try!(self.parse_rules());
        try!(self.expect_type(TokenType::Eof));
        Ok(rules)
    }
}
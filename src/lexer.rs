//! Lexical analysis.

use crate::util;
use std::fmt;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum TokenType {
    Eof,
    Variable,
    Literal,
    Alternative,
    Assign,
    Dot,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub struct Position {
    pub line: i32,
    pub column: i32,
}

impl Position {
    pub fn new(line: i32, column: i32) -> Position {
        Position {
            line: line,
            column: column,
        }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Error {
    pub message: String,
    pub position: Position,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} - {}", self.position, self.message)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Token {
    pub typ: TokenType,
    pub value: Option<String>,
    pub position: Position,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Token ({:?}, {:?})", self.typ, self.value)
    }
}

pub type LexicalResult<T> = ::std::result::Result<T, Error>;

pub struct Lexer<'a> {
    data: &'a str,
    cursor: usize,
    current: Option<char>,
    position: Position,
}

impl<'a> Lexer<'a> {
    /// Creates a new lexer structure
    ///
    /// # Arguments
    ///
    /// * `src` - The source code string.
    ///
    pub fn new(src: &str) -> Lexer<'_> {
        let first = src.chars().next();
        Lexer {
            data: src,
            cursor: 0,
            current: first,
            position: Position::new(1, 1),
        }
    }

    /// Consume a character
    fn consume(&mut self) {
        self.cursor += self.current.unwrap().len_utf8();
        if self.cursor < self.data.len() {
            let ch = get_char(&self.data, self.cursor);
            self.position.column += 1;
            self.current = Some(ch);
        } else {
            self.current = None;
        }
    }

    /// Test if the current character is equal to c
    fn curr_is(&self, c: char) -> bool {
        self.current == Some(c)
    }

    /// Peek in front of the buffer, at a certain offset
    fn peek(&mut self, offset: usize) -> Option<char> {
        let incr = self.current.unwrap().len_utf8() * offset;
        if self.cursor + incr < self.data.len() {
            Some(get_char(&self.data, self.cursor + incr))
        } else {
            None
        }
    }

    fn err(&self, msg: &str, position: Position) -> Error {
        Error {
            message: msg.into(),
            position: position,
        }
    }

    fn is_ident(&mut self) -> bool {
        self.current
            .map(|x| x.is_alphanumeric() || x == '_')
            .unwrap_or(false)
    }

    fn skip_space(&mut self) {
        if self.current == Some('\r') && self.peek(1) == Some('\n') {
            self.consume();
            self.consume();
            self.position.line += 1;
            self.position.column = 1;
        } else if self.current == Some('\r') {
            self.consume();
            self.position.column = 1;
        } else if self.current == Some('\n') {
            self.consume();
            self.position.line += 1;
            self.position.column = 1;
        } else {
            self.consume();
        }
    }

    /// Tests if the current character is a whitespace
    fn is_space(&mut self) -> bool {
        self.current.map(|x| x.is_whitespace()).unwrap_or(false)
    }

    fn is_punctuation(&mut self) -> bool {
        self.current
            .map(|c| match c {
                '|' | '=' | '.' | '(' | ')' | '[' | ']' | '{' | '}' => true,
                _ => false,
            })
            .unwrap_or(false)
    }

    fn scan_punctuation(&mut self) -> LexicalResult<Token> {
        let position = self.position;
        let c = match self.current {
            Some(v) => v,
            None => return Err(self.err("reached end of file", position)),
        };
        self.consume();

        let kind = match c {
            '|' => TokenType::Alternative,
            '=' => TokenType::Assign,
            '.' => TokenType::Dot,
            '(' => TokenType::LParen,
            ')' => TokenType::RParen,
            '[' => TokenType::LBracket,
            ']' => TokenType::RBracket,
            '{' => TokenType::LBrace,
            '}' => TokenType::RBrace,
            _ => return Err(self.err("unknown punctuation", position)),
        };

        Ok(Token {
            typ: kind,
            value: None,
            position: position,
        })
    }

    fn scan_literal(&mut self) -> LexicalResult<Token> {
        let position = self.position;
        self.consume();

        // Begin scanning
        let start = self.cursor;
        let mut escaped = false;

        while let Some(c) = self.current {
            if c == '"' && !escaped {
                break;
            }

            if c == '\\' {
                escaped = true;
            } else {
                escaped = false;
            }

            self.consume();
        }

        if self.current.is_none() {
            return Err(self.err("reached end of file, literal is not ending", self.position));
        }

        let s = util::unescape(&self.data[start..self.cursor]);
        self.consume();
        Ok(Token {
            typ: TokenType::Literal,
            value: Some(s),
            position: position,
        })
    }

    fn scan_variable(&mut self) -> LexicalResult<Token> {
        let start = self.cursor;
        let position = self.position;
        while self.is_ident() {
            self.consume()
        }
        while self.curr_is('\'') {
            self.consume()
        }

        // Test for builtin-types
        let s = &self.data[start..self.cursor];

        Ok(Token {
            typ: TokenType::Variable,
            value: Some(s.to_owned()),
            position: position,
        })
    }

    fn next_token(&mut self) -> LexicalResult<Token> {
        let position = self.position;
        if self.is_space() {
            self.skip_space();
            self.next_token()
        } else if self.curr_is('"') {
            self.scan_literal()
        } else if self.is_punctuation() {
            self.scan_punctuation()
        } else if self.is_ident() {
            self.scan_variable()
        } else {
            Err(self.err("invalid character", position))
        }
    }

    /// Tokenizes the source code into a vector of tokens
    pub fn run(&mut self) -> LexicalResult<Vec<Token>> {
        let mut tokens = Vec::new();

        // Read all the tokens
        while self.cursor < self.data.len() {
            tokens.push(self.next_token()?);
        }

        Ok(tokens)
    }
}

fn get_char(s: &str, byte: usize) -> char {
    s[byte..].chars().next().unwrap()
}

//! Parser for Extended Backus-Naur Form
use crate::lexer::{Error, Lexer, Position, Token, TokenType};
use log::debug;
use std::fmt;
use std::fs;
use std::io;
use std::iter::Peekable;

use std::collections::HashSet;
use std::ops::Index;

use log::trace;

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

#[derive(Debug)]
pub enum GrammarError {
    IOError(io::Error),
    LexisError(Error),
    SyntaxError(Error),
    SemanticError(String),
}

impl From<io::Error> for GrammarError {
    fn from(err: io::Error) -> GrammarError {
        GrammarError::IOError(err)
    }
}

impl fmt::Display for GrammarError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GrammarError::IOError(e) => write!(f, "IO error: {}", e),
            GrammarError::LexisError(e) => write!(f, "Lexical error: {}", e),
            GrammarError::SyntaxError(e) => write!(f, "Syntax error: {}", e),
            GrammarError::SemanticError(e) => write!(f, "Semantic error: {}", e),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTCollector {
    pub id: usize,
    pub rules: Vec<Rule>,
}

impl ASTCollector {
    pub fn new() -> ASTCollector {
        ASTCollector {
            id: 0,
            rules: Vec::new(),
        }
    }

    fn push_id(&mut self) -> String {
        let id = self.id;
        self.id += 1;
        format!("r{}", id)
    }

    fn push_rules(&mut self, alts: Vec<Vec<(String, bool)>>) -> String {
        let name = self.push_id();
        for alt in alts {
            let rule = Rule {
                head: name.clone(),
                body: alt,
            };
            self.rules.push(rule);
        }
        name
    }

    fn visit_term(&mut self, term: &ASTTerm) -> (String, bool) {
        match term {
            ASTTerm::Variable(x) => (x.clone(), false),
            ASTTerm::Literal(x) => (x.clone(), true),
            ASTTerm::Group(x) => {
                let alts = self.visit_expr(x);
                let id = self.push_rules(alts);
                (id, false)
            }
            ASTTerm::Optional(x) => {
                let alts = self.visit_expr(x);
                let id = self.push_rules(alts);
                self.rules.push(Rule {
                    head: id.clone(),
                    body: Vec::new(),
                });
                (id, false)
            }
            ASTTerm::Repetition(x) => {
                let alts = self.visit_expr(x);
                let indirect_id = self.push_rules(alts);
                let direct_id = self.push_id();

                self.rules.push(Rule {
                    head: direct_id.clone(),
                    body: vec![(indirect_id, false), (direct_id.clone(), false)],
                });
                self.rules.push(Rule {
                    head: direct_id.clone(),
                    body: Vec::new(),
                });
                (direct_id, false)
            }
        }
    }

    fn visit_alt(&mut self, alt: &ASTAlternative) -> Vec<(String, bool)> {
        let mut items = Vec::new();
        for term in alt {
            items.push(self.visit_term(&term));
        }
        items
    }

    fn visit_expr(&mut self, expr: &ASTExpr) -> Vec<Vec<(String, bool)>> {
        expr.iter().map(|x| self.visit_alt(x)).collect()
    }

    fn visit_rule(&mut self, rule: &ASTRule) -> Vec<Rule> {
        self.visit_expr(&rule.expr)
            .iter()
            .map(|x| Rule {
                head: rule.name.clone(),
                body: x.clone(),
            })
            .collect()
    }

    pub fn visit_grammar(&mut self, grammar: &ASTGrammar) -> Vec<Rule> {
        self.rules.clear();

        let mut found_rules: Vec<Rule> = grammar.iter().flat_map(|x| self.visit_rule(&x)).collect();

        found_rules.extend(self.rules.iter().cloned());
        found_rules
    }
}

#[derive(Debug, Clone)]
pub enum ASTTerm {
    Variable(String),
    Literal(String),
    Group(Box<ASTExpr>),
    Optional(Box<ASTExpr>),
    Repetition(Box<ASTExpr>),
}

pub type ASTAlternative = Vec<ASTTerm>;
pub type ASTExpr = Vec<ASTAlternative>;

#[derive(Debug, Clone)]
pub struct ASTRule {
    pub name: String,
    pub expr: ASTExpr,
}

pub type ASTGrammar = Vec<ASTRule>;

#[derive(Debug, Clone)]
pub struct Grammar {
    pub nonterminals: HashSet<String>,
    pub terminals: HashSet<String>,
    pub rules: Vec<Rule>,
}

impl Grammar {
    pub fn from_ast(ast: &ASTGrammar) -> Result<Grammar, GrammarError> {
        let mut collector = ASTCollector::new();
        Grammar::from_rules(collector.visit_grammar(ast))

        //let mut ctx = Context::new();
        //Grammar::from_rules(ast.iter().flat_map(|x| x.desugar(&mut ctx)).collect())
    }

    pub fn from_rules(rules: Vec<Rule>) -> Result<Grammar, GrammarError> {
        // Collect non-terminals
        let nonterminals: HashSet<_> = rules.iter().map(|x| x.head.clone()).collect();
        let mut terminals = HashSet::new();

        // While collecting the terminals
        // find every non-terminal that is used but not defined
        for rule in rules.iter() {
            for (symbol, term) in rule.body.iter() {
                if *term {
                    terminals.insert(symbol.clone());
                } else {
                    if !nonterminals.contains(symbol) {
                        return Err(GrammarError::SemanticError(format!(
                            "undefined symbol '{}' is used in rule '{}'",
                            symbol, rule
                        )));
                    }
                }
            }
        }

        Ok(Grammar {
            nonterminals: nonterminals,
            terminals: terminals,
            rules: rules,
        })
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

pub type ParsingResult<T> = ::std::result::Result<T, GrammarError>;

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
    fn err(&self, message: String) -> GrammarError {
        GrammarError::SyntaxError(Error {
            message: message,
            position: self.current.position,
        })
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
    fn get_current_value(&self) -> Result<String, GrammarError> {
        self.current
            .value
            .as_ref()
            .map(|x| x.to_owned())
            .ok_or(self.err("trying to unwrap a reserved token".to_owned()))
    }

    fn expect_type(&mut self, t: TokenType) -> ParsingResult<()> {
        if self.current.typ == t {
            self.bump();
            Ok(())
        } else {
            Err(self.err(format!(
                "unexpected token '{:?}', expected '{:?}'",
                self.current.typ, t
            )))
        }
    }

    /// Term = VARIABLE | LITERAL | "(" Expr ")" | "[" Expr "]" | "{" Expr "}" .
    fn parse_term(&mut self) -> ParsingResult<ASTTerm> {
        trace!("parse_term");
        Ok(match self.current.typ {
            TokenType::Variable => {
                let value = self.get_current_value()?;
                self.bump();
                ASTTerm::Variable(value)
            }
            TokenType::Literal => {
                let value = self.get_current_value()?;
                self.bump();
                ASTTerm::Literal(value)
            }
            TokenType::LParen => {
                self.bump();
                let expr = self.parse_expr()?;
                self.expect_type(TokenType::RParen)?;
                ASTTerm::Group(Box::new(expr))
            }
            TokenType::LBracket => {
                self.bump();
                let expr = self.parse_expr()?;
                self.expect_type(TokenType::RBracket)?;
                ASTTerm::Optional(Box::new(expr))
            }
            TokenType::LBrace => {
                self.bump();
                let expr = self.parse_expr()?;
                self.expect_type(TokenType::RBrace)?;
                ASTTerm::Repetition(Box::new(expr))
            }
            _ => {
                return Err(
                    self.err("Invalid token, expected, VARIABLE, LITERAL, (, [ or {".to_owned())
                );
            }
        })
    }

    /// Alt = { Term } .
    fn parse_alt(&mut self) -> ParsingResult<ASTAlternative> {
        trace!("parse_alt");
        let mut terms = Vec::new();
        while match self.current.typ {
            TokenType::Variable
            | TokenType::Literal
            | TokenType::LParen
            | TokenType::LBracket
            | TokenType::LBrace => true,
            _ => false,
        } {
            terms.push(self.parse_term()?);
        }

        Ok(terms)
    }

    /// Expr = Alt { "|" Alt } .
    fn parse_expr(&mut self) -> ParsingResult<ASTExpr> {
        trace!("parse_expr");
        let mut alts = vec![self.parse_alt()?];
        while self.current.typ == TokenType::Alternative {
            self.bump();
            alts.push(self.parse_alt()?);
        }
        Ok(alts)
    }

    /// Rule = VARIABLE "=" Expr "." .
    fn parse_rule(&mut self) -> ParsingResult<ASTRule> {
        trace!("parse_rule");
        let name = self.get_current_value()?;
        self.expect_type(TokenType::Variable)?;
        self.expect_type(TokenType::Assign)?;

        let expr = self.parse_expr()?;
        self.expect_type(TokenType::Dot)?;

        Ok(ASTRule {
            name: name,
            expr: expr,
        })
    }

    /// Grammar = Rule { Rule }
    fn parse_grammar(&mut self) -> ParsingResult<ASTGrammar> {
        trace!("parse_grammar");
        let mut rules = Vec::new();
        while self.current.typ == TokenType::Variable {
            rules.push(self.parse_rule()?);
        }
        Ok(rules)
    }

    pub fn parse(&mut self) -> ParsingResult<ASTGrammar> {
        let grammar = self.parse_grammar()?;
        self.expect_type(TokenType::Eof)?;
        Ok(grammar)
    }
}

pub fn parse_grammar(path: &str) -> Result<Grammar, GrammarError> {
    let content = fs::read_to_string(path)?;
    let mut lexer = Lexer::new(&content);
    let tokens = lexer.run().map_err(|x| GrammarError::LexisError(x))?;

    for token in tokens.clone() {
        debug!("{}", token);
    }

    let mut parser = EBNFParser::new(tokens.into_iter());
    let ast = parser.parse()?;
    let grammar = Grammar::from_ast(&ast)?;
    for (i, rule) in grammar.iter().enumerate() {
        debug!("{}. {}", i, rule);
    }

    Ok(grammar)
}

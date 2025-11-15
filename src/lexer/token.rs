use owo_colors::OwoColorize;

use crate::lexer::Location;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Location,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}: {}", self.loc, self.kind)
    }
}

impl Token {
    pub fn new(kind: TokenKind, loc: Location) -> Self {
        Self { kind, loc }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // keywords
    Func,
    Defer,
    Return,

    // literals
    Identifier(String),
    Number(String),

    // arithmetic
    Plus,
    Minus,
    Slash,
    Star,

    // logical
    Not,
    And,
    Or,

    // comparative
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    DoubleEqual,

    // assignment
    Colon,
    DoubleColon,
    Equal,

    // other
    Comma,
    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
    Eof,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenKind::Identifier(name) => {
                write!(f, "{}({:?})", "Identifier".cyan(), name)
            }
            TokenKind::Number(value) => {
                write!(f, "{}({:?})", "Number".cyan(), value)
            }
            _ => write!(f, "{:?}", self.cyan()),
        }
    }
}

#[derive(Clone)]
pub struct TokenList {
    tokens: Vec<Token>,
}

impl std::fmt::Display for TokenList {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for token in &self.tokens {
            write!(f, "{}\n", token)?;
        }
        Ok(())
    }
}

impl TokenList {
    pub fn new() -> Self {
        Self { tokens: Vec::new() }
    }

    pub fn push(&mut self, token: Token) {
        self.tokens.push(token);
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn to_vec(&self) -> Vec<Token> {
        self.tokens.clone()
    }
}

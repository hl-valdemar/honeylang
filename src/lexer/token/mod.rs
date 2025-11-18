use crate::lexer::Location;
use crate::parser::error::ExpectedTokenKind;

mod display;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Location,
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
    Mut,

    // literals
    Identifier(Option<String>),
    Number(Option<String>),
    Boolean(bool),

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
    NotEqual,

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

impl TokenKind {
    pub fn to_expected(&self) -> ExpectedTokenKind {
        match self {
            Self::Identifier(_) => ExpectedTokenKind::Identifier,
            TokenKind::Equal => ExpectedTokenKind::Equal,
            TokenKind::Colon => ExpectedTokenKind::Colon,
            TokenKind::DoubleColon => ExpectedTokenKind::DoubleColon,
            _ => todo!(),
        }
    }
}

#[derive(Clone)]
pub struct TokenList {
    tokens: Vec<Token>,
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

    pub fn as_slice(&self) -> &[Token] {
        self.tokens.as_slice()
    }
}

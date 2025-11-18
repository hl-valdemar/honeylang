use crate::lexer::{Location, token::Token};

mod display;

#[derive(Debug)]
pub enum ParsingError {
    UnexpectedEof,

    UnexpectedToken {
        found: Token,
        expected: Vec<ExpectedTokenKind>,
    },

    ExpectedStatement {
        found: Token,
    },
    ExpectedConstDeclaration {
        found: Token,
    },
    ExpectedExpression {
        found: Token,
    },
    ExpectedType {
        found: Token,
    },

    TrailingComma {
        loc: Location,
    },

    NoValue(NoValueKind),
}

#[derive(Debug, Clone, Copy)]
pub enum ExpectedTokenKind {
    Identifier,
    Number,
    Type,
    Colon,
    DoubleColon,
    Equal,
}

#[derive(Debug)]
pub enum NoValueKind {
    Identifier,
    Number,
}

pub struct ErrorList {
    errors: Vec<ParsingError>,
}

impl ErrorList {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn push(&mut self, error: ParsingError) {
        self.errors.push(error);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

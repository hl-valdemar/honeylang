use crate::lexer::token::TokenKind;

mod display;

#[derive(Debug)]
pub enum ParsingError {
    UnexpectedEof,

    UnexpectedToken {
        found: TokenKind,
        expected: Vec<TokenKind>,
    },

    ExpectedStatement {
        found: TokenKind,
    },
    ExpectedDeclaration {
        found: TokenKind,
    },
    ExpectedExpression {
        found: TokenKind,
    },
    ExpectedType {
        found: TokenKind,
    },
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

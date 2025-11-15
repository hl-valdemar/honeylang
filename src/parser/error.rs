use owo_colors::OwoColorize;

use crate::lexer::token::TokenKind;

pub enum ParsingError {
    UnexpectedToken {
        found: TokenKind,
        expected: TokenKind,
    },
    UnexpectedEof,
}

impl std::fmt::Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken { found, expected } => write!(
                f,
                "unexpected token '{}', expected '{}'",
                found.red(),
                expected.cyan()
            ),
            Self::UnexpectedEof => write!(f, "unexpected EOF"),
        }
    }
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

impl std::fmt::Display for ErrorList {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for error in &self.errors {
            write!(f, "{}: {}\n", "error".red(), error)?;
        }
        Ok(())
    }
}

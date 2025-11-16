use owo_colors::OwoColorize;

use crate::lexer::{
    Token,
    token::{TokenKind, TokenList},
};

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}: {}", self.loc, self.kind)
    }
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

impl std::fmt::Display for TokenList {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for token in &self.tokens {
            write!(f, "{}\n", token)?;
        }
        Ok(())
    }
}

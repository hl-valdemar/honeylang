use owo_colors::OwoColorize;

use crate::lexer::error::{ErrorList, LexingError};

impl std::fmt::Display for LexingError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnexpectedCharacter { name, loc } => {
                write!(f, "unexpected character '{}' at {}", name.red(), loc,)
            }
        }
    }
}

impl std::fmt::Display for ErrorList {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for error in &self.errors {
            write!(f, "{}: {}.\n", "error".red(), error)?;
        }
        Ok(())
    }
}

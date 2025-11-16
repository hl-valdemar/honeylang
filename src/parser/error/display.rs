use owo_colors::OwoColorize;

use crate::parser::error::{ErrorList, ParsingError};

impl std::fmt::Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnexpectedEof => write!(f, "unexpected EOF"),
            Self::UnexpectedToken { found, expected } => {
                write!(f, "unexpected token '{}', expected ", found.red())?;
                for (i, kind) in expected.iter().enumerate() {
                    let is_last = i == expected.len() - 1;
                    if !is_last {
                        write!(f, "'{}', ", kind)?;
                    } else {
                        write!(f, "or '{}'", kind)?;
                    }
                }
                Ok(())
            }
            Self::ExpectedStatement { found } => write!(f, "expected statement, found '{}'", found),
            Self::ExpectedDeclaration { found } => {
                write!(f, "expected declaration, found '{}'", found)
            }
            Self::ExpectedExpression { found } => {
                write!(f, "expected expression, found '{}'", found)
            }
            Self::ExpectedType { found } => write!(f, "expected type, found '{}'", found),
        }
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

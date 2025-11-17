use owo_colors::OwoColorize;

use crate::semantic::error::{ErrorList, FatalError, RecoverableError, SemanticError};

impl std::fmt::Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Recoverable(recoverable) => match recoverable {
                RecoverableError::DuplicateName(name) => {
                    write!(f, "found duplicate name '{}'", name)
                }
                RecoverableError::ExpectedConstDecl { found } => {
                    write!(f, "expected const declaration, found {}", found)
                }
            },
            Self::Fatal(fatal) => match fatal {
                FatalError::ExpectedProgram { found } => {
                    write!(f, "expected program, found {}", found)
                }
            },
        }
    }
}

impl std::fmt::Display for ErrorList {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for error in &self.errors {
            match error {
                SemanticError::Recoverable(_) => write!(f, "{}: {}.\n", "error".red(), error)?,
                SemanticError::Fatal(_) => write!(f, "{}: {}.\n", "fatal".red(), error)?,
            }
        }
        Ok(())
    }
}

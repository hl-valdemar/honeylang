use owo_colors::OwoColorize;

use crate::{
    lexer::token::TokenKind,
    parser::error::{ErrorList, NoValueKind, ParsingError},
};

impl std::fmt::Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnexpectedEof => write!(f, "unexpected EOF"),
            Self::TrailingComma { loc } => write!(f, "trailing comma at {}", loc),
            Self::UnexpectedToken { found, expected } => {
                write!(
                    f,
                    "unexpected token '{}' at {}, expected ",
                    found.kind.red(),
                    found.loc
                )?;
                for (i, kind) in expected.iter().enumerate() {
                    let kind = if matches!(kind, TokenKind::Identifier(_) | TokenKind::Number(_)) {
                        format!("{}", kind.cyan())
                    } else {
                        format!("'{}'", kind.cyan())
                    };

                    let is_last = i == expected.len() - 1;
                    if !is_last {
                        write!(f, "{}, ", kind)?;
                    } else if is_last && expected.len() > 1 {
                        write!(f, "or {}", kind)?;
                    } else {
                        write!(f, "{}", kind)?;
                    }
                }
                Ok(())
            }
            Self::ExpectedStatement { found } => {
                write!(f, "expected statement, found '{}'", found.kind)
            }
            Self::ExpectedConstDeclaration { found } => {
                write!(f, "expected declaration, found '{}'", found.kind)
            }
            Self::ExpectedExpression { found } => {
                write!(f, "expected expression, found '{}'", found.kind)
            }
            Self::ExpectedType { found } => write!(f, "expected type, found '{}'", found.kind),
            Self::NoValue(kind) => write!(f, "expected value for {}", kind),
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

impl std::fmt::Display for NoValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier => write!(f, "identifier"),
            Self::Number => write!(f, "number"),
        }
    }
}

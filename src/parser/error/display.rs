use owo_colors::OwoColorize;

use crate::parser::error::{ErrorList, ExpectedTokenKind, NoValueKind, ParsingError};

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
                    let kind = if matches!(
                        kind,
                        ExpectedTokenKind::Identifier | ExpectedTokenKind::Number
                    ) {
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
                write!(
                    f,
                    "unexpected token '{}' at {}, expected {}",
                    found.kind.red(),
                    found.loc,
                    "declaration".purple(),
                )
            }
            Self::ExpectedConstDeclaration { found } => {
                write!(
                    f,
                    "unexpected token '{}' at {}, expected {}",
                    found.kind.red(),
                    found.loc,
                    "const declaration".purple(),
                )
            }
            Self::ExpectedExpression { found } => {
                write!(
                    f,
                    "unexpected token '{}' at {}, expected {}",
                    found.kind.red(),
                    found.loc,
                    "expression".purple(),
                )
            }
            Self::ExpectedType { found } => write!(
                f,
                "unexpected token '{}' at {}, expected {}",
                found.kind.red(),
                found.loc,
                "type".purple(),
            ),
            Self::NoValue(kind) => write!(f, "expected value for {}", kind),
        }
    }
}

impl std::fmt::Display for ExpectedTokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier => write!(f, "{}", "identifier".purple()),
            Self::Number => write!(f, "{}", "number".purple()),
            Self::Type => write!(f, "{}", "type".purple()),
            Self::Colon => write!(f, "{}", ":".purple()),
            Self::DoubleColon => write!(f, "{}", "::".purple()),
            Self::Equal => write!(f, "{}", "=".purple()),
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

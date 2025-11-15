use owo_colors::OwoColorize;

use crate::lexer::Location;

#[derive(Debug)]
pub enum LexingError {
    UnexpectedCharacter { name: String, loc: Location },
}

impl std::fmt::Display for LexingError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnexpectedCharacter { name, loc } => write!(
                f,
                "unexpected character '{}' at {}:{}:{}",
                name.red(), loc.filename, loc.line, loc.col
            ),
        }
    }
}

pub struct ErrorList {
    errors: Vec<LexingError>,
}

impl std::fmt::Display for ErrorList {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for error in &self.errors {
            write!(f, "{}: {}\n", "error".red(), error)?;
        }
        Ok(())
    }
}

impl ErrorList {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn push(&mut self, error: LexingError) {
        self.errors.push(error);
    }
}

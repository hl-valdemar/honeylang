use crate::lexer::Location;

mod display;

#[derive(Debug)]
pub enum LexingError {
    UnexpectedCharacter { name: String, loc: Location },
}

pub struct ErrorList {
    errors: Vec<LexingError>,
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

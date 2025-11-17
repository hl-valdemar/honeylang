use crate::parser::ast::AstNode;

mod display;

#[derive(Clone)]
pub enum SemanticError {
    Recoverable(RecoverableError),
    Fatal(FatalError),
}

#[derive(Clone)]
pub enum RecoverableError {
    DuplicateName(String),
    ExpectedConstDecl { found: AstNode },
}

#[derive(Clone)]
pub enum FatalError {
    ExpectedProgram { found: AstNode },
}

pub struct ErrorList {
    errors: Vec<SemanticError>,
}

impl ErrorList {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn push(&mut self, error: SemanticError) {
        self.errors.push(error);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

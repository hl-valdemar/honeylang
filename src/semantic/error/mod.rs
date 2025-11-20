use crate::parser::ast::{AstNode, ConstDeclKind};

mod display;

#[derive(Clone)]
pub enum SemanticError {
    Recoverable(RecoverableError),
    Fatal(FatalError),
}

#[derive(Clone)]
pub enum RecoverableError {
    DuplicateSymbol(String),
    UndefinedSymbol(String),
    ExpectedConstDecl { found: AstNode },
    InvalidOperandType { expected: String, found: String },
    CannotNegateUnsignedType,
    CannotNegateBool,
    InvalidNumberLiteral,
    TypeMismatch { left: String, right: String },
    InvalidTypeConversion { from: String, to: String },
}

#[derive(Clone)]
pub enum FatalError {
    ExpectedProgram {
        found: AstNode,
    },
    CircularComptimeDependency {
        sym_name: String,
        sym_kind: ConstDeclKind,
    },
    DivisionByZero,
    CannotInferType,
}

pub struct ErrorList {
    errors: Vec<RecoverableError>,
}

impl ErrorList {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn push(&mut self, error: RecoverableError) {
        self.errors.push(error);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

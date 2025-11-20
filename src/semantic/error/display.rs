use owo_colors::OwoColorize;

use crate::{
    parser::ast::ConstDeclKind,
    semantic::error::{ErrorList, FatalError, RecoverableError, SemanticError},
};

impl std::fmt::Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Recoverable(recoverable) => write!(f, "{}", recoverable),
            Self::Fatal(fatal) => write!(f, "{}", fatal),
        }
    }
}

impl std::fmt::Display for RecoverableError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DuplicateSymbol(name) => write!(f, "found duplicate name '{}'", name),
            Self::UndefinedSymbol(name) => write!(f, "use of undefined symbol '{}'", name),
            Self::ExpectedConstDecl { found } => {
                write!(f, "expected const declaration, found {:?}", found)
            }
            Self::InvalidOperandType { expected, found } => write!(
                f,
                "invalid operand type: expected '{}', found '{}'",
                expected, found
            ),
            Self::CannotNegateUnsignedType => write!(f, "cannot negate unsigned integer type"),
            Self::CannotNegateBool => write!(f, "cannot apply arithmetic negation to boolean"),
            Self::InvalidNumberLiteral => write!(f, "invalid number literal"),
            Self::TypeMismatch { left, right } => write!(
                f,
                "type mismatch in binary operation: {} and {}",
                left, right
            ),
            Self::InvalidTypeConversion { from, to } => {
                write!(f, "invalid type conversion from '{}' to '{}'", from, to)
            }
        }
    }
}

impl std::fmt::Display for FatalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedProgram { found } => write!(f, "expected program, found {:?}", found),
            Self::CircularComptimeDependency { sym_name, sym_kind } => write!(
                f,
                "circular dependency detected in {} '{}'",
                sym_kind, sym_name
            ),
            Self::DivisionByZero => write!(f, "division by zero"),
            Self::CannotInferType => write!(f, "cannot infer type"),
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

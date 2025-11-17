use crate::parser::{
    AstNode,
    ast::{BinaryOpKind, ConstDeclKind, TreeDisplay, Type, UnaryOpKind},
};

impl std::fmt::Display for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.fmt_tree(f, "", true)
    }
}

impl std::fmt::Display for UnaryOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::LogicalNot => write!(f, "!"),
            Self::ArithmeticNeg => write!(f, "-"),
        }
    }
}

impl std::fmt::Display for BinaryOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // arithmetic
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),

            // comparative
            Self::Less => write!(f, "<"),
            Self::Greater => write!(f, ">"),
            Self::LessEqual => write!(f, "<="),
            Self::GreaterEqual => write!(f, ">="),
            Self::Equal => write!(f, "=="),
            Self::Different => write!(f, "!="),

            // logical
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
        }
    }
}

impl std::fmt::Display for ConstDeclKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Const => write!(f, "const"),
            Self::Func => write!(f, "func"),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Named(type_) => write!(f, "{}", type_),
        }
    }
}

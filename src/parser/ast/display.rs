use crate::parser::{
    AstNode,
    ast::{
        BinaryOpKind, ConstDeclKind, Number, ResolvedNumber, ResolvedType, TreeDisplay, Type,
        UnaryOpKind,
    },
};

impl std::fmt::Display for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.fmt_tree(f, "", true)
    }
}

impl std::fmt::Display for UnaryOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::LogicalNot => write!(f, "not"),
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
            Self::Unresolved(name) => write!(f, "{}", name),
            Self::Resolved(resolved) => write!(f, "{}", resolved),
        }
    }
}

impl std::fmt::Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::F16 => write!(f, "f16"),
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
        }
    }
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unresolved(unresolved) => write!(f, "{}", unresolved),
            Self::Resolved(resolved) => write!(f, "{}", resolved),
        }
    }
}

impl std::fmt::Display for ResolvedNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(val) => write!(f, "{}", val),
            _ => todo!(),
        }
    }
}

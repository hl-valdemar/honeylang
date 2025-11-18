mod display;

pub fn make_identifier(name: &str) -> AstNode {
    AstNode::Ident(name.to_string())
}

pub fn make_number(value: &str) -> AstNode {
    AstNode::Num(Number::Unresolved(value.to_string()))
}

pub fn make_boolean(value: bool) -> AstNode {
    AstNode::Bool(value)
}

pub fn make_unary(op: UnaryOpKind, operand: AstNode) -> AstNode {
    AstNode::UnaryOp {
        op,
        operand: Box::new(operand),
    }
}

pub fn make_binary(op: BinaryOpKind, left: AstNode, right: AstNode) -> AstNode {
    AstNode::BinaryOp {
        op,
        left: Box::new(left),
        right: Box::new(right),
    }
}

pub fn make_const_decl(
    kind: ConstDeclKind,
    name: &str,
    type_: Option<Type>,
    value: AstNode,
) -> AstNode {
    AstNode::ConstDecl {
        kind,
        name: name.to_string(),
        type_,
        value: Box::new(value),
    }
}

#[derive(Debug, Clone)]
pub enum AstNode {
    Program {
        declarations: Vec<AstNode>,
    },
    ConstDecl {
        name: String,
        kind: ConstDeclKind,
        type_: Option<Type>,
        value: Box<AstNode>,
    },
    Ident(String),
    Num(Number),
    Bool(bool),
    UnaryOp {
        op: UnaryOpKind,
        operand: Box<AstNode>,
    },
    BinaryOp {
        op: BinaryOpKind,
        left: Box<AstNode>,
        right: Box<AstNode>,
    },
    Function {
        params: Vec<Parameter>,
        body: Box<AstNode>,
    },
    Block {
        statements: Vec<AstNode>,
        deferred: Vec<AstNode>,
    },
    VarDecl {
        name: String,
        type_: Option<Type>,
        value: Box<AstNode>,
        is_mutable: bool,
    },
    Assignment {
        target: Box<AstNode>,
        value: Box<AstNode>,
    },
    Defer {
        stmt: Box<AstNode>,
    },
    Return {
        value: Box<AstNode>,
    },
}

#[derive(Debug, Clone)]
pub enum Number {
    Unresolved(String),
    Resolved(ResolvedNumber),
}

#[derive(Debug, Clone, Copy)]
pub enum ResolvedNumber {
    Bool(bool),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}
#[derive(Debug, Clone, Copy)]
pub enum UnaryOpKind {
    LogicalNot,
    ArithmeticNeg,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOpKind {
    // arithmetic
    Add,
    Sub,
    Mul,
    Div,

    // comparative
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    Different,

    // logical
    And,
    Or,
}

#[derive(Debug, Clone, Copy)]
pub enum ConstDeclKind {
    Const,
    Func,
}

#[derive(Debug, Clone)]
pub enum Type {
    Resolved(ResolvedType),
    Unresolved(String),
}

impl Type {
    pub fn resolve(&self) -> ResolvedType {
        match self {
            Type::Unresolved(name) => match name.as_str() {
                "bool" => ResolvedType::Bool,
                "u8" => ResolvedType::U8,
                "u16" => ResolvedType::U16,
                "u32" => ResolvedType::U32,
                "u64" => ResolvedType::U64,
                "i8" => ResolvedType::I8,
                "i16" => ResolvedType::I16,
                "i32" => ResolvedType::U32,
                "i64" => ResolvedType::I64,
                "f16" => ResolvedType::F16,
                "f32" => ResolvedType::F32,
                "f64" => ResolvedType::F64,
                _ => unreachable!(),
            },
            Type::Resolved(resolved) => *resolved,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ResolvedType {
    Bool,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F16,
    F32,
    F64,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub type_: Type,
}

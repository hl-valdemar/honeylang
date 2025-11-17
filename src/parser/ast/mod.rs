use owo_colors::OwoColorize;

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

impl TreeDisplay for AstNode {
    fn fmt_tree(
        &self,
        f: &mut std::fmt::Formatter,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        let connector = if is_last { "└─" } else { "├─" };
        let child_prefix = format!("{}{}", prefix, if is_last { "   " } else { "││ " });

        match self {
            Self::Program { declarations } => {
                writeln!(f, "{}:", "program".cyan())?;
                for (i, decl) in declarations.iter().enumerate() {
                    let is_last_decl = i == declarations.len() - 1;
                    decl.fmt_tree(f, "", is_last_decl)?;
                }
                Ok(())
            }
            Self::ConstDecl {
                kind,
                name,
                type_,
                value,
            } => {
                writeln!(
                    f,
                    "{}{} {} ({}):",
                    prefix,
                    connector,
                    "declaration".purple(),
                    kind.green()
                )?;

                writeln!(f, "{}{} name: {}", child_prefix, "├─", name.blue())?;

                if let Some(t) = type_ {
                    writeln!(f, "{}{} type: {}", child_prefix, "├─", t.cyan())?;
                } else {
                    writeln!(
                        f,
                        "{}{} type: {}",
                        child_prefix,
                        "├─",
                        "<nil>".bright_black()
                    )?;
                }

                value.fmt_tree(f, &child_prefix, true)?;
                Ok(())
            }
            Self::Ident(name) => {
                writeln!(f, "{}{} identifier: {}", prefix, connector, name.blue())
            }
            Self::Num(number) => match number {
                Number::Resolved(resolved) => {
                    write!(f, "{}{} literal: {}", prefix, connector, resolved.red())
                }
                Number::Unresolved(unresolved) => {
                    writeln!(f, "{}{} literal: {}", prefix, connector, unresolved.red())
                }
            },
            Self::Bool(value) => writeln!(f, "{}{} literal: {}", prefix, connector, value.red()),
            Self::UnaryOp { op, operand } => {
                writeln!(f, "{}{} unary op: {}", prefix, connector, op.cyan())?;
                operand.fmt_tree(f, &child_prefix, true)
            }
            Self::BinaryOp { op, left, right } => {
                writeln!(f, "{}{} binary op: {}", prefix, connector, op.cyan())?;
                left.fmt_tree(f, &child_prefix, false)?;
                right.fmt_tree(f, &child_prefix, true)
            }
            Self::Function { params, body } => {
                writeln!(f, "{}{} func:", prefix, connector)?;

                write!(f, "{}{} params:", child_prefix, "├─")?;
                if params.is_empty() {
                    writeln!(f, " {}", "none".bright_black())?;
                } else {
                    writeln!(f)?;
                    let param_prefix = format!("{}│  ", child_prefix);
                    for (i, param) in params.iter().enumerate() {
                        let is_last_param = i == params.len() - 1;
                        param.fmt_tree(f, &param_prefix, is_last_param)?;
                    }
                }

                body.fmt_tree(f, &child_prefix, true)?;
                Ok(())
            }
            Self::Block {
                statements,
                deferred,
            } => {
                writeln!(f, "{}{} block:", prefix, connector)?;
                writeln!(
                    f,
                    "{}{} statements: {}",
                    child_prefix,
                    "├─",
                    statements.len()
                )?;
                writeln!(f, "{}{} deferred: {}", child_prefix, "└─", deferred.len())?;
                Ok(())
            }
            Self::VarDecl {
                name,
                type_,
                value,
                is_mutable,
            } => {
                writeln!(f, "{}{} var decl:", prefix, connector)?;
                writeln!(f, "{}{} mutable: {}", child_prefix, "├─", is_mutable)?;
                writeln!(f, "{}{} name: {}", child_prefix, "├─", name)?;

                if let Some(t) = type_ {
                    writeln!(f, "{}{} type: {}", child_prefix, "├─", t.cyan())?;
                } else {
                    writeln!(
                        f,
                        "{}{} type: {}",
                        child_prefix,
                        "├─",
                        "<nil>".bright_black()
                    )?;
                }

                value.fmt_tree(f, &child_prefix, true)?;
                Ok(())
            }
            Self::Assignment { target, value } => {
                writeln!(f, "{}{} assignment:", prefix, connector)?;
                writeln!(f, "{}{} target:", child_prefix, "├─")?;
                target.fmt_tree(f, &format!("{}│  ", child_prefix), true)?;
                writeln!(f, "{}{} value:", child_prefix, "└─")?;
                value.fmt_tree(f, &format!("{}   ", child_prefix), true)?;
                Ok(())
            }
            Self::Return { .. } | Self::Defer { .. } => Ok(()),
        }
    }
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

impl TreeDisplay for Parameter {
    fn fmt_tree(
        &self,
        f: &mut std::fmt::Formatter,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        let connector = if is_last { "└─" } else { "├─" };
        writeln!(
            f,
            "{}{} {}: {}",
            prefix,
            connector,
            self.name.blue(),
            self.type_.cyan()
        )
    }
}

pub trait TreeDisplay {
    fn fmt_tree(
        &self,
        f: &mut std::fmt::Formatter,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result;
}

use owo_colors::OwoColorize;

pub fn make_identifier(name: &str) -> AstNode {
    AstNode::Identifier(name.to_string())
}

pub fn make_number(value: &str) -> AstNode {
    AstNode::Number(value.to_string())
}

pub fn make_unary(op: UnaryOpKind, operand: AstNode) -> AstNode {
    AstNode::UnaryOp {
        op,
        operand: Box::new(operand),
    }
}

pub fn make_declaration(
    kind: DeclKind,
    name: &str,
    type_: Option<Type>,
    value: AstNode,
) -> AstNode {
    AstNode::Declaration {
        kind,
        name: name.to_string(),
        type_,
        value: Box::new(value),
    }
}

#[derive(Clone)]
pub enum AstNode {
    Program {
        declarations: Vec<AstNode>,
    },
    Declaration {
        kind: DeclKind,
        name: String,
        type_: Option<Type>,
        value: Box<AstNode>,
    },
    Identifier(String),
    Number(String),
    UnaryOp {
        op: UnaryOpKind,
        operand: Box<AstNode>,
    },
}

impl std::fmt::Display for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.fmt_tree(f, "", true)
    }
}

impl AstNode {
    fn fmt_tree(
        &self,
        f: &mut std::fmt::Formatter,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        // Build the prefix for children - either continue the line or use spaces
        let child_prefix = format!("{}{}", prefix, if is_last { "   " } else { "│  " });

        match self {
            Self::Program { declarations } => {
                writeln!(f, "{}:", "program".cyan())?;
                for (i, decl) in declarations.iter().enumerate() {
                    let is_last_decl = i == declarations.len() - 1;
                    decl.fmt_tree(f, " ", is_last_decl)?;
                }
                Ok(())
            }
            Self::Declaration {
                kind,
                name,
                type_,
                value,
            } => {
                writeln!(
                    f,
                    "{}└─ {} ({}):",
                    prefix,
                    "declaration".purple(),
                    kind.green()
                )?;
                writeln!(f, "{}└─ name: {}", child_prefix, name)?;

                if let Some(t) = type_ {
                    writeln!(f, "{}└─ type: {}", child_prefix, t.cyan())?;
                } else {
                    writeln!(f, "{}└─ type: {}", child_prefix, "<nil>".purple())?;
                }

                value.fmt_tree(f, &child_prefix, true)?;
                Ok(())
            }
            Self::Identifier(name) => {
                writeln!(f, "{}└─ identifier: {}", prefix, name.blue())
            }
            Self::Number(value) => {
                writeln!(f, "{}└─ literal: {}", prefix, value.blue())
            }
            Self::UnaryOp { op, operand } => {
                writeln!(f, "{}└─ unary op: {}", prefix, op.blue())?;
                operand.fmt_tree(f, &child_prefix, true)
            }
        }
    }
}

#[derive(Clone)]
pub enum UnaryOpKind {
    LogicalNot,
    ArithmeticNeg,
}

impl std::fmt::Display for UnaryOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::LogicalNot => write!(f, "!"),
            Self::ArithmeticNeg => write!(f, "-"),
        }
    }
}

#[derive(Clone)]
pub enum DeclKind {
    Const, // comptime const
    Func,  // (comptime) func
           // Immutable, // runtime immutable
           // Mutable,   // runtime mutable
}

impl std::fmt::Display for DeclKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Const => write!(f, "const"),
            Self::Func => write!(f, "func"),
        }
    }
}

#[derive(Clone)]
pub enum Type {
    NamedType(String),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::NamedType(type_) => write!(f, "{}", type_),
        }
    }
}

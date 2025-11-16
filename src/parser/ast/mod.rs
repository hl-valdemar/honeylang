use std::fmt::write;

use owo_colors::{OwoColorize, colors::Default};

mod display;

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

#[derive(Clone)]
pub enum AstNode {
    Program {
        declarations: Vec<AstNode>,
    },
    ConstDecl {
        kind: ConstDeclKind,
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
    Function {
        params: Vec<Parameter>,
        body: Box<AstNode>,
    },
    Block {
        statements: Vec<AstNode>,
        deferred: Vec<AstNode>,
    },
    Return {
        value: Box<AstNode>,
    },
    Defer {
        stmt: Box<AstNode>,
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
}

impl TreeDisplay for AstNode {
    fn fmt_tree(
        &self,
        f: &mut std::fmt::Formatter,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        // continue line or use spaces
        let child_prefix = format!("{}{}", prefix, if is_last { "   " } else { "│  " });
        let grand_child_prefix = format!("{}{}", child_prefix, if is_last { "   " } else { "│  " });

        match self {
            Self::Program { declarations } => {
                writeln!(f, "{}:", "program".cyan())?;
                for (i, decl) in declarations.iter().enumerate() {
                    let is_last_decl = i == declarations.len() - 1;
                    decl.fmt_tree(f, " ", is_last_decl)?;
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
            Self::Function { params, body } => {
                writeln!(f, "{}└─ func:", prefix)?;

                writeln!(f, "{}└─ params:", child_prefix)?;
                for param in params {
                    param.fmt_tree(f, &grand_child_prefix, false);
                }

                body.fmt_tree(f, &child_prefix, true)?;
                Ok(())
            }
            Self::Block {
                statements,
                deferred,
            } => {
                writeln!(f, "{}└─ block:", prefix)?;
                writeln!(f, "{}└─ statements: {}", child_prefix, statements.len())?;
                writeln!(f, "{}└─ deferred: {}", child_prefix, deferred.len())?;
                Ok(())
            }
            Self::VarDecl {
                name,
                type_,
                value,
                is_mutable,
            } => {
                writeln!(f, "{}└─ var decl:", prefix)?;
                writeln!(f, "{}└─ mutable: {}", child_prefix, is_mutable)?;
                writeln!(f, "{}└─ name: {}", child_prefix, name)?;

                if let Some(t) = type_ {
                    writeln!(f, "{}└─ type: {}", child_prefix, t.cyan())?;
                } else {
                    writeln!(f, "{}└─ type: {}", child_prefix, "<nil>".purple())?;
                }

                value.fmt_tree(f, &child_prefix, true);
                Ok(())
            }
            Self::Assignment { target, value } => {
                writeln!(f, "{}└─ assignment:", prefix)?;

                writeln!(f, "{}└─ target:", child_prefix)?;
                target.fmt_tree(f, &grand_child_prefix, false);

                writeln!(f, "{}└─ value:", child_prefix)?;
                value.fmt_tree(f, &grand_child_prefix, true);
                Ok(())
            }
            Self::Return { .. } | Self::Defer { .. } => Ok(()),
        }
    }
}

#[derive(Clone)]
pub enum UnaryOpKind {
    LogicalNot,
    ArithmeticNeg,
}

#[derive(Clone)]
pub enum ConstDeclKind {
    Const,
    Func,
}

#[derive(Clone)]
pub enum Type {
    NamedType(String),
}

#[derive(Clone)]
pub struct Parameter {
    pub name: String,
    pub type_: Type,
}

impl TreeDisplay for Parameter {
    fn fmt_tree(
        &self,
        f: &mut std::fmt::Formatter,
        prefix: &str,
        _is_last: bool,
    ) -> std::fmt::Result {
        writeln!(f, "{}└─ {}: {}", prefix, self.name, self.type_)
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

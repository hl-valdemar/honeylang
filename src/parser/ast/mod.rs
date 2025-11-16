use owo_colors::OwoColorize;

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
        let child_prefix = format!("{}{}", prefix, if is_last { "   " } else { "│  " });

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
                
                writeln!(f, "{}{} name: {}", child_prefix, "├─", name.purple())?;
                
                if let Some(t) = type_ {
                    writeln!(f, "{}{} type: {}", child_prefix, "├─", t.cyan())?;
                } else {
                    writeln!(f, "{}{} type: {}", child_prefix, "├─", "<nil>".red())?;
                }
                
                value.fmt_tree(f, &child_prefix, true)?;
                Ok(())
            }
            Self::Identifier(name) => {
                writeln!(f, "{}{} identifier: {}", prefix, connector, name.purple())
            }
            Self::Number(value) => {
                writeln!(f, "{}{} literal: {}", prefix, connector, value.blue())
            }
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
                    writeln!(f, " {}", "none".purple())?;
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
                writeln!(f, "{}{} statements: {}", child_prefix, "├─", statements.len())?;
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
                    writeln!(f, "{}{} type: {}", child_prefix, "├─", "<nil>".red())?;
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

#[derive(Clone)]
pub enum UnaryOpKind {
    LogicalNot,
    ArithmeticNeg,
}

#[derive(Clone)]
pub enum BinaryOpKind {
    // arithmetic
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone)]
pub enum ConstDeclKind {
    Const,
    Func,
}

#[derive(Clone)]
pub enum Type {
    Named(String),
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
        is_last: bool,
    ) -> std::fmt::Result {
        let connector = if is_last { "└─" } else { "├─" };
        writeln!(f, "{}{} {}: {}", prefix, connector, self.name, self.type_)
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

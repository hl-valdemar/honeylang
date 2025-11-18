use owo_colors::OwoColorize;

use crate::parser::{
    AstNode,
    ast::{
        BinaryOpKind, ConstDeclKind, Number, Parameter, ResolvedNumber, ResolvedType, Type,
        UnaryOpKind,
    },
};

struct TreeFormatter {
    prefix: String,
}

impl TreeFormatter {
    fn new(prefix: &str) -> Self {
        Self {
            prefix: prefix.to_string(),
        }
    }

    fn connector(&self, is_last: bool) -> &'static str {
        if is_last { " └─" } else { " ├─" }
    }

    fn child(&self, is_last: bool) -> Self {
        let extension = if is_last { "    " } else { " │  " };
        Self {
            prefix: format!("{}{}", self.prefix, extension),
        }
    }

    fn prefix(&self) -> &str {
        &self.prefix
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
            Self::Resolved(resolved) => write!(f, "{}", resolved),
            Self::Unresolved(name) => write!(f, "{}", name),
        }
    }
}

impl std::fmt::Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "void"),
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

impl TreeDisplay for AstNode {
    fn fmt_tree(
        &self,
        f: &mut std::fmt::Formatter,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        let tree = TreeFormatter::new(prefix);

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
                    tree.prefix(),
                    tree.connector(is_last),
                    "declaration".purple(),
                    kind.green()
                )?;

                let child = tree.child(is_last);
                writeln!(
                    f,
                    "{}{} name: {}",
                    child.prefix(),
                    child.connector(false),
                    name.blue()
                )?;

                if let Some(t) = type_ {
                    writeln!(
                        f,
                        "{}{} type: {}",
                        child.prefix(),
                        child.connector(false),
                        t.cyan()
                    )?;
                } else {
                    writeln!(
                        f,
                        "{}{} type: {}",
                        child.prefix(),
                        child.connector(false),
                        "<unresolved>".cyan(),
                    )?;
                }

                value.fmt_tree(f, child.prefix(), true)
            }
            Self::Ident(name) => {
                writeln!(
                    f,
                    "{}{} identifier: {}",
                    tree.prefix(),
                    tree.connector(is_last),
                    name.blue()
                )
            }
            Self::Num(number) => match number {
                Number::Resolved(resolved) => writeln!(
                    f,
                    "{}{} literal: {}",
                    tree.prefix(),
                    tree.connector(is_last),
                    resolved.red()
                ),
                Number::Unresolved(unresolved) => writeln!(
                    f,
                    "{}{} literal: {}",
                    tree.prefix(),
                    tree.connector(is_last),
                    unresolved.red()
                ),
            },
            Self::Bool(value) => writeln!(
                f,
                "{}{} literal: {}",
                tree.prefix(),
                tree.connector(is_last),
                value.red()
            ),
            Self::UnaryOp { op, operand } => {
                writeln!(
                    f,
                    "{}{} unary op: {}",
                    tree.prefix(),
                    tree.connector(is_last),
                    op.cyan()
                )?;
                let child = tree.child(is_last);
                operand.fmt_tree(f, child.prefix(), true)
            }
            Self::BinaryOp { op, left, right } => {
                writeln!(
                    f,
                    "{}{} binary op: {}",
                    tree.prefix(),
                    tree.connector(is_last),
                    op.cyan()
                )?;
                let child = tree.child(is_last);
                left.fmt_tree(f, child.prefix(), false)?;
                right.fmt_tree(f, child.prefix(), true)
            }
            Self::Function { params, body } => {
                writeln!(f, "{}{} func:", tree.prefix(), tree.connector(is_last))?;

                let child = tree.child(is_last);
                write!(
                    f,
                    "{}{} params: {}\n",
                    child.prefix(),
                    child.connector(false),
                    params.len().purple()
                )?;
                let param_child = child.child(false);
                for (i, param) in params.iter().enumerate() {
                    let is_last_param = i == params.len() - 1;
                    param.fmt_tree(f, param_child.prefix(), is_last_param)?;
                }

                body.fmt_tree(f, child.prefix(), true)
            }
            Self::Block {
                statements,
                deferred,
            } => {
                writeln!(f, "{}{} block:", tree.prefix(), tree.connector(is_last))?;

                let child = tree.child(is_last);
                writeln!(
                    f,
                    "{}{} statements: {}",
                    child.prefix(),
                    child.connector(false),
                    statements.len().purple(),
                )?;

                let stmt_child = child.child(false);
                for (i, stmt) in statements.iter().enumerate() {
                    let is_last_stmt = i == statements.len() - 1;
                    stmt.fmt_tree(f, stmt_child.prefix(), is_last_stmt)?;
                }

                writeln!(
                    f,
                    "{}{} deferred: {}",
                    child.prefix(),
                    child.connector(true),
                    deferred.len().purple(),
                )?;

                let def_child = child.child(true);
                for (i, stmt) in deferred.iter().enumerate() {
                    let is_last_def = i == deferred.len() - 1;
                    stmt.fmt_tree(f, def_child.prefix(), is_last_def)?;
                }

                Ok(())
            }
            Self::VarDecl {
                name,
                type_,
                value,
                is_mutable,
            } => {
                writeln!(f, "{}{} var decl:", tree.prefix(), tree.connector(is_last))?;

                let child = tree.child(is_last);
                writeln!(
                    f,
                    "{}{} name: {}",
                    child.prefix(),
                    child.connector(false),
                    name.blue(),
                )?;

                if let Some(t) = type_ {
                    writeln!(
                        f,
                        "{}{} type: {}",
                        child.prefix(),
                        child.connector(false),
                        t.cyan()
                    )?;
                } else {
                    writeln!(
                        f,
                        "{}{} type: {}",
                        child.prefix(),
                        child.connector(false),
                        "<unresolved>".cyan()
                    )?;
                }

                writeln!(
                    f,
                    "{}{} mutable: {}",
                    child.prefix(),
                    child.connector(false),
                    is_mutable
                )?;

                writeln!(f, "{}{} value:", child.prefix, child.connector(true))?;
                let grand_child = child.child(true);
                value.fmt_tree(f, grand_child.prefix(), true)
            }
            Self::Assignment { target, value } => {
                writeln!(
                    f,
                    "{}{} assignment:",
                    tree.prefix(),
                    tree.connector(is_last)
                )?;

                let child = tree.child(is_last);
                writeln!(
                    f,
                    "{}{} target: {}",
                    child.prefix(),
                    child.connector(false),
                    target.blue(),
                )?;
                value.fmt_tree(f, child.prefix(), true)
            }
            Self::Return { expr } => {
                writeln!(f, "{}{} return:", tree.prefix(), tree.connector(is_last))?;
                let child = tree.child(is_last);
                expr.fmt_tree(f, child.prefix(), true)
            }
            Self::Defer { stmt } => stmt.fmt_tree(f, prefix, true),
        }
    }
}

impl TreeDisplay for Parameter {
    fn fmt_tree(
        &self,
        f: &mut std::fmt::Formatter,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        let tree = TreeFormatter::new(prefix);
        writeln!(
            f,
            "{}{} {}: {}",
            tree.prefix(),
            tree.connector(is_last),
            self.name.blue(),
            self.type_.cyan()
        )
    }
}

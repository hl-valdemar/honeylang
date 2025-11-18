use owo_colors::OwoColorize;

use crate::parser::{
    ast::{
        BinaryOpKind, ConstDeclKind, Number, Parameter, ResolvedNumber, ResolvedType, Type, UnaryOpKind
    }, AstNode
};

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

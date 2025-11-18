use owo_colors::OwoColorize;

use crate::lexer::{
    Token,
    token::{TokenKind, TokenList},
};

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            // literals
            Self::Identifier(name) => {
                if let Some(s) = name {
                    write!(f, "{}({})", "identifier".purple(), s)
                } else {
                    write!(f, "{}", "identifier".purple())
                }
            }
            Self::Number(value) => {
                if let Some(s) = value {
                    write!(f, "{}({})", "number".purple(), s)
                } else {
                    write!(f, "{}", "number".purple())
                }
            }
            Self::Boolean(value) => write!(f, "{}", value.purple()),

            // keywords
            Self::Func => write!(f, "{}", "func".purple()),
            Self::Return => write!(f, "{}", "return".purple()),
            Self::Defer => write!(f, "{}", "defer".purple()),
            Self::Mut=>write!(f, "{}", "mut".purple()),

            // assignment
            Self::Equal => write!(f, "{}", "=".purple()),
            Self::Colon => write!(f, "{}", ":".purple()),
            Self::DoubleColon => write!(f, "{}", "::".purple()),

            // arithmetic
            Self::Plus => write!(f, "{}", "+".purple()),
            Self::Minus => write!(f, "{}", "-".purple()),
            Self::Star => write!(f, "{}", "*".purple()),
            Self::Slash => write!(f, "{}", "/".purple()),

            // logical
            Self::Not => write!(f, "{}", "not".purple()),
            Self::And => write!(f, "{}", "and".purple()),
            Self::Or => write!(f, "{}", "or".purple()),

            // comparative
            Self::Less => write!(f, "{}", "<".purple()),
            Self::LessEqual => write!(f, "{}", "<=".purple()),
            Self::Greater => write!(f, "{}", ">".purple()),
            Self::GreaterEqual => write!(f, "{}", ">=".purple()),
            Self::DoubleEqual => write!(f, "{}", "==".purple()),
            Self::NotEqual => write!(f, "{}", "!=".purple()),

            // other
            Self::Comma => write!(f, "{}", ",".purple()),
            Self::LeftParen => write!(f, "{}", "(".purple()),
            Self::RightParen => write!(f, "{}", ")".purple()),
            Self::LeftCurly => write!(f, "{}", "{".purple()),
            Self::RightCurly => write!(f, "{}", "}".purple()),
            Self::Eof => write!(f, "{}", "EOF".purple()),
        }
    }
}

impl std::fmt::Display for TokenList {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for token in &self.tokens {
            write!(f, "{}: {}\n", token.loc, token)?;
        }
        Ok(())
    }
}

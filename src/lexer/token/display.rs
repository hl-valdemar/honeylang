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
            TokenKind::Identifier(name) => {
                if let Some(s) = name {
                    write!(f, "identifier({:?})", s)
                } else {
                    write!(f, "identifier")
                }
            }
            TokenKind::Number(value) => {
                if let Some(s) = value {
                    write!(f, "number({:?})", s)
                } else {
                    write!(f, "number")
                }
            }

            // keywords
            Self::Func => write!(f, "func"),
            Self::Return => write!(f, "return"),

            // assignment
            Self::Equal => write!(f, "="),
            Self::Colon => write!(f, ":"),
            Self::DoubleColon => write!(f, "::"),

            // arithmetic
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),

            // logical
            Self::Not => write!(f, "not"),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),

            // comparative
            Self::DoubleEqual => write!(f, "=="),
            Self::Less => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),

            // other
            Self::Comma => write!(f, ","),
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::LeftCurly => write!(f, "{{"),
            Self::RightCurly => write!(f, "}}"),
            Self::Eof => write!(f, "EOF"),

            _ => todo!("finish token kind display implementation"),
        }
    }
}

impl std::fmt::Display for TokenList {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for token in &self.tokens {
            write!(f, "{}: {}\n", token.loc, token.cyan())?;
        }
        Ok(())
    }
}

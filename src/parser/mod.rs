use crate::lexer::token::{Token, TokenKind, TokenList};
use crate::parser::ast::{AstNode, DeclKind, Type, UnaryOpKind, make_declaration, make_unary};
use crate::parser::error::{ErrorList, ParsingError};

pub mod ast;
mod error;

pub fn parse(tokens: &TokenList) -> (AstNode, ErrorList) {
    let mut parser = Parser::new(tokens.to_vec());
    parser.parse()
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse(&mut self) -> (AstNode, ErrorList) {
        let mut declarations: Vec<AstNode> = Vec::new();
        let mut errors = ErrorList::new();

        while self.peek().is_some_and(|tok| tok.kind != TokenKind::Eof) {
            match self.parse_declaration() {
                Ok(decl) => declarations.push(decl),
                Err(error) => {
                    errors.push(error);
                    self.advance();
                }
            };
        }

        let program = AstNode::Program { declarations };

        (program, errors)
    }

    fn parse_declaration(&mut self) -> Result<AstNode, ParsingError> {
        let Some(token) = self.peek() else {
            return Err(ParsingError::UnexpectedEof);
        };

        if let TokenKind::Identifier(_) = token.kind {
            if self.peek_offset(1).is_some_and(|tok| {
                tok.kind == TokenKind::Colon || tok.kind == TokenKind::DoubleColon
            }) {
                return self.parse_const_decl();
            } else if self
                .peek_offset(1)
                .is_some_and(|tok| tok.kind == TokenKind::DoubleColon)
                && self
                    .peek_offset(2)
                    .is_some_and(|tok| tok.kind == TokenKind::Func)
            {
                // TODO: parse function declaration
                unimplemented!();
            }
        }

        Err(ParsingError::UnexpectedToken {
            found: token.kind,
            expected: TokenKind::DoubleEqual,
        })
    }

    fn parse_const_decl(&mut self) -> Result<AstNode, ParsingError> {
        let Some(token) = self.peek() else {
            return Err(ParsingError::UnexpectedEof);
        };

        // expect identifier
        let name = match token.kind {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(ParsingError::UnexpectedToken {
                    found: token.kind,
                    expected: TokenKind::Identifier(String::from("ident")),
                });
            }
        };
        self.advance();

        let Some(token) = self.peek() else {
            return Err(ParsingError::UnexpectedEof);
        };

        // check for optional type annotation
        let type_ = match token.kind {
            TokenKind::Colon => {
                self.advance();
                let Some(token) = self.peek() else {
                    return Err(ParsingError::UnexpectedEof);
                };

                let type_name = match token.kind {
                    TokenKind::Identifier(name) => name,
                    _ => {
                        return Err(ParsingError::UnexpectedToken {
                            found: token.kind,
                            expected: TokenKind::Identifier(String::from("ident")),
                        });
                    }
                };
                self.advance();
                Some(Type::NamedType(type_name))
            }
            TokenKind::DoubleColon => None,
            _ => {
                return Err(ParsingError::UnexpectedToken {
                    found: token.kind,
                    expected: TokenKind::DoubleColon,
                });
            }
        };

        let Some(token) = self.peek() else {
            return Err(ParsingError::UnexpectedEof);
        };

        // expect assignment operator '::'
        if token.kind != TokenKind::DoubleColon {
            return Err(ParsingError::UnexpectedToken {
                found: token.kind,
                expected: TokenKind::DoubleColon,
            });
        }
        self.advance();

        let value = self.parse_expression()?;

        Ok(make_declaration(DeclKind::Const, &name, type_, value))
    }

    fn parse_func_decl(&mut self) -> Result<AstNode, ParsingError> {
        let Some(token) = self.peek() else {
            return Err(ParsingError::UnexpectedEof);
        };

        // expect identifier
        let name = match token.kind {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(ParsingError::UnexpectedToken {
                    found: token.kind,
                    expected: TokenKind::Identifier(String::from("ident")),
                });
            }
        };
        self.advance();

        let Some(token) = self.peek() else {
            return Err(ParsingError::UnexpectedEof);
        };

        // expect assignment operator '::'
        if token.kind != TokenKind::DoubleColon {
            return Err(ParsingError::UnexpectedToken {
                found: token.kind,
                expected: TokenKind::DoubleColon,
            });
        }
        self.advance();

        let Some(token) = self.peek() else {
            return Err(ParsingError::UnexpectedEof);
        };

        // expect 'func' keyword
        if token.kind != TokenKind::Func {
            return Err(ParsingError::UnexpectedToken {
                found: token.kind,
                expected: TokenKind::Func,
            });
        }
        self.advance();

        // TODO:
        // 1. parse parameters
        // 2. parse return type
        // 3. parse function body

        // parse parameters
        {
            let Some(token) = self.peek() else {
                return Err(ParsingError::UnexpectedEof);
            };

            if token.kind != TokenKind::LeftParen {
                return Err(ParsingError::UnexpectedToken {
                    found: token.kind,
                    expected: TokenKind::LeftParen,
                });
            }
            self.advance();

            // TODO: parse all parameters (name:type pairs, comma separated)

            if token.kind != TokenKind::RightParen {
                return Err(ParsingError::UnexpectedToken {
                    found: token.kind,
                    expected: TokenKind::RightParen,
                });
            }
            self.advance();
        }

        let Some(token) = self.peek() else {
            return Err(ParsingError::UnexpectedEof);
        };

        // expect return type
        let return_type = match token.kind {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(ParsingError::UnexpectedToken {
                    found: token.kind,
                    expected: TokenKind::Identifier(String::from("ident")),
                });
            }
        };
        self.advance();

        // expect function block
        let _ = self.parse_block();

        // NOTE: FIGURE OUT HOW TO PROPERLY REPORT EXPECTED TOKENS WHEN MULTIPLE APPLY

        unimplemented!();
    }

    fn parse_block(&mut self) -> Result<AstNode, ParsingError> {
        let Some(token) = self.peek() else {
            return Err(ParsingError::UnexpectedEof);
        };

        // expect '{'
        if token.kind != TokenKind::LeftCurly {
            return Err(ParsingError::UnexpectedToken {
                found: token.kind,
                expected: TokenKind::LeftCurly,
            });
        }
        self.advance();

        // TODO: parse statements
        while !self.check(TokenKind::RightCurly) && !self.check(TokenKind::Eof) {
            unimplemented!()
        }

        // expect '}'
        if token.kind != TokenKind::RightCurly {
            return Err(ParsingError::UnexpectedToken {
                found: token.kind,
                expected: TokenKind::RightCurly,
            });
        }
        self.advance();

        unimplemented!()
    }

    fn parse_statement(&mut self) -> Result<AstNode, ParsingError> {
        if self.check(TokenKind::Defer) {
            // TODO: parse defer statement
            unimplemented!()
        } else if self.check(TokenKind::Return) {
            // TODO: parse return statement
            unimplemented!()
        }

        unimplemented!()
    }

    // simply a wrapper around the most generic expression type
    fn parse_expression(&mut self) -> Result<AstNode, ParsingError> {
        self.parse_unary()
    }

    fn parse_unary(&mut self) -> Result<AstNode, ParsingError> {
        let Some(token) = self.peek() else {
            return Err(ParsingError::UnexpectedEof);
        };

        // check for unary operation
        if token.kind == TokenKind::Minus || token.kind == TokenKind::Not {
            self.advance();

            // parse the operation
            let op = match token.kind {
                TokenKind::Minus => UnaryOpKind::ArithmeticNeg,
                TokenKind::Not => UnaryOpKind::LogicalNot,
                _ => unreachable!(),
            };

            // parse the operand (can be another unary)
            let operand = self.parse_unary()?;

            return Ok(make_unary(op, operand));
        }

        // otherwise, we're dealing with a primary
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<AstNode, ParsingError> {
        let Some(token) = self.peek() else {
            return Err(ParsingError::UnexpectedEof);
        };

        let node = match token.kind {
            TokenKind::Identifier(name) => ast::make_identifier(&name),
            TokenKind::Number(value) => ast::make_number(&value),
            _ => {
                return Err(ParsingError::UnexpectedToken {
                    found: token.kind,
                    expected: TokenKind::Identifier(String::from("ident")),
                });
            }
        };

        self.advance();
        Ok(node)
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.pos).cloned()
    }

    fn peek_offset(&self, offset: usize) -> Option<Token> {
        self.tokens.get(self.pos + offset).cloned()
    }

    fn check(&self, token_kind: TokenKind) -> bool {
        self.peek().is_some_and(|tok| tok.kind == token_kind)
    }

    fn check_offset(&self, token_kind: TokenKind, offset: usize) -> bool {
        self.peek_offset(offset)
            .is_some_and(|tok| tok.kind == token_kind)
    }

    fn expect(&mut self, token_kind: TokenKind) -> bool {
        if self.check(token_kind) {
            self.advance();
            return true;
        }
        false
    }
}

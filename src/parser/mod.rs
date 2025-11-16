use crate::lexer::token::{Token, TokenKind, TokenList};
use crate::parser::ast::{AstNode, ConstDeclKind, Type, UnaryOpKind, make_const_decl, make_unary};
use crate::parser::error::{ErrorList, ParsingError};

pub mod ast;
mod error;

pub fn parse(tokens: &TokenList) -> (AstNode, ErrorList) {
    let mut parser = Parser::new(tokens.as_slice());
    parser.parse()
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: &[Token]) -> Self {
        Self {
            tokens: tokens.to_vec(),
            pos: 0,
        }
    }

    pub fn parse(&mut self) -> (AstNode, ErrorList) {
        let mut declarations: Vec<AstNode> = Vec::new();
        let mut errors = ErrorList::new();

        while self.peek().is_some_and(|tok| tok.kind != TokenKind::Eof) {
            match self.parse_const_decl() {
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

    fn parse_const_decl(&mut self) -> Result<AstNode, ParsingError> {
        // expect identifier
        let name = self.expect_identifier()?;

        // check for optional type annotation
        let token = self.current()?;
        let type_ = match token.kind {
            TokenKind::Colon => {
                // consume ':'
                self.advance();

                // expect type name
                let type_name = self.expect_identifier()?;

                Some(Type::NamedType(type_name))
            }
            TokenKind::DoubleColon => None,
            _ => {
                return Err(ParsingError::UnexpectedToken {
                    found: token.kind,
                    expected: vec![TokenKind::DoubleColon],
                });
            }
        };

        // expect assignment operator '::'
        self.expect(TokenKind::DoubleColon)?;

        // expect expression
        let value = self.parse_expression()?;

        Ok(make_const_decl(ConstDeclKind::Const, &name, type_, value))
    }

    // simply a wrapper around the most generic expression type
    fn parse_expression(&mut self) -> Result<AstNode, ParsingError> {
        self.parse_unary()
    }

    fn parse_unary(&mut self) -> Result<AstNode, ParsingError> {
        let token = self.current()?;

        // check for unary operation
        if matches!(token.kind, TokenKind::Minus | TokenKind::Not) {
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
        let token = self.current()?;

        let node = match token.kind {
            TokenKind::Identifier(name) => ast::make_identifier(&name),
            TokenKind::Number(value) => ast::make_number(&value),
            _ => {
                return Err(ParsingError::UnexpectedToken {
                    found: token.kind,
                    expected: vec![
                        TokenKind::Identifier(String::from("identifier")),
                        TokenKind::Number(String::from("number")),
                    ],
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

    fn current(&self) -> Result<Token, ParsingError> {
        self.peek().ok_or(ParsingError::UnexpectedEof)
    }

    fn check(&self, token_kind: TokenKind) -> bool {
        self.peek().is_some_and(|tok| tok.kind == token_kind)
    }

    fn expect(&mut self, expected: TokenKind) -> Result<(), ParsingError> {
        if self.check(expected.clone()) {
            self.advance();
            Ok(())
        } else {
            Err(ParsingError::UnexpectedToken {
                found: self.peek().map(|t| t.kind).unwrap_or(TokenKind::Eof),
                expected: vec![expected.clone()],
            })
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParsingError> {
        match self.peek() {
            Some(Token {
                kind: TokenKind::Identifier(name),
                ..
            }) => {
                self.advance();
                Ok(name)
            }
            Some(token) => Err(ParsingError::UnexpectedToken {
                found: token.kind,
                expected: vec![TokenKind::Identifier(String::from("identifier"))],
            }),
            None => Err(ParsingError::UnexpectedEof),
        }
    }
}

//     fn parse_declaration(&mut self) -> Result<AstNode, ParsingError> {
//         let Some(token) = self.peek() else {
//             return Err(ParsingError::UnexpectedEof);
//         };
//
//         if let TokenKind::Identifier(_) = token.kind {
//             if self.peek_offset(1).is_some_and(|tok| {
//                 tok.kind == TokenKind::Colon || tok.kind == TokenKind::DoubleColon
//             }) {
//                 return self.parse_const_decl();
//             } else if self
//                 .peek_offset(1)
//                 .is_some_and(|tok| tok.kind == TokenKind::DoubleColon)
//                 && self
//                     .peek_offset(2)
//                     .is_some_and(|tok| tok.kind == TokenKind::Func)
//             {
//                 // TODO: parse function declaration
//                 unimplemented!();
//             }
//         }
//
//         Err(ParsingError::UnexpectedToken {
//             found: token.kind,
//             expected: TokenKind::DoubleEqual,
//         })
//     }
//
//     fn parse_func_decl(&mut self) -> Result<AstNode, ParsingError> {
//         let Some(token) = self.peek() else {
//             return Err(ParsingError::UnexpectedEof);
//         };
//
//         // expect identifier
//         let name = match token.kind {
//             TokenKind::Identifier(name) => name,
//             _ => {
//                 return Err(ParsingError::UnexpectedToken {
//                     found: token.kind,
//                     expected: TokenKind::Identifier(String::from("ident")),
//                 });
//             }
//         };
//         self.advance();
//
//         let Some(token) = self.peek() else {
//             return Err(ParsingError::UnexpectedEof);
//         };
//
//         // expect assignment operator '::'
//         if token.kind != TokenKind::DoubleColon {
//             return Err(ParsingError::UnexpectedToken {
//                 found: token.kind,
//                 expected: TokenKind::DoubleColon,
//             });
//         }
//         self.advance();
//
//         let Some(token) = self.peek() else {
//             return Err(ParsingError::UnexpectedEof);
//         };
//
//         // expect 'func' keyword
//         if token.kind != TokenKind::Func {
//             return Err(ParsingError::UnexpectedToken {
//                 found: token.kind,
//                 expected: TokenKind::Func,
//             });
//         }
//         self.advance();
//
//         // TODO:
//         // 1. parse parameters
//         // 2. parse return type
//         // 3. parse function body
//
//         // parse parameters
//         {
//             let Some(token) = self.peek() else {
//                 return Err(ParsingError::UnexpectedEof);
//             };
//
//             if token.kind != TokenKind::LeftParen {
//                 return Err(ParsingError::UnexpectedToken {
//                     found: token.kind,
//                     expected: TokenKind::LeftParen,
//                 });
//             }
//             self.advance();
//
//             // TODO: parse all parameters (name:type pairs, comma separated)
//
//             if token.kind != TokenKind::RightParen {
//                 return Err(ParsingError::UnexpectedToken {
//                     found: token.kind,
//                     expected: TokenKind::RightParen,
//                 });
//             }
//             self.advance();
//         }
//
//         let Some(token) = self.peek() else {
//             return Err(ParsingError::UnexpectedEof);
//         };
//
//         // expect return type
//         let return_type = match token.kind {
//             TokenKind::Identifier(name) => name,
//             _ => {
//                 return Err(ParsingError::UnexpectedToken {
//                     found: token.kind,
//                     expected: TokenKind::Identifier(String::from("ident")),
//                 });
//             }
//         };
//         self.advance();
//
//         // expect function block
//         let _ = self.parse_block();
//
//         // NOTE: FIGURE OUT HOW TO PROPERLY REPORT EXPECTED TOKENS WHEN MULTIPLE APPLY
//
//         unimplemented!();
//     }
//
//     fn parse_block(&mut self) -> Result<AstNode, ParsingError> {
//         self.expect(TokenKind::LeftCurly)?;
//
//         let mut statements = Vec::new();
//
//         while !self.check(TokenKind::RightCurly) && !self.check(TokenKind::Eof) {
//             statements.push(self.parse_statement()?);
//         }
//
//         if !self.expect(TokenKind::RightCurly) {
//             return Err(ParsingError::UnexpectedToken {
//                 found: self.peek().map(|t| t.kind).unwrap_or(TokenKind::Eof),
//                 expected: TokenKind::RightCurly,
//             });
//         }
//
//         Ok(AstNode::Block { statements })
//     }
//
//     fn parse_statement(&mut self) -> Result<AstNode, ParsingError> {
//         if self.check(TokenKind::Defer) {
//             // TODO: parse defer statement
//             unimplemented!()
//         } else if self.check(TokenKind::Return) {
//             // TODO: parse return statement
//             unimplemented!()
//         }
//
//         unimplemented!()
//     }

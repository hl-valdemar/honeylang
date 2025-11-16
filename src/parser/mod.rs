use owo_colors::OwoColorize;

use crate::lexer::token::{Token, TokenKind, TokenList};
use crate::parser::ast::{
    AstNode, ConstDeclKind, Parameter, Type, UnaryOpKind, make_const_decl, make_unary,
};
use crate::parser::error::{ErrorList, NoValueKind, ParsingError};

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
        let token = self.current()?;
        let next = self.next()?;

        // check for func decl pattern
        if matches!(token.kind, TokenKind::Identifier(_))
            && matches!(next.kind, TokenKind::DoubleColon)
            && self
                .peek_offset(2)
                .is_some_and(|tok| tok.kind == TokenKind::Func)
        {
            self.parse_func_decl()
        }
        // check for const decl pattern
        else if matches!(token.kind, TokenKind::Identifier(_))
            && matches!(next.kind, TokenKind::Colon | TokenKind::DoubleColon)
        {
            self.parse_const_decl()
        }
        // invalid!
        else {
            Err(ParsingError::ExpectedConstDeclaration { found: token })
        }
    }

    fn parse_func_decl(&mut self) -> Result<AstNode, ParsingError> {
        // expect identifier
        let name = self.expect_identifier()?;

        // expect assignment operator '::'
        self.expect(TokenKind::DoubleColon)?;

        // expect 'func' keyword
        self.expect(TokenKind::Func)?;

        // expect '('
        self.expect(TokenKind::LeftParen)?;

        // parse parameters (name: type, comma separated)
        let params = self.parse_params()?;

        // expect ')'
        self.expect(TokenKind::RightParen)?;

        // expect return type
        let return_type = self.expect_identifier()?;
        let return_type = Some(Type::Named(return_type));

        // parse function body
        let body = self.parse_block()?;

        // construct function data
        let func = AstNode::Function {
            params,
            body: Box::new(body),
        };

        Ok(ast::make_const_decl(
            ConstDeclKind::Func,
            &name,
            return_type,
            func,
        ))
    }

    fn parse_params(&mut self) -> Result<Vec<Parameter>, ParsingError> {
        let mut params = vec![];
        while !self.check(TokenKind::RightParen) {
            // expect param name
            let param_name = self.expect_identifier()?;

            // expect ':'
            self.expect(TokenKind::Colon)?;

            // expect type name
            let type_name = self.expect_identifier()?;

            // FIXME: this code is ugly
            let token = self.current()?;
            let next = self.current()?;
            if matches!(token.kind, TokenKind::Comma)
                && !matches!(next.kind, TokenKind::Identifier(_))
            {
                return Err(ParsingError::UnexpectedToken {
                    found: next,
                    expected: vec![TokenKind::Identifier(None)],
                });
            } else if matches!(token.kind, TokenKind::Comma) {
                self.advance(); // consume ','
            }

            // save param
            params.push(Parameter {
                name: param_name,
                type_: Type::Named(type_name),
            });
        }

        Ok(params)
    }

    fn parse_block(&mut self) -> Result<AstNode, ParsingError> {
        // expect '{'
        self.expect(TokenKind::LeftCurly)?;

        // parse statements
        let mut statements = vec![];
        let mut deferred = vec![];
        while let Ok(stmt) = self.parse_statement() {
            match stmt {
                AstNode::Defer { stmt: _ } => deferred.push(stmt),
                _ => statements.push(stmt),
            }
        }

        // expect '}'
        self.expect(TokenKind::RightCurly)?;

        Ok(AstNode::Block {
            statements,
            deferred,
        })
    }

    fn parse_statement(&mut self) -> Result<AstNode, ParsingError> {
        let token = self.current()?;

        // check if return statement
        if matches!(token.kind, TokenKind::Return) {
            self.parse_return_stmt()
        }
        // invalid!
        else {
            Err(ParsingError::UnexpectedToken {
                found: token,
                expected: vec![TokenKind::Return],
            })
        }
    }

    fn parse_return_stmt(&mut self) -> Result<AstNode, ParsingError> {
        // expect 'return' followed by an expression
        self.expect(TokenKind::Return)?;
        self.parse_expression()
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

                Some(Type::Named(type_name))
            }
            TokenKind::DoubleColon => None,
            _ => {
                return Err(ParsingError::UnexpectedToken {
                    found: token,
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
            TokenKind::Identifier(name) => {
                let name = name.ok_or(ParsingError::NoValue(NoValueKind::Identifier))?;
                ast::make_identifier(&name)
            }
            TokenKind::Number(value) => {
                let value = value.ok_or(ParsingError::NoValue(NoValueKind::Number))?;
                ast::make_number(&value)
            }
            _ => {
                return Err(ParsingError::UnexpectedToken {
                    found: token,
                    expected: vec![TokenKind::Identifier(None), TokenKind::Number(None)],
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

    fn current(&self) -> Result<Token, ParsingError> {
        self.peek().ok_or(ParsingError::UnexpectedEof)
    }

    fn next(&self) -> Result<Token, ParsingError> {
        self.peek_offset(1).ok_or(ParsingError::UnexpectedEof)
    }

    fn check(&self, token_kind: TokenKind) -> bool {
        self.peek().is_some_and(|tok| tok.kind == token_kind)
    }

    fn expect(&mut self, kind: TokenKind) -> Result<(), ParsingError> {
        if self.check(kind.clone()) {
            self.advance();
            Ok(())
        } else {
            Err(ParsingError::UnexpectedToken {
                found: self.peek().ok_or(ParsingError::UnexpectedEof)?,
                expected: vec![kind.clone()],
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
                name.ok_or(ParsingError::NoValue(NoValueKind::Identifier))
            }
            Some(token) => Err(ParsingError::UnexpectedToken {
                found: token,
                expected: vec![TokenKind::Identifier(None)],
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

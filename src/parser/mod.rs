// PARSING ORDER (from tight coupling to loose):
// 1. primary (literals, names, parens)
// 2. unary (-, not)
// 3. multiplicative (*, /)
// 4. additive (+, -)
// 5. comparison (<, >, <=, >=, ==, !=)
// 6. logical and
// 7. logical or
// 8. expression (pretty much just wraps logical or)
// 9. statement

use crate::lexer::token::{Token, TokenKind, TokenList};
use crate::parser::ast::{
    AstNode, BinaryOpKind, ConstDeclKind, Parameter, Type, UnaryOpKind, make_const_decl, make_unary,
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

                    // TODO: implement synchronization points (e.g., skipping to the next declaration boundary)
                    // current: weak error recovery -> cascading errors
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
            return self.parse_func_decl();
        }
        // check for const decl pattern
        else if matches!(token.kind, TokenKind::Identifier(_))
            && matches!(next.kind, TokenKind::Colon | TokenKind::DoubleColon)
        {
            return self.parse_const_decl();
        }

        // otherwise, invalid
        Err(ParsingError::ExpectedConstDeclaration { found: token })
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
        let return_type = Some(Type::Unresolved(return_type));

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
        while !self.check(&TokenKind::RightParen) {
            // expect param name
            let param_name = self.expect_identifier()?;

            // expect ':'
            self.expect(TokenKind::Colon)?;

            // expect type name
            let type_name = self.expect_identifier()?;

            // save param
            params.push(Parameter {
                name: param_name,
                type_: Type::Unresolved(type_name),
            });

            // handle possible comma
            let token = self.current()?;
            if matches!(token.kind, TokenKind::Comma) {
                self.advance();
                // if there's a comma, there must be another parameter
                if self.check(&TokenKind::RightParen) {
                    return Err(ParsingError::TrailingComma { loc: token.loc });
                }
            }
        }

        Ok(params)
    }

    fn parse_block(&mut self) -> Result<AstNode, ParsingError> {
        // expect '{'
        self.expect(TokenKind::LeftCurly)?;

        // parse statements
        let mut statements = vec![];
        let mut deferred = vec![];

        while !self.check(&TokenKind::RightCurly) {
            let stmt = self.parse_statement()?;
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
        let next = self.next()?;

        match token.kind {
            TokenKind::Identifier(_) if matches!(next.kind, TokenKind::Colon) => {
                todo!("parse variable declaration")
            }
            TokenKind::Identifier(_) if matches!(next.kind, TokenKind::Equal) => {
                todo!("parse variable assignment")
            }
            TokenKind::Defer => todo!("parse defer statement"),
            TokenKind::Return => self.parse_return_stmt(),
            _ => Err(ParsingError::UnexpectedToken {
                found: token,
                expected: vec![TokenKind::Return],
            }),
        }
    }

    fn parse_return_stmt(&mut self) -> Result<AstNode, ParsingError> {
        self.expect(TokenKind::Return)?;
        let value = self.parse_expression()?;
        Ok(AstNode::Return {
            value: Box::new(value),
        })
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

                Some(Type::Unresolved(type_name))
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
        self.parse_logical_or()
    }

    fn parse_logical_or(&mut self) -> Result<AstNode, ParsingError> {
        // parse left-hand side
        let mut left = self.parse_logical_and()?;

        // parse operations
        loop {
            let Ok(token) = self.current() else { break };
            if !matches!(token.kind, TokenKind::Or) {
                break;
            }
            self.advance();

            // parse right-hand side
            let right = self.parse_logical_and()?;

            // wrap it up nicely
            left = ast::make_binary(BinaryOpKind::Or, left, right);
        }

        Ok(left)
    }

    fn parse_logical_and(&mut self) -> Result<AstNode, ParsingError> {
        // parse left-hand side
        let mut left = self.parse_comparative()?;

        // parse operations
        loop {
            let Ok(token) = self.current() else { break };
            if !matches!(token.kind, TokenKind::And) {
                break;
            }
            self.advance();

            // parse right-hand side
            let right = self.parse_comparative()?;

            // wrap it up nicely
            left = ast::make_binary(BinaryOpKind::And, left, right);
        }

        Ok(left)
    }

    fn parse_comparative(&mut self) -> Result<AstNode, ParsingError> {
        // parse left-hand side
        let mut left = self.parse_additive()?;

        // parse operations
        loop {
            let Ok(token) = self.current() else { break };
            if !matches!(
                token.kind,
                TokenKind::Less
                    | TokenKind::Greater
                    | TokenKind::LessEqual
                    | TokenKind::GreaterEqual
            ) {
                break;
            }
            self.advance();

            // parse operation
            let op = match token.kind {
                TokenKind::Less => BinaryOpKind::Less,
                TokenKind::Greater => BinaryOpKind::Greater,
                TokenKind::LessEqual => BinaryOpKind::LessEqual,
                TokenKind::GreaterEqual => BinaryOpKind::GreaterEqual,
                TokenKind::Equal => BinaryOpKind::Equal,
                TokenKind::NotEqual => BinaryOpKind::Different,
                _ => unreachable!(),
            };

            // parse right-hand side
            let right = self.parse_additive()?;

            // wrap it up nicely
            left = ast::make_binary(op, left, right);
        }

        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<AstNode, ParsingError> {
        // parse left-hand side
        let mut left = self.parse_multiplicative()?;

        // parse operations
        loop {
            let Ok(token) = self.current() else { break };
            if !matches!(token.kind, TokenKind::Plus | TokenKind::Minus) {
                break;
            }
            self.advance();

            // parse operation
            let op = match token.kind {
                TokenKind::Plus => BinaryOpKind::Add,
                TokenKind::Minus => BinaryOpKind::Sub,
                _ => unreachable!(),
            };

            // parse right-hand side
            let right = self.parse_multiplicative()?;

            // wrap it up nicely
            left = ast::make_binary(op, left, right);
        }

        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<AstNode, ParsingError> {
        // parse left-hand side
        let mut left = self.parse_unary()?;

        // parse operations
        loop {
            let Ok(token) = self.current() else { break };
            if !matches!(token.kind, TokenKind::Star | TokenKind::Slash) {
                break;
            }
            self.advance();

            // parse operation
            let op = match token.kind {
                TokenKind::Star => BinaryOpKind::Mul,
                TokenKind::Slash => BinaryOpKind::Div,
                _ => unreachable!(),
            };

            // parse right-hand side
            let right = self.parse_unary()?;

            // wrap it up nicely
            left = ast::make_binary(op, left, right);
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<AstNode, ParsingError> {
        let token = self.current()?;

        // check for unary operation
        if matches!(token.kind, TokenKind::Minus | TokenKind::Not) {
            self.advance();

            // parse operation
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
                self.advance();
                let name = name.ok_or(ParsingError::NoValue(NoValueKind::Identifier))?;
                ast::make_identifier(&name)
            }
            TokenKind::Number(value) => {
                self.advance();
                let value = value.ok_or(ParsingError::NoValue(NoValueKind::Number))?;
                ast::make_number(&value)
            }
            TokenKind::Boolean(value) => {
                self.advance();
                ast::make_boolean(value)
            }
            TokenKind::LeftParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(TokenKind::RightParen)?;
                expr
            }
            _ => {
                self.advance();
                return Err(ParsingError::UnexpectedToken {
                    found: token,
                    expected: vec![TokenKind::Identifier(None), TokenKind::Number(None)],
                });
            }
        };

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

    fn check(&self, token_kind: &TokenKind) -> bool {
        self.peek().is_some_and(|tok| tok.kind == *token_kind)
    }

    fn expect(&mut self, kind: TokenKind) -> Result<(), ParsingError> {
        if self.check(&kind) {
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

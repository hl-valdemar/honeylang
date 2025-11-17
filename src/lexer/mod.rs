use crate::lexer::error::{ErrorList, LexingError};
use crate::lexer::token::{Token, TokenKind, TokenList};

mod error;
pub mod token;

pub fn scan(filename: &'static str) -> (TokenList, ErrorList) {
    let src =
        std::fs::read_to_string(filename).expect(&format!("Failed to read file: {}", filename));

    let mut lexer = Lexer::new(filename, &src);
    lexer.scan()
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
    filename: &'static str,
    line: usize,
    col: usize,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.filename, self.line, self.col)
    }
}

impl Location {
    pub fn new(filename: &'static str, line: usize, col: usize) -> Self {
        Self {
            filename,
            line,
            col,
        }
    }
}

pub struct Lexer {
    src: Vec<u8>,
    pos: usize,
    loc: Location,
}

impl Lexer {
    pub fn new(filename: &'static str, src: &str) -> Self {
        Self {
            src: src.as_bytes().to_vec(),
            pos: 0,
            loc: Location::new(filename, 1, 1),
        }
    }

    pub fn scan(&mut self) -> (TokenList, ErrorList) {
        let mut tokens = TokenList::new();
        let mut errors = ErrorList::new();

        while self.peek().is_some() {
            let Some(c) = self.peek_char() else {
                tokens.push(Token::new(TokenKind::Eof, self.loc));
                break;
            };

            // literals
            if c.is_alphabetic() || c == '_' {
                match self.scan_identifier() {
                    Ok(ident) => tokens.push(ident),
                    Err(error) => errors.push(error),
                }
                continue;
            } else if c.is_numeric() {
                match self.scan_number() {
                    Ok(num) => tokens.push(num),
                    Err(error) => errors.push(error),
                }
                continue;
            }

            // simple tokens
            match c {
                '+' => {
                    self.advance();
                    tokens.push(Token::new(TokenKind::Plus, self.loc));
                }
                '-' => {
                    self.advance();
                    tokens.push(Token::new(TokenKind::Minus, self.loc));
                }
                '*' => {
                    self.advance();
                    tokens.push(Token::new(TokenKind::Star, self.loc));
                }
                '/' => {
                    self.advance();
                    tokens.push(Token::new(TokenKind::Slash, self.loc));
                }
                '(' => {
                    self.advance();
                    tokens.push(Token::new(TokenKind::LeftParen, self.loc));
                }
                ')' => {
                    self.advance();
                    tokens.push(Token::new(TokenKind::RightParen, self.loc));
                }
                '{' => {
                    self.advance();
                    tokens.push(Token::new(TokenKind::LeftCurly, self.loc));
                }
                '}' => {
                    self.advance();
                    tokens.push(Token::new(TokenKind::RightCurly, self.loc));
                }
                ',' => {
                    self.advance();
                    tokens.push(Token::new(TokenKind::Comma, self.loc));
                }
                ':' => {
                    if self.check_offset(':', 1) {
                        self.advance_n(2);
                        tokens.push(Token::new(TokenKind::DoubleColon, self.loc));
                    } else {
                        self.advance();
                        tokens.push(Token::new(TokenKind::Colon, self.loc));
                    }
                }
                '=' => {
                    if self.check_offset('=', 1) {
                        self.advance_n(2);
                        tokens.push(Token::new(TokenKind::DoubleEqual, self.loc));
                    } else {
                        self.advance();
                        tokens.push(Token::new(TokenKind::Equal, self.loc));
                    }
                }
                '<' => {
                    if self.check_offset('=', 1) {
                        self.advance_n(2);
                        tokens.push(Token::new(TokenKind::LessEqual, self.loc));
                    } else {
                        self.advance();
                        tokens.push(Token::new(TokenKind::Less, self.loc));
                    }
                }
                '>' => {
                    if self.check_offset('=', 1) {
                        self.advance_n(2);
                        tokens.push(Token::new(TokenKind::GreaterEqual, self.loc));
                    } else {
                        self.advance();
                        tokens.push(Token::new(TokenKind::Greater, self.loc));
                    }
                }
                '!' => {
                    if self.check_offset('=', 1) {
                        self.advance_n(2);
                        tokens.push(Token::new(TokenKind::NotEqual, self.loc));
                    } else {
                        errors.push(LexingError::UnexpectedCharacter {
                            name: c.to_string(),
                            loc: self.loc,
                        });
                        self.advance();
                    }
                }
                '#' => {
                    // consume entire line
                    while let Some(c) = self.peek_char() {
                        self.advance();
                        if c == '\n' {
                            self.loc.line += 1;
                            self.loc.col = 1;
                            break;
                        }
                    }
                }
                '\n' => self.advance(), // reachable when scanning invalid syntax
                c if c.is_whitespace() => {
                    self.advance();
                    if c == '\n' {
                        self.loc.line += 1;
                        self.loc.col = 1;
                    }
                }
                _ => {
                    errors.push(LexingError::UnexpectedCharacter {
                        name: c.to_string(),
                        loc: self.loc,
                    });
                    self.advance();
                }
            }
        }

        // push EOF
        tokens.push(Token::new(TokenKind::Eof, self.loc));

        (tokens, errors)
    }

    fn check_keyword(&self, name: &str) -> Token {
        match name {
            "func" => Token::new(TokenKind::Func, self.loc),
            "defer" => Token::new(TokenKind::Defer, self.loc),
            "return" => Token::new(TokenKind::Return, self.loc),
            "not" => Token::new(TokenKind::Not, self.loc),
            "and" => Token::new(TokenKind::And, self.loc),
            "or" => Token::new(TokenKind::Or, self.loc),
            _ => Token::new(TokenKind::Identifier(Some(name.to_string())), self.loc),
        }
    }

    fn scan_identifier(&mut self) -> Result<Token, LexingError> {
        let start = self.pos;

        // caller ensures we're at identifier start
        debug_assert!(
            self.peek_char()
                .map_or(false, |c| c.is_alphabetic() || c == '_'),
            "scan_identifier called at non-identifier position"
        );

        // assume valid start at identifier
        self.advance();

        // scan remaining characters
        while let Some(c) = self.peek_char() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let ident_bytes = self
            .src
            .get(start..self.pos)
            .expect("scan_identifier: indices should be valid");

        let name = std::str::from_utf8(ident_bytes).expect("Invalid UTF-8 in source");
        let token = self.check_keyword(name);

        Ok(token)
    }

    fn scan_number(&mut self) -> Result<Token, LexingError> {
        let start = self.pos;
        let mut has_decimal = false;

        // caller ensures we're at number start
        debug_assert!(
            self.peek_char().map_or(false, |c| c.is_numeric()),
            "scan_number called at non-number position"
        );

        // assume valid start at number
        self.advance();

        // scan remaining characters
        while let Some(c) = self.peek_char() {
            if c.is_numeric() {
                self.advance();
            } else if c == '.' && !has_decimal {
                has_decimal = true;
                self.advance();
            } else if c == '.' && has_decimal {
                return Err(LexingError::UnexpectedCharacter {
                    name: c.to_string(),
                    loc: self.loc,
                });
            } else {
                break;
            }
        }

        let num_bytes = self
            .src
            .get(start..self.pos)
            .expect("scan_number: indices should be valid");

        let num = std::str::from_utf8(num_bytes)
            .expect("Invalid UTF-8 source")
            .to_string();

        let token = Token::new(TokenKind::Number(Some(num)), self.loc);

        Ok(token)
    }

    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }

    fn peek_offset(&self, offset: usize) -> Option<u8> {
        self.src.get(self.pos + offset).copied()
    }

    fn peek_char(&self) -> Option<char> {
        self.peek().map(|b| b as char)
    }

    fn peek_char_offset(&self, offset: usize) -> Option<char> {
        self.peek_offset(offset).map(|b| b as char)
    }

    fn check_offset(&self, expected: char, offset: usize) -> bool {
        if let Some(c) = self.peek_char_offset(offset) {
            return c == expected;
        }
        false
    }

    fn advance(&mut self) {
        self.pos += 1;
        self.loc.col += 1;
    }

    fn advance_n(&mut self, n: usize) {
        self.pos += n;
        self.loc.col += n;
    }
}

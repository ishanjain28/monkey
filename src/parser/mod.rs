pub mod ast;
mod program;

pub use self::program::Program;

use self::ast::{Let, Return, Statement};
use crate::lexer::{Lexer, Token};
use std::iter::Peekable;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    current_token: Option<Token>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Parser {
        Parser {
            lexer: lexer.peekable(),
            current_token: None,
        }
    }
    fn parse_statement(&mut self, token: Token) -> Result<Statement, ParseError> {
        match token {
            Token::Let => match Let::parse(self) {
                Ok(v) => Ok(Statement::Let(v)),
                Err(e) => Err(e), //TODO: Return appropriate error
            },
            Token::Return => match Return::parse(self) {
                Ok(v) => Ok(Statement::Return(v)),
                Err(e) => Err(e),
            },
            n @ _ => {
                println!("{:?}", n);
                unimplemented!();
            }
        }
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        match self.lexer.peek() {
            Some(v) if v == &token => {
                self.current_token = self.lexer.next();
                true
            }
            Some(_) | None => false,
        }
    }

    fn current_token_is(&self, token: Token) -> bool {
        self.current_token == Some(token)
    }
}

#[derive(Debug)]
pub struct ParseError {
    desc: String,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "ParseError: {}", self)
    }
}

impl std::error::Error for ParseError {
    fn description(&self) -> &str {
        &self.desc
    }
}

impl ParseError {
    fn new(desc: &str) -> ParseError {
        ParseError {
            desc: desc.to_owned(),
        }
    }
}

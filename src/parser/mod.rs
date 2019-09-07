pub mod ast;
mod program;

pub use self::program::Program;

use self::ast::{LetStatement, StatementType};
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
    fn parse_statement(&mut self, token: Token) -> Option<ast::StatementType> {
        match token {
            Token::Let => {
                match LetStatement::parse(self) {
                    Ok(v) => Some(StatementType::Let(v)),
                    Err(_) => None, //TODO: Return appropriate error
                }
            }
            _ => None,
        }
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if let Some(v) = self.lexer.peek() {
            if v == &token {
                self.current_token = self.lexer.next();
                true
            } else {
                false
            }
        } else {
            false
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

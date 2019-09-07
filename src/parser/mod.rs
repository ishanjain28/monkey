pub mod ast;
mod program;

pub use self::program::Program;

use self::ast::{LetStatement, Statement};
use crate::lexer::{Lexer, Token};
use std::iter::Peekable;

pub(crate) struct Parser<'a> {
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
    fn parse_statement(&mut self, token: Token) -> Option<Box<dyn ast::Statement>> {
        match token {
            Token::Let => {
                let stmt = match LetStatement::parse(self) {
                    Ok(v) => v,
                    Err(_) => return None, //TODO: Return appropriate error
                };

                Some(Box::new(stmt))
            }
            n @ _ => {
                println!("{:?}", n);
                None
            }
        }
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if let Some(v) = self.lexer.peek() {
            if v == &token {
                self.lexer.next();
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
struct ParseError {
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

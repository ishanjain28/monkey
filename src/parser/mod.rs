pub mod ast;
mod program;

pub use self::program::Program;

use self::ast::{LetStatement, Statement};
use crate::lexer::{Lexer, Token};

struct Parser {}

impl Parser {
    fn parse_statement(token: Token, lexer: &mut Lexer) -> Option<Box<dyn ast::Statement>> {
        match token {
            Token::Let => Some(Box::new(LetStatement::parse(lexer))),
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug)]
struct ParserError {
    desc: String,
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "ParserError: ")
    }
}

impl std::error::Error for ParserError {
    fn description(&self) -> &str {
        &self.desc
    }
}

impl ParserError {
    fn new(desc: String) -> ParserError {
        ParserError { desc }
    }
}

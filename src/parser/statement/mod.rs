use super::ast;
use crate::lexer::Lexer;

#[derive(Debug)]
pub enum Statement {
    Let,
}

impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Let => "let".to_owned(),
        }
    }

    pub fn parse(&self, lexer: &mut Lexer) -> ast::Statement {
        match self {
            Let => Statement::Let(ast::LetStatement::parse(lexer)),
        }
    }
}

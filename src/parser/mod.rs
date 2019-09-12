pub mod ast;
mod program;

pub use self::program::Program;

use self::ast::{Expression, ExpressionStatement, Identifier, Let, Return, Statement};
use crate::lexer::{Lexer, Token, TokenType};
use std::{collections::HashMap, convert::TryFrom, iter::Peekable};

type PrefixParseFn = fn(&mut Parser) -> Result<Expression, ParseError>;
type InfixParseFn = fn(Expression) -> Result<Expression, ParseError>;

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
enum ExpressionPriority {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    current_token: Option<Token>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Parser {
        let prefix_parse_fns = HashMap::new();

        let mut parser = Parser {
            lexer: lexer.peekable(),
            current_token: None,
            infix_parse_fns: HashMap::new(),
            prefix_parse_fns,
        };
        parser.register_prefix_fn(TokenType::Ident, Parser::parse_identifier);
        parser.register_prefix_fn(TokenType::Int, Parser::parse_integer_literal);
        parser
    }

    fn parse_statement(&mut self, token: TokenType) -> Result<Statement, ParseError> {
        match token {
            TokenType::Let => match Let::parse(self) {
                Ok(v) => Ok(Statement::Let(v)),
                Err(e) => Err(e), //TODO: Return appropriate error
            },
            TokenType::Return => match Return::parse(self) {
                Ok(v) => Ok(Statement::Return(v)),
                Err(e) => Err(e),
            },
            _ => match ExpressionStatement::parse(self) {
                Ok(v) => Ok(Statement::ExpressionStatement(v)),
                Err(e) => Err(e),
            },
        }
    }

    fn parse_expression(
        &mut self,
        _priority: ExpressionPriority,
    ) -> Result<Expression, ParseError> {
        let current_token = if let Some(token) = &self.current_token {
            token
        } else {
            return Err(ParseError::new(
                "parser.current_token is None. This *should* not have happened.",
            ));
        };
        let prefix = match self.prefix_parse_fns.get(&current_token.name) {
            Some(v) => v,
            None => {
                return Err(ParseError::new(&format!(
                    "no prefix parse function with token {:?} found in parser",
                    current_token
                )))
            }
        };
        prefix(self)
    }

    fn parse_identifier(parser: &mut Parser) -> Result<Expression, ParseError> {
        let ct = parser.current_token.clone().unwrap();
        Ok(Expression::Ident(Identifier::new(
            ct.clone(), // TODO: Correction needed, Can be a source of subtle error in some cases
            String::try_from(ct.value.unwrap())?.into(),
        )))
    }

    fn parse_integer_literal(parser: &mut Parser) -> Result<Expression, ParseError> {
        let v = parser.current_token.clone().unwrap();

        Ok(Expression::Ident(Identifier::new(
            v.clone(),
            v.value.unwrap(),
        )))
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        match self.lexer.peek() {
            Some(v) if v.name == token.name => {
                self.current_token = self.lexer.next();
                true
            }
            Some(_) | None => false,
        }
    }

    fn current_token_is(&self, token: Token) -> bool {
        self.current_token == Some(token)
    }

    fn peek_token_is(&mut self, token: &Token) -> bool {
        self.lexer.peek() == Some(token)
    }

    fn register_infix_fn(&mut self, token: TokenType, f: InfixParseFn) {
        self.infix_parse_fns.insert(token, f);
    }

    fn register_prefix_fn(&mut self, token: TokenType, f: PrefixParseFn) {
        self.prefix_parse_fns.insert(token, f);
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

impl From<String> for ParseError {
    fn from(desc: String) -> ParseError {
        ParseError { desc }
    }
}

impl From<&str> for ParseError {
    fn from(s: &str) -> ParseError {
        ParseError { desc: s.to_owned() }
    }
}

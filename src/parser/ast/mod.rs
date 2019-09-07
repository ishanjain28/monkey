use super::ParseError;
use crate::lexer::Token;
use crate::parser::Parser;
use std::error::Error;

pub enum Node {
    Statement(StatementType),
    Expression,
}

#[derive(PartialEq, Debug)]
pub enum StatementType {
    Let(LetStatement),
    Ident(Identifier),
}

pub trait Statement {
    fn parse(&mut Parser) -> Result<StatementType, Box<dyn Error>>
    where
        Self: Sized;
    fn new() -> StatementType
    where
        Self: Sized;
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    name: Identifier,
    // value: dyn Expression,
}

impl LetStatement {
    #[allow(dead_code)]
    pub fn new(identifier: Identifier) -> StatementType {
        StatementType::Let(LetStatement { name: identifier })
    }

    pub fn parse(parser: &mut Parser) -> Result<LetStatement, Box<dyn Error>> {
        let name;
        //TODO: Add expression parser
        match parser.lexer.next() {
            Some(v) => match v {
                Token::Ident(q) => name = Identifier { name: q },
                n @ _ => {
                    return Err(Box::new(ParseError::new(&format!(
                        "expected IDENT, Found {:?}",
                        n
                    ))))
                }
            },
            None => {
                return Err(Box::new(ParseError::new(
                    "expected IDENT after let, Could not find it",
                )))
            }
        };

        if !parser.expect_peek(Token::Assign) {
            return Err(Box::new(ParseError::new("expected =, Could not find it")));
        }

        // TODO: Replace this with code to parse expressions correctly
        while !parser.current_token_is(Token::Semicolon) {
            parser.current_token = parser.lexer.next();
        }

        Ok(LetStatement { name })
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    name: String,
}

impl Identifier {
    #[allow(dead_code)]
    pub fn new(name: &str) -> Identifier {
        Identifier {
            name: name.to_owned(),
        }
    }
}

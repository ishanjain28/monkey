use super::ParseError;
use crate::lexer::Token;
use crate::parser::Parser;
use std::error::Error;

pub trait Node {
    fn token_literal(&self) -> &'static str;
    fn validate(&self) -> Result<(), Box<dyn Error>>;
}

pub(crate) trait Statement: Node {
    fn parse(&mut Parser) -> Result<Self, Box<dyn Error>>
    where
        Self: Sized;
}

pub trait Expression: Node {}

#[derive(Debug)]
pub struct LetStatement {
    name: Identifier,
    // value: dyn Expression,
}

impl LetStatement {
    pub fn new(identifier: Identifier) -> LetStatement {
        LetStatement { name: identifier }
    }
}
impl Node for LetStatement {
    fn validate(&self) -> Result<(), Box<dyn Error>> {
        if self.token_literal() != "let" {
            return Err(Box::new(ParseError::new(
                &("self.token_literal is not set, got=".to_owned() + self.token_literal()),
            )));
        }
        Ok(())
    }

    fn token_literal(&self) -> &'static str {
        "let"
    }
}

impl Statement for LetStatement {
    fn parse(parser: &mut Parser) -> Result<LetStatement, Box<dyn Error>> {
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

        while !parser.current_token_is(Token::Semicolon) {
            println!("prev_token={:?}", parser.current_token);
            parser.current_token = parser.lexer.next();
            println!("current_token={:?}", parser.current_token);
        }

        Ok(LetStatement { name })
    }
}

#[derive(Debug)]
pub struct Identifier {
    name: String,
}

impl Identifier {
    pub fn new(name: &str) -> Identifier {
        Identifier {
            name: name.to_owned(),
        }
    }
}
impl Node for Identifier {
    fn validate(&self) -> Result<(), Box<dyn Error>> {
        Ok(())
    }
    fn token_literal(&self) -> &'static str {
        "IDENT"
    }
}

impl Expression for Identifier {}

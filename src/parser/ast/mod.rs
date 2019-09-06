use super::ParserError;
use crate::lexer::Lexer;
use std::error::Error;

pub trait Node {
    fn token_literal(&self) -> &'static str;
    fn validate(&self) -> Result<(), Box<dyn Error>>;
}

pub trait Statement: Node {
    fn parse(&mut Lexer) -> Self
    where
        Self: Sized;
}

pub trait Expression: Node {}

#[derive(Debug)]
pub struct LetStatement {
    name: Identifier,
    // value: dyn Expression,
}

impl Node for LetStatement {
    fn validate(&self) -> Result<(), Box<dyn Error>> {
        if self.token_literal() != "let" {
            return Err(Box::new(ParserError::new(
                "self.token_literal is not set, got=".to_owned() + self.token_literal(),
            )));
        }
        Ok(())
    }

    fn token_literal(&self) -> &'static str {
        "let"
    }
}

impl Statement for LetStatement {
    fn parse(lexer: &mut Lexer) -> LetStatement {
        LetStatement {
            name: Identifier { name: "potato" },
            // value: "",
        }
    }
}

#[derive(Debug)]
pub struct Identifier {
    name: &'static str,
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

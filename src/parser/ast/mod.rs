use crate::{
    lexer::Token,
    parser::{ParseError, Parser},
};

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Let),

    Return(Return),
    //ExpressionStatement(ExpressionStatement),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident(Identifier),
}
#[derive(Debug, PartialEq)]
pub struct Let {
    name: Identifier,
    // value: dyn Expression,
}

impl Let {
    #[allow(dead_code)]
    pub fn new(identifier: Identifier) -> Let {
        Let { name: identifier }
    }

    pub fn parse(parser: &mut Parser) -> Result<Let, ParseError> {
        let name;

        //TODO: Add expression parser
        match parser.lexer.next() {
            Some(v) => match v {
                Token::Ident(q) => name = Identifier { name: q },
                n @ _ => {
                    return Err(ParseError::new(&format!("expected IDENT, Found {:?}", n)));
                }
            },
            None => {
                return Err(ParseError::new(
                    "expected IDENT after let, Could not find it",
                ))
            }
        };

        if !parser.expect_peek(Token::Assign) {
            return Err(ParseError::new("expected =, Could not find it"));
        }

        // TODO: Replace this with code to parse expressions correctly
        while !parser.current_token_is(Token::Semicolon) {
            parser.current_token = parser.lexer.next();
        }

        Ok(Let { name })
    }
}

#[derive(Debug, PartialEq)]
pub struct Expr;

#[derive(Debug, PartialEq)]
pub struct Return {
    return_value: Expr,
}

impl Return {
    pub fn new() -> Return {
        Return { return_value: Expr }
    }

    pub fn parse(parser: &mut Parser) -> Result<Return, ParseError> {
        while !parser.current_token_is(Token::Semicolon) {
            parser.current_token = parser.lexer.next();
        }

        Ok(Return { return_value: Expr })
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

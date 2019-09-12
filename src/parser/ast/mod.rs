use crate::{
    lexer::{Literal, Token, TokenType},
    parser::{ExpressionPriority, ParseError, Parser},
};
use std::convert::TryFrom;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Let),
    Return(Return),
    ExpressionStatement(ExpressionStatement),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident(Identifier),
}

#[derive(Debug, PartialEq)]
pub struct Let {
    name: Identifier,
    value: Option<Expression>,
}

impl Let {
    pub fn new(name: Identifier, value: Option<Expression>) -> Let {
        Let { name, value }
    }

    pub fn parse(parser: &mut Parser) -> Result<Let, ParseError> {
        if !parser.expect_peek(Token::new(TokenType::Ident)) {
            return Err(ParseError::new("expected ident, Couldn't find it"));
        }

        let literal = String::try_from(parser.current_token.clone().unwrap().value.unwrap())?;
        let name = Identifier::new(Token::new(TokenType::Let), &literal);

        if !parser.expect_peek(Token::new(TokenType::Assign)) {
            return Err(ParseError::new("expected =, Could not find it"));
        }

        // TODO: Replace this with code to parse expressions correctly
        while !parser.current_token_is(Token::new(TokenType::Semicolon)) {
            parser.current_token = parser.lexer.next();
        }

        Ok(Let::new(name, None))
    }
}

#[derive(Debug, PartialEq)]
pub struct Return {
    return_value: Expression,
}

impl Return {
    pub fn new() -> Return {
        Return {
            return_value: Expression::Ident(Identifier::new(
                Token::new(TokenType::Return),
                "return",
            )), //TODO FIX THIS
        }
    }

    pub fn parse(parser: &mut Parser) -> Result<Return, ParseError> {
        while !parser.current_token_is(Token::new(TokenType::Semicolon)) {
            parser.current_token = parser.lexer.next();
        }

        Ok(Return::new())
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    name: Token,
    value: Literal,
}

impl Identifier {
    pub fn new(token: Token, name: &str) -> Identifier {
        Identifier {
            name: token,
            value: name.into(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    token: Option<Token>, // The first token in Expression
    expression: Expression,
}

impl ExpressionStatement {
    #[allow(dead_code)]
    pub fn new(token: Option<Token>, expression: Expression) -> Self {
        ExpressionStatement { token, expression }
    }

    pub fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let ct = parser.current_token.clone();

        let expr = parser.parse_expression(ExpressionPriority::Lowest)?;

        let s = Token::new(TokenType::Semicolon);
        if parser.peek_token_is(&s) {
            parser.current_token = parser.lexer.next();
        }

        Ok(ExpressionStatement::new(ct, expr))
    }
}

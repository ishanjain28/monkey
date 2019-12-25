use crate::{
    lexer::{Token, TokenType},
    parser::Parser,
};

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
}

impl<'a> Statement {
    pub fn parse(parser: &'a mut Parser, token: Token) -> Option<Self> {
        match token.name {
            TokenType::Let => Some(Statement::Let(LetStatement::parse(parser)?)),
            TokenType::Return => Some(Statement::Return(ReturnStatement::parse(parser)?)),
            _ => None,
        }
    }
}

// TODO: Expressions are not going to be a struct so using this here just as a placeholder
#[derive(Debug, PartialEq)]
pub struct Expression;

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    // name field is to store the identifier of the binding
    pub name: Identifier,
    // value is to store the expression that'll produce value
    pub value: Option<Expression>,
}

impl LetStatement {
    // TODO: Implement code to parse let statement
    pub fn parse(parser: &mut Parser) -> Option<Self> {
        let mut stmt = LetStatement {
            name: Identifier::new(TokenType::Let, "placeholder_value"),
            value: None,
        };

        if let Some(v) = parser.expect_peek(TokenType::Ident) {
            stmt.name.value = v.literal?;
        } else {
            return None;
        }

        parser.expect_peek(TokenType::Assign)?;

        // TODO: Right now, We are just skipping over all the expressions
        // That'll come later
        // Also, Right now, It hangs forever in case there is no semicolon at the end
        while parser.lexer.next() != Some(Token::new(TokenType::Semicolon)) {}

        Some(stmt)
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    return_value: Option<Expression>,
}

impl ReturnStatement {
    fn parse(parser: &mut Parser) -> Option<Self> {
        let stmt = ReturnStatement {
            return_value: Some(Expression),
        };
        while parser.lexer.next() != Some(Token::new(TokenType::Semicolon)) {}
        return Some(stmt);
    }
}

// Identifier will be an expression
// Identifier in a let statement like, let x = 5; where `x` is an identifier doesn't produce a value
// but an identifier *can* produce value when used on rhs, e.g. let x = y; Here `y` is producing a value
#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub token: TokenType,
    pub value: String,
}

impl Identifier {
    pub fn new(token: TokenType, v: &str) -> Self {
        Identifier {
            token: token,
            value: v.to_string(),
        }
    }
}

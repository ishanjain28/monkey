// TODO: Maybe implement String method to pretty print all AST nodes
use {
    crate::{
        lexer::{Token, TokenType},
        parser::{Parser, ParserError},
    },
    std::convert::From,
};

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl ToString for Program {
    fn to_string(&self) -> String {
        let mut out = String::new();

        for statement in &self.statements {
            out.push_str(&statement.to_string());
            out.push('\n');
        }
        out
    }
}

pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl<'a> Statement {
    pub fn parse(parser: &'a mut Parser, token: Token) -> Option<Self> {
        match token.name {
            TokenType::Let => Some(Statement::Let(LetStatement::parse(parser)?)),
            TokenType::Return => Some(Statement::Return(ReturnStatement::parse(parser)?)),
            _ => Some(Statement::ExpressionStatement(ExpressionStatement::parse(
                parser, token,
            )?)),
        }
    }
}

impl ToString for Statement {
    fn to_string(&self) -> String {
        match self {
            Statement::Let(v) => v.to_string(),
            Statement::Return(v) => v.to_string(),
            Statement::ExpressionStatement(v) => v.to_string(),
        }
    }
}

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

        let ident = parser.expect_peek(TokenType::Ident)?;
        stmt.name.value = ident.literal?;

        parser.expect_peek(TokenType::Assign)?;

        // TODO: Right now, We are just skipping over all the expressions
        // That'll come later
        // Also, Right now, It hangs forever in case there is no semicolon at the end
        while parser.lexer.next() != Some(Token::new(TokenType::Semicolon)) {}

        Some(stmt)
    }

    const fn token_literal() -> &'static str {
        "let"
    }
}

impl ToString for LetStatement {
    fn to_string(&self) -> String {
        let mut out = format!("{} {} = ", Self::token_literal(), self.name.value);

        if let Some(v) = &self.value {
            let a: String = v.into();
            out.push_str(&a);
        }
        out.push(';');
        out
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    return_value: Option<Expression>,
}

impl ReturnStatement {
    fn parse(parser: &mut Parser) -> Option<Self> {
        let stmt = ReturnStatement {
            return_value: Some(Expression::None),
        };
        while parser.lexer.next() != Some(Token::new(TokenType::Semicolon)) {}
        return Some(stmt);
    }

    // TODO: REMOVE THIS!
    const fn token_literal() -> &'static str {
        "return"
    }
}

impl ToString for ReturnStatement {
    fn to_string(&self) -> String {
        let mut out = String::from(Self::token_literal());

        if let Some(v) = &self.return_value {
            out.push(' ');
            let a: String = v.into();
            out.push_str(&a);
        }
        out.push(';');
        out
    }
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl ExpressionStatement {
    fn parse(parser: &mut Parser, current_token: Token) -> Option<Self> {
        // let expr = Expression::parse(parser, token.clone(), ExpressionPriority::Lowest)?;
        let stmt = ExpressionStatement {
            token: current_token.clone(),
            expression: Expression::parse(parser, current_token, ExpressionPriority::Lowest)?,
        };
        if parser.peek_token_is(TokenType::Semicolon) {
            parser.lexer.next();
        }
        Some(stmt)
    }
}

impl ToString for ExpressionStatement {
    fn to_string(&self) -> String {
        self.expression.to_string()
    }
}

#[derive(Debug, PartialEq)]
enum ExpressionPriority {
    Lowest = 0,
    Equals = 1,
    LessGreater = 2,
    Sum = 3,
    Product = 4,
    Prefix = 5,
    Call = 6,
}

// TODO: Expressions are not going to be a struct so using this here just as a placeholder

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    // TODO: Temporary placeholder value. Should be removed once this section is done
    None,
}

impl Expression {
    fn parse(parser: &mut Parser, token: Token, precedence: ExpressionPriority) -> Option<Self> {
        let prefix = parser.prefix_parse_fns.get(&token.name)?;

        prefix(parser, token)
    }

    pub fn parse_identifier(parser: &mut Parser, token: Token) -> Option<Self> {
        Some(Self::Identifier(Identifier::new(
            token.name,
            &token.literal?,
        )))
    }

    pub fn parse_integer_literal(parser: &mut Parser, token: Token) -> Option<Self> {
        let n = match token.literal?.parse::<i64>() {
            Ok(v) => v,
            Err(e) => {
                parser.errors.push(ParserError {
                    reason: e.to_string(),
                });
                return None;
            }
        };

        Some(Self::IntegerLiteral(IntegerLiteral::new(TokenType::Int, n)))
    }

    fn to_string(&self) -> String {
        match self {
            Expression::Identifier(v) => v.to_string(),
            Expression::IntegerLiteral(v) => v.value.to_string(),
            Expression::None => "None".into(),
        }
    }
}

impl From<&Expression> for String {
    fn from(expr: &Expression) -> String {
        expr.to_string()
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

    pub fn to_string(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug, PartialEq)]
pub struct IntegerLiteral {
    token: TokenType,
    value: i64,
}

impl IntegerLiteral {
    pub fn new(token: TokenType, v: i64) -> Self {
        Self {
            token: token,
            value: v,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::{Token, TokenType},
        parser::{
            ast::{Expression, Identifier, LetStatement, ReturnStatement, Statement},
            Program,
        },
    };

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![
                Statement::Let(LetStatement {
                    name: Identifier::new(TokenType::Ident, "myVar"),
                    value: Some(Expression::Identifier(Identifier::new(
                        TokenType::Ident,
                        "anotherVar",
                    ))),
                }),
                Statement::Return(ReturnStatement {
                    return_value: Some(Expression::Identifier(Identifier::new(
                        TokenType::Int,
                        "5",
                    ))),
                }),
                Statement::Return(ReturnStatement { return_value: None }),
            ],
        };
        assert_eq!(
            program.to_string(),
            "let myVar = anotherVar;\nreturn 5;\nreturn;\n"
        );
    }
}

// TODO: Maybe implement String method to pretty print all AST nodes
use {
    crate::{
        lexer::{Token, TokenType},
        parser::{Error as ParserError, Parser},
    },
    std::{
        cmp::PartialOrd,
        convert::From,
        fmt::{Display, Error as FmtError, Formatter},
    },
};

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        let mut out = String::new();

        for statement in &self.statements {
            out.push_str(&statement.to_string());
        }
        write!(f, "{}", out)
    }
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

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        match self {
            Statement::Let(v) => write!(f, "{}", v.to_string()),
            Statement::Return(v) => write!(f, "{}", v.to_string()),
            Statement::ExpressionStatement(v) => write!(f, "{}", v.to_string()),
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

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        let mut out = format!("{} {} = ", Self::token_literal(), self.name.value);

        if let Some(v) = &self.value {
            let a: String = v.into();
            out.push_str(&a);
        }
        out.push(';');
        write!(f, "{}", out)
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

    const fn token_literal() -> &'static str {
        "return"
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        let mut out = String::from(Self::token_literal());

        if let Some(v) = &self.return_value {
            out.push(' ');
            let a: String = v.into();
            out.push_str(&a);
        }
        out.push(';');
        write!(f, "{}", out)
    }
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl ExpressionStatement {
    fn parse(parser: &mut Parser, current_token: Token) -> Option<Self> {
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

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        write!(f, "{}", self.expression.to_string())
    }
}

#[derive(Debug, PartialEq, Copy, PartialOrd, Clone)]
pub enum ExpressionPriority {
    Lowest = 0,
    Equals = 1,
    LessGreater = 2,
    Sum = 3,
    Product = 4,
    Prefix = 5,
    Call = 6,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    // TODO: Temporary placeholder value. Should be removed once this section is done
    None,
}

impl Expression {
    fn parse(parser: &mut Parser, ctoken: Token, precedence: ExpressionPriority) -> Option<Self> {
        match parser.prefix_parse_fns.get(&ctoken.name) {
            Some(prefix) => {
                let mut left_expr = prefix(parser, ctoken);
                while !parser.peek_token_is(TokenType::Semicolon)
                    && precedence < parser.peek_precedence()
                {
                    let peek_token = match parser.lexer.peek() {
                        Some(token) => token.clone(),
                        None => return left_expr,
                    };
                    match parser.infix_parse_fns.get(&peek_token.name) {
                        Some(infix) => {
                            let next_token = parser.lexer.next()?;
                            left_expr = infix(parser, next_token, left_expr.unwrap());
                        }
                        None => return left_expr,
                    };
                }
                left_expr
            }
            None => {
                parser.no_prefix_parse_fn_error(&ctoken.name);
                None
            }
        }
    }

    pub fn parse_identifier(_parser: &mut Parser, token: Token) -> Option<Self> {
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

    pub fn parse_prefix_expression(parser: &mut Parser, ctoken: Token) -> Option<Self> {
        let next_token = parser.lexer.next()?;
        let right_expr = Expression::parse(parser, next_token.clone(), ExpressionPriority::Prefix)?;
        Some(Expression::PrefixExpression(PrefixExpression {
            token: ctoken.clone(),
            operator: ctoken.to_string().into(),
            right: Box::new(right_expr),
        }))
    }

    pub fn parse_infix_expression(
        parser: &mut Parser,
        token: Token,
        left_expr: Self,
    ) -> Option<Self> {
        let cprecedence = parser.current_precedence(&token.name);
        let next_token = parser.lexer.next()?;
        let right_expr = Expression::parse(parser, next_token, cprecedence)?;
        Some(Expression::InfixExpression(InfixExpression::new(
            token.clone(),
            left_expr,
            &token.to_string(),
            right_expr,
        )))
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        write!(
            f,
            "{}",
            match self {
                Expression::Identifier(v) => v.to_string(),
                Expression::IntegerLiteral(v) => v.value.to_string(),
                Expression::PrefixExpression(v) => v.to_string(),
                Expression::InfixExpression(v) => v.to_string(),
                Expression::None => "None".into(),
            }
        )
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
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        write!(f, "{}", self.value.clone())
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

#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    token: Token,
    operator: String,
    right: Box<Expression>,
}

impl PrefixExpression {
    pub fn new(token: Token, operator: &str, right: Expression) -> Self {
        Self {
            token: token,
            operator: operator.to_string(),
            right: Box::new(right),
        }
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        write!(f, "({}{})", self.operator, self.right.to_string())
    }
}

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    token: Token,
    left: Box<Expression>,
    operator: String,
    right: Box<Expression>,
}

impl InfixExpression {
    pub fn new(token: Token, left: Expression, operator: &str, right: Expression) -> Self {
        Self {
            token: token,
            left: Box::new(left),
            operator: operator.to_string(),
            right: Box::new(right),
        }
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        write!(
            f,
            "({} {} {})",
            self.left.to_string(),
            self.operator,
            self.right.to_string()
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::TokenType,
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
            "let myVar = anotherVar;return 5;return;"
        );
    }
}

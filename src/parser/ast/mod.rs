use {
    crate::{
        lexer::{Token, TokenType},
        parser::{Error as ParserError, Parser},
    },
    itertools::Itertools,
    std::{
        cmp::PartialOrd,
        convert::From,
        fmt::{Display, Formatter, Result as FmtResult},
    },
};

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let mut out = String::new();

        for statement in &self.statements {
            out.push_str(&statement.to_string());
        }
        f.write_str(&out)
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
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
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Statement::Let(v) => f.write_str(&v.to_string()),
            Statement::Return(v) => f.write_str(&v.to_string()),
            Statement::ExpressionStatement(v) => f.write_str(&v.to_string()),
            Statement::BlockStatement(v) => f.write_str(&v.to_string()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    // name field is to store the identifier of the binding
    name: Identifier,
    // value is to store the expression that'll produce value
    value: Option<Expression>,
}

impl LetStatement {
    pub fn new(name: Identifier) -> Self {
        Self { name, value: None }
    }
    pub fn with_value(name: Identifier, value: Expression) -> Self {
        Self {
            name,
            value: Some(value),
        }
    }

    pub fn parse(parser: &mut Parser) -> Option<Self> {
        let ident = parser.expect_peek(TokenType::Ident)?;

        parser.expect_peek(TokenType::Assign)?;
        let ntoken = parser.lexer.next()?;
        let expr = Expression::parse(parser, ntoken, ExpressionPriority::Lowest);

        parser.expect_peek(TokenType::Semicolon)?;

        Some(Self {
            name: Identifier::new(TokenType::Let, &ident.literal?),
            value: expr,
        })
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let mut out = format!("{} {} = ", TokenType::Let.to_string(), self.name.value);

        if let Some(v) = &self.value {
            let a: String = v.into();
            out.push_str(&a);
        }
        out.push(';');
        f.write_str(&out)
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    return_value: Option<Expression>,
}

impl ReturnStatement {
    pub fn new(expr: Expression) -> Self {
        ReturnStatement {
            return_value: Some(expr),
        }
    }

    fn parse(parser: &mut Parser) -> Option<Self> {
        let token = parser.lexer.next()?;
        let expr = Expression::parse(parser, token, ExpressionPriority::Lowest);
        parser.expect_peek(TokenType::Semicolon)?;
        Some(ReturnStatement { return_value: expr })
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let mut out: String = TokenType::Return.to_string();

        if let Some(v) = &self.return_value {
            out.push(' ');
            let a: String = v.into();
            out.push_str(&a);
        }
        out.push(';');
        f.write_str(&out)
    }
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    token: Token,
    pub expression: Expression,
}

impl ExpressionStatement {
    pub fn new(token: Token, expression: Expression) -> Self {
        Self { token, expression }
    }
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
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(&self.expression.to_string())
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
    BooleanExpression(BooleanExpression),
    IfExpression(IfExpression),
    FunctionExpression(FunctionLiteral),
    CallExpression(CallExpression),
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
                parser.no_prefix_parse_fn_error(ctoken.name);
                None
            }
        }
    }

    pub fn parse_grouped_expression(parser: &mut Parser, _token: Token) -> Option<Self> {
        let next_token = parser.lexer.next()?;
        let expr = Expression::parse(parser, next_token, ExpressionPriority::Lowest);

        if parser.expect_peek(TokenType::RParen).is_none() {
            None
        } else {
            expr
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let value = match self {
            Expression::Identifier(v) => v.to_string(),
            Expression::IntegerLiteral(v) => v.value.to_string(),
            Expression::PrefixExpression(v) => v.to_string(),
            Expression::InfixExpression(v) => v.to_string(),
            Expression::BooleanExpression(v) => v.to_string(),
            Expression::IfExpression(v) => v.to_string(),
            Expression::FunctionExpression(v) => v.to_string(),
            Expression::CallExpression(v) => v.to_string(),
        };

        f.write_str(&value)
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
    token: TokenType,
    value: String,
}

impl Identifier {
    pub fn new(token: TokenType, v: &str) -> Self {
        Identifier {
            token,
            value: v.to_string(),
        }
    }
    pub fn parse(_parser: &mut Parser, token: Token) -> Option<Expression> {
        Some(Expression::Identifier(Identifier::new(
            token.name,
            &token.literal?,
        )))
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(&self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct IntegerLiteral {
    pub value: i64,
}

impl IntegerLiteral {
    pub fn new(v: i64) -> Self {
        Self { value: v }
    }
    pub fn parse(parser: &mut Parser, token: Token) -> Option<Expression> {
        let n = match token.literal?.parse::<i64>() {
            Ok(v) => v,
            Err(e) => {
                parser.errors.push(ParserError {
                    reason: e.to_string(),
                });
                return None;
            }
        };
        Some(Expression::IntegerLiteral(IntegerLiteral::new(n)))
    }
}

#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    pub operator: TokenType,
    pub right: Box<Expression>,
}

impl PrefixExpression {
    pub fn new(operator: TokenType, right: Expression) -> Self {
        Self {
            operator,
            right: Box::new(right),
        }
    }

    pub fn parse(parser: &mut Parser, ctoken: Token) -> Option<Expression> {
        let next_token = parser.lexer.next()?;
        let right_expr = Expression::parse(parser, next_token, ExpressionPriority::Prefix)?;
        Some(Expression::PrefixExpression(PrefixExpression {
            operator: ctoken.name,
            right: Box::new(right_expr),
        }))
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_fmt(format_args!(
            "({}{})",
            self.operator,
            self.right.to_string()
        ))
    }
}

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub operator: TokenType,
    pub right: Box<Expression>,
}

impl InfixExpression {
    pub fn new(left: Expression, operator: TokenType, right: Expression) -> Self {
        Self {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
    pub fn parse(parser: &mut Parser, token: Token, left_expr: Expression) -> Option<Expression> {
        let cprecedence = parser.current_precedence(token.name);
        let next_token = parser.lexer.next()?;
        let right_expr = Expression::parse(parser, next_token, cprecedence)?;
        Some(Expression::InfixExpression(InfixExpression::new(
            left_expr, token.name, right_expr,
        )))
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_fmt(format_args!(
            "({} {} {})",
            self.left.to_string(),
            self.operator,
            self.right.to_string(),
        ))
    }
}

#[derive(Debug, PartialEq)]
pub struct BooleanExpression {
    token: TokenType,
    pub value: bool,
}

impl BooleanExpression {
    pub fn new(token: TokenType) -> Self {
        BooleanExpression {
            token,
            value: token == TokenType::True,
        }
    }
    pub fn parse(_parser: &mut Parser, token: Token) -> Option<Expression> {
        Some(Expression::BooleanExpression(BooleanExpression::new(
            token.name,
        )))
    }
}

impl Display for BooleanExpression {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(&self.value.to_string())
    }
}

#[derive(Debug, PartialEq)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl IfExpression {
    pub fn new(
        condition: Expression,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> Self {
        Self {
            condition: Box::new(condition),
            consequence,
            alternative,
        }
    }
    pub fn parse(parser: &mut Parser, _ctoken: Token) -> Option<Expression> {
        parser.expect_peek(TokenType::LParen)?;
        let next_token = parser.lexer.next()?;
        let condition = Expression::parse(parser, next_token.clone(), ExpressionPriority::Lowest)?;

        parser.expect_peek(TokenType::RParen)?;
        parser.expect_peek(TokenType::LBrace)?;
        let consequence = BlockStatement::parse(parser, next_token)?;
        let mut expr = IfExpression::new(condition, consequence, None);

        if parser.peek_token_is(TokenType::Else) {
            let token = parser.lexer.next()?;
            parser.expect_peek(TokenType::LBrace)?;
            expr.alternative = BlockStatement::parse(parser, token);
        }
        Some(Expression::IfExpression(expr))
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let mut out = format!(
            "if {} {{ {} }}",
            self.condition.to_string(),
            self.consequence.to_string()
        );
        if let Some(alternative) = &self.alternative {
            out += &format!(" else {{ {} }}", alternative.to_string());
        }
        f.write_str(&out)
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn new(stmt: Vec<Statement>) -> Self {
        Self { statements: stmt }
    }

    pub fn parse(parser: &mut Parser, _ogtoken: Token) -> Option<Self> {
        let mut stmts = vec![];
        let mut ctoken = parser.lexer.next();

        while let Some(token) = ctoken {
            if token.name == TokenType::RBrace {
                break;
            }
            let stmt = Statement::parse(parser, token);

            if let Some(stmt) = stmt {
                stmts.push(stmt);
            }
            ctoken = parser.lexer.next();
        }
        Some(BlockStatement::new(stmts))
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let mut out = String::new();

        for stmt in &self.statements {
            out.push_str(&stmt.to_string());
        }
        f.write_str(&out)
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionLiteral {
    token: Token,
    parameters: Vec<Identifier>,
    body: BlockStatement,
}

impl FunctionLiteral {
    pub fn new(token: Token, parameters: Vec<Identifier>, body: BlockStatement) -> Self {
        Self {
            token,
            parameters,
            body,
        }
    }
    pub fn parse(parser: &mut Parser, ctoken: Token) -> Option<Expression> {
        parser.expect_peek(TokenType::LParen)?;
        let ntoken = parser.lexer.peek()?.clone();
        let parameters = FunctionLiteral::parse_function_parameters(parser, ntoken)?;

        parser.expect_peek(TokenType::LBrace)?;

        let ntoken = parser.lexer.peek()?.clone();

        let body = BlockStatement::parse(parser, ntoken)?;

        Some(Expression::FunctionExpression(Self {
            token: ctoken,
            parameters,
            body,
        }))
    }

    fn parse_function_parameters(parser: &mut Parser, _ctoken: Token) -> Option<Vec<Identifier>> {
        let mut out = vec![];

        if parser.peek_token_is(TokenType::RParen) {
            parser.lexer.next();
            Some(out)
        } else {
            let ntoken = parser.lexer.next()?;
            let ident = Identifier::new(ntoken.name, &ntoken.literal?);
            out.push(ident);

            while parser.peek_token_is(TokenType::Comma) {
                parser.lexer.next();
                let token = parser.lexer.next()?;
                let ident = Identifier::new(token.name, &token.literal?);
                out.push(ident);
            }
            parser.expect_peek(TokenType::RParen)?;
            Some(out)
        }
    }
}
impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_fmt(format_args!(
            "{}({}) {{ {} }}",
            self.token.name,
            self.parameters.iter().join(", "),
            self.body
        ))
    }
}

#[derive(Debug, PartialEq)]
pub struct CallExpression {
    function: Box<Expression>,
    arguments: Vec<Expression>,
}

impl CallExpression {
    pub fn new(f: Expression, arguments: Vec<Expression>) -> Self {
        Self {
            function: Box::new(f),
            arguments,
        }
    }

    pub fn parse(parser: &mut Parser, ctoken: Token, expr: Expression) -> Option<Expression> {
        let args = Self::parse_call_arguments(parser, ctoken);

        Some(Expression::CallExpression(Self {
            function: Box::new(expr),
            arguments: args?,
        }))
    }

    fn parse_call_arguments(parser: &mut Parser, _ctoken: Token) -> Option<Vec<Expression>> {
        let mut expressions = vec![];
        if let Some(token) = parser.lexer.peek() {
            if token.name == TokenType::RParen {
                parser.lexer.next();
                return Some(expressions);
            }
        }
        let ntoken = match parser.lexer.next() {
            Some(token) => token,
            None => return Some(expressions),
        };

        if let Some(expr) = Expression::parse(parser, ntoken, ExpressionPriority::Lowest) {
            expressions.push(expr);
        } else {
            return Some(expressions);
        }
        while parser.peek_token_is(TokenType::Comma) {
            parser.lexer.next();
            let token = match parser.lexer.next() {
                Some(v) => v,
                None => return Some(expressions),
            };
            if let Some(expr) = Expression::parse(parser, token, ExpressionPriority::Lowest) {
                expressions.push(expr);
            } else {
                return Some(expressions);
            }
        }
        parser.expect_peek(TokenType::RParen)?;

        Some(expressions)
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_fmt(format_args!(
            "{}({})",
            self.function,
            self.arguments.iter().join(", ")
        ))
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

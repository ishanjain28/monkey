pub mod ast;

use {
    crate::{
        lexer::{Lexer, Token, TokenType},
        parser::ast::*,
    },
    std::{
        collections::HashMap,
        fmt::{Display, Formatter, Result as FmtResult},
        iter::Peekable,
    },
};

type PrefixParseFn = fn(&mut Parser, token: Token) -> Option<Expression>;
type InfixParseFn = fn(&mut Parser, Token, Expression) -> Option<Expression>;

//TODO: Add Parser tracing so we can easily figure out the call stack hierarchy
lazy_static! {
    static ref PRECEDENCE_MAP: HashMap<TokenType, ExpressionPriority> = {
        let mut m = HashMap::new();
        m.insert(TokenType::Equals, ExpressionPriority::Equals);
        m.insert(TokenType::NotEquals, ExpressionPriority::Equals);
        m.insert(TokenType::LessThan, ExpressionPriority::LessGreater);
        m.insert(TokenType::GreaterThan, ExpressionPriority::LessGreater);
        m.insert(TokenType::Plus, ExpressionPriority::Sum);
        m.insert(TokenType::Minus, ExpressionPriority::Sum);
        m.insert(TokenType::Slash, ExpressionPriority::Product);
        m.insert(TokenType::Asterisk, ExpressionPriority::Product);
        m.insert(TokenType::LParen, ExpressionPriority::Call);
        m
    };
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    pub errors: Vec<Error>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer: lexer.peekable(),
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(TokenType::String, StringLiteral::parse);
        parser.register_prefix(TokenType::Ident, Identifier::parse);
        parser.register_prefix(TokenType::Int, IntegerLiteral::parse);
        parser.register_prefix(TokenType::Bang, PrefixExpression::parse);
        parser.register_prefix(TokenType::Minus, PrefixExpression::parse);
        parser.register_prefix(TokenType::True, BooleanExpression::parse);
        parser.register_prefix(TokenType::False, BooleanExpression::parse);
        parser.register_prefix(TokenType::LParen, Expression::parse_grouped_expression);
        parser.register_prefix(TokenType::If, IfExpression::parse);
        parser.register_prefix(TokenType::Function, FunctionLiteral::parse);
        parser.register_prefix(TokenType::LBracket, ArrayLiteral::parse);

        // Neat trick!
        // Call expressions looks like <ident>(<args>).
        // We can easily parse those by registering a infix on '('/LParen
        parser.register_infix(TokenType::LParen, CallExpression::parse);
        parser.register_infix(TokenType::Plus, InfixExpression::parse);
        parser.register_infix(TokenType::Minus, InfixExpression::parse);
        parser.register_infix(TokenType::Slash, InfixExpression::parse);
        parser.register_infix(TokenType::Asterisk, InfixExpression::parse);
        parser.register_infix(TokenType::Equals, InfixExpression::parse);
        parser.register_infix(TokenType::NotEquals, InfixExpression::parse);
        parser.register_infix(TokenType::LessThan, InfixExpression::parse);
        parser.register_infix(TokenType::GreaterThan, InfixExpression::parse);
        parser
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program { statements: vec![] };

        while let Some(token) = self.lexer.next() {
            if token.name == TokenType::EOF {
                break;
            }

            match Statement::parse(self, token.clone()) {
                Some(x) => program.statements.push(x),
                None => eprintln!("error in generating statement: {:?}", token),
            };
        }

        Some(program)
    }

    fn peek_token_is(&mut self, token: TokenType) -> bool {
        match self.lexer.peek() {
            Some(v) => v.name == token,
            None => false,
        }
    }

    fn expect_peek(&mut self, token: TokenType) -> Option<Token> {
        if self.peek_token_is(token) {
            self.lexer.next()
        } else {
            let got_token = self.lexer.peek().map(|v| v.name);
            self.peek_error(token, got_token);
            None
        }
    }

    fn peek_error(&mut self, et: TokenType, gt: Option<TokenType>) {
        let msg = match gt {
            Some(v) => format!("expected next token to be {:?}, Got {:?} instead", et, v),
            None => format!("expected next token to be {}, Got None instead", et),
        };
        self.errors.push(Error { reason: msg });
    }

    fn register_prefix(&mut self, token: TokenType, f: PrefixParseFn) {
        self.prefix_parse_fns.insert(token, f);
    }

    fn register_infix(&mut self, token: TokenType, f: InfixParseFn) {
        self.infix_parse_fns.insert(token, f);
    }

    fn no_prefix_parse_fn_error(&mut self, token: TokenType) {
        self.errors.push(Error {
            reason: format!("no prefix parse function for {} found", token),
        });
    }

    fn peek_precedence(&mut self) -> ExpressionPriority {
        match self.lexer.peek() {
            Some(token) => match PRECEDENCE_MAP.get(&token.name) {
                Some(p) => *p,
                None => ExpressionPriority::Lowest,
            },
            None => ExpressionPriority::Lowest,
        }
    }

    fn current_precedence(&mut self, token: TokenType) -> ExpressionPriority {
        match PRECEDENCE_MAP.get(&token) {
            Some(p) => *p,
            None => ExpressionPriority::Lowest,
        }
    }

    fn parse_expression_list(&mut self, end_token: TokenType) -> Option<Vec<Expression>> {
        let mut out = vec![];

        if self.peek_token_is(end_token) {
            self.lexer.next();
            return Some(out);
        }

        let next_token = self.lexer.next()?;
        out.push(Expression::parse(
            self,
            next_token,
            ExpressionPriority::Lowest,
        )?);

        while self.peek_token_is(TokenType::Comma) {
            self.lexer.next();
            let next_token = self.lexer.next()?;

            out.push(Expression::parse(
                self,
                next_token,
                ExpressionPriority::Lowest,
            )?);
        }

        self.expect_peek(end_token)?;

        Some(out)
    }
}

#[derive(PartialEq, Debug)]
pub struct Error {
    reason: String,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(&self.reason)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::{Lexer, Token, TokenType},
        parser::{ast::*, Parser},
    };

    fn check_parser_errors(p: &Parser) {
        if !p.errors.is_empty() {
            let mut out = String::new();
            out.push_str(&format!("parser has {} errors\n", p.errors.len()));
            for error in &p.errors {
                out.push_str(&format!("parser error: {}\n", error));
            }
            eprintln!("{}", out);
        }
    }

    fn check_test_cases(test_cases: &[(&str, Vec<Statement>)]) {
        for test in test_cases.iter() {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);
            assert_eq!(parser.errors.len(), 0);
            assert!(program.is_some());
            assert_eq!(program.unwrap().statements, test.1);
        }
    }

    #[test]
    fn let_statements() {
        let test_cases = [(
            "let x =5;let y=10; let foobar=538383;",
            vec![
                Statement::Let(LetStatement::with_value(
                    Identifier::new(TokenType::Let, "x"),
                    Expression::IntegerLiteral(IntegerLiteral::new(5)),
                )),
                Statement::Let(LetStatement::with_value(
                    Identifier::new(TokenType::Let, "y"),
                    Expression::IntegerLiteral(IntegerLiteral::new(10)),
                )),
                Statement::Let(LetStatement::with_value(
                    Identifier::new(TokenType::Let, "foobar"),
                    Expression::IntegerLiteral(IntegerLiteral::new(538383)),
                )),
            ],
        )];

        check_test_cases(&test_cases);

        let fail_case = "let x 5; let 10; let 83838383;";

        let lexer = Lexer::new(fail_case);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(parser.errors.len(), 3);
        assert!(program.is_some());
    }

    #[test]
    fn return_statements() {
        let test_cases = [(
            "return 5; return 10; return add(10);",
            vec![
                Statement::Return(ReturnStatement::new(Expression::IntegerLiteral(
                    IntegerLiteral::new(5),
                ))),
                Statement::Return(ReturnStatement::new(Expression::IntegerLiteral(
                    IntegerLiteral::new(10),
                ))),
                Statement::Return(ReturnStatement::new(Expression::CallExpression(
                    CallExpression::new(
                        Expression::Identifier(Identifier::new(TokenType::Ident, "add")),
                        vec![Expression::IntegerLiteral(IntegerLiteral::new(10))],
                    ),
                ))),
            ],
        )];

        check_test_cases(&test_cases);
    }

    #[test]
    fn identifier_expression() {
        // TODO: Add more tests for this
        let lexer = Lexer::new("foobar;");
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);
        assert_eq!(parser.errors.len(), 0);
        assert!(program.is_some());
        let program = program.unwrap();
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements,
            vec![Statement::ExpressionStatement(ExpressionStatement::new(
                token!(TokenType::Ident, "foobar"),
                Expression::Identifier(Identifier::new(TokenType::Ident, "foobar")),
            ))]
        );
    }

    #[test]
    fn integer_literal_expression() {
        let lexer = Lexer::new("5;");
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(parser.errors.len(), 0);
        assert!(program.is_some());

        assert_eq!(
            program.unwrap().statements,
            vec![Statement::ExpressionStatement(ExpressionStatement::new(
                token!(TokenType::Int, "5"),
                Expression::IntegerLiteral(IntegerLiteral::new(5))
            ))]
        );
    }

    #[test]
    fn string_literal_expression() {
        let lexer = Lexer::new("\"hello world\";");
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(parser.errors.len(), 0);
        assert!(program.is_some());

        assert_eq!(
            program.unwrap().statements,
            vec![Statement::ExpressionStatement(ExpressionStatement::new(
                token!(TokenType::String, "hello world"),
                Expression::StringLiteral(StringLiteral::new("hello world"))
            ))]
        );
    }

    #[test]
    fn prefix_expressions() {
        let test_cases = [
            (
                "!5",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Bang),
                    Expression::PrefixExpression(PrefixExpression::new(
                        TokenType::Bang,
                        Expression::IntegerLiteral(IntegerLiteral::new(5)),
                    )),
                ))],
            ),
            (
                "-15;",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Minus),
                    Expression::PrefixExpression(PrefixExpression::new(
                        TokenType::Minus,
                        Expression::IntegerLiteral(IntegerLiteral::new(15)),
                    )),
                ))],
            ),
            (
                "!foobar;",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Bang),
                    Expression::PrefixExpression(PrefixExpression::new(
                        TokenType::Bang,
                        Expression::Identifier(Identifier::new(TokenType::Ident, "foobar")),
                    )),
                ))],
            ),
            (
                "!true;",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Bang),
                    Expression::PrefixExpression(PrefixExpression::new(
                        TokenType::Bang,
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::True)),
                    )),
                ))],
            ),
            (
                "!false;",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Bang),
                    Expression::PrefixExpression(PrefixExpression::new(
                        TokenType::Bang,
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::False)),
                    )),
                ))],
            ),
            (
                "!isGreaterThanZero( 2);",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Bang),
                    Expression::PrefixExpression(PrefixExpression::new(
                        TokenType::Bang,
                        Expression::CallExpression(CallExpression::new(
                            Expression::Identifier(Identifier::new(
                                TokenType::Ident,
                                "isGreaterThanZero",
                            )),
                            vec![Expression::IntegerLiteral(IntegerLiteral::new(2))],
                        )),
                    )),
                ))],
            ),
        ];

        check_test_cases(&test_cases);
    }

    #[test]
    fn parsing_infix_expressions() {
        let test_cases = [
            (
                "5 + 10;",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Int, "5"),
                    Expression::InfixExpression(InfixExpression::new(
                        Expression::IntegerLiteral(IntegerLiteral::new(5)),
                        TokenType::Plus,
                        Expression::IntegerLiteral(IntegerLiteral::new(10)),
                    )),
                ))],
            ),
            (
                "5 - 10;",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Int, "5"),
                    Expression::InfixExpression(InfixExpression::new(
                        Expression::IntegerLiteral(IntegerLiteral::new(5)),
                        TokenType::Minus,
                        Expression::IntegerLiteral(IntegerLiteral::new(10)),
                    )),
                ))],
            ),
            (
                "5 * 15;",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Int, "5"),
                    Expression::InfixExpression(InfixExpression::new(
                        Expression::IntegerLiteral(IntegerLiteral::new(5)),
                        TokenType::Asterisk,
                        Expression::IntegerLiteral(IntegerLiteral::new(15)),
                    )),
                ))],
            ),
            (
                "15 / 3;",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Int, "15"),
                    Expression::InfixExpression(InfixExpression::new(
                        Expression::IntegerLiteral(IntegerLiteral::new(15)),
                        TokenType::Slash,
                        Expression::IntegerLiteral(IntegerLiteral::new(3)),
                    )),
                ))],
            ),
            (
                "5 > 15;",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Int, "5"),
                    Expression::InfixExpression(InfixExpression::new(
                        Expression::IntegerLiteral(IntegerLiteral::new(5)),
                        TokenType::GreaterThan,
                        Expression::IntegerLiteral(IntegerLiteral::new(15)),
                    )),
                ))],
            ),
            (
                "a + b + c;",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Ident, "a"),
                    Expression::InfixExpression(InfixExpression::new(
                        Expression::InfixExpression(InfixExpression::new(
                            Expression::Identifier(Identifier::new(TokenType::Ident, "a")),
                            TokenType::Plus,
                            Expression::Identifier(Identifier::new(TokenType::Ident, "b")),
                        )),
                        TokenType::Plus,
                        Expression::Identifier(Identifier::new(TokenType::Ident, "c")),
                    )),
                ))],
            ),
            (
                "true == true",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::True),
                    Expression::InfixExpression(InfixExpression::new(
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::True)),
                        TokenType::Equals,
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::True)),
                    )),
                ))],
            ),
            (
                "true != false",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::True),
                    Expression::InfixExpression(InfixExpression::new(
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::True)),
                        TokenType::NotEquals,
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::False)),
                    )),
                ))],
            ),
            (
                "false == false",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::False),
                    Expression::InfixExpression(InfixExpression::new(
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::False)),
                        TokenType::Equals,
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::False)),
                    )),
                ))],
            ),
        ];
        check_test_cases(&test_cases);
    }

    #[test]
    fn operator_precedence_parsing() {
        let test_cases = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
        ];

        for test in test_cases.iter() {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);
            assert_eq!(parser.errors.len(), 0);
            assert!(program.is_some());
            assert_eq!(program.unwrap().to_string(), test.1);
        }
    }

    #[test]
    fn boolean_expression() {
        let test_cases = [
            (
                "true;",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::True),
                    Expression::BooleanExpression(BooleanExpression::new(TokenType::True)),
                ))],
            ),
            (
                "false;",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::False),
                    Expression::BooleanExpression(BooleanExpression::new(TokenType::False)),
                ))],
            ),
            (
                "let foobar = true;",
                vec![Statement::Let(LetStatement::with_value(
                    Identifier::new(TokenType::Let, "foobar"),
                    Expression::BooleanExpression(BooleanExpression::new(TokenType::True)),
                ))],
            ),
        ];
        check_test_cases(&test_cases);
    }
    #[test]
    fn if_expression() {
        let test_cases = [(
            "if (x > y) { x };",
            vec![Statement::ExpressionStatement(ExpressionStatement::new(
                token!(TokenType::If),
                Expression::IfExpression(IfExpression::new(
                    Expression::InfixExpression(InfixExpression::new(
                        Expression::Identifier(Identifier::new(TokenType::Ident, "x")),
                        TokenType::GreaterThan,
                        Expression::Identifier(Identifier::new(TokenType::Ident, "y")),
                    )),
                    BlockStatement::new(vec![Statement::ExpressionStatement(
                        ExpressionStatement::new(
                            token!(TokenType::Ident, "x"),
                            Expression::Identifier(Identifier::new(TokenType::Ident, "x")),
                        ),
                    )]),
                    None,
                )),
            ))],
        )];

        check_test_cases(&test_cases);
    }
    #[test]
    fn if_else_expression() {
        let test_cases = [(
            "if (x > y) { x } else { y };",
            vec![Statement::ExpressionStatement(ExpressionStatement::new(
                token!(TokenType::If),
                Expression::IfExpression(IfExpression::new(
                    Expression::InfixExpression(InfixExpression::new(
                        Expression::Identifier(Identifier::new(TokenType::Ident, "x")),
                        TokenType::GreaterThan,
                        Expression::Identifier(Identifier::new(TokenType::Ident, "y")),
                    )),
                    BlockStatement::new(vec![Statement::ExpressionStatement(
                        ExpressionStatement::new(
                            token!(TokenType::Ident, "x"),
                            Expression::Identifier(Identifier::new(TokenType::Ident, "x")),
                        ),
                    )]),
                    Some(BlockStatement::new(vec![Statement::ExpressionStatement(
                        ExpressionStatement::new(
                            token!(TokenType::Ident, "y"),
                            Expression::Identifier(Identifier::new(TokenType::Ident, "y")),
                        ),
                    )])),
                )),
            ))],
        )];
        check_test_cases(&test_cases);
    }

    #[test]
    fn function_literal_parsing() {
        let test_cases = [
            (
                "fn(a,b) {x + y;}",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Function),
                    Expression::FunctionExpression(FunctionLiteral::new(
                        token!(TokenType::Function),
                        vec![
                            Identifier::new(TokenType::Ident, "a"),
                            Identifier::new(TokenType::Ident, "b"),
                        ],
                        BlockStatement::new(vec![Statement::ExpressionStatement(
                            ExpressionStatement::new(
                                token!(TokenType::Ident, "x"),
                                Expression::InfixExpression(InfixExpression::new(
                                    Expression::Identifier(Identifier::new(TokenType::Ident, "x")),
                                    TokenType::Plus,
                                    Expression::Identifier(Identifier::new(TokenType::Ident, "y")),
                                )),
                            ),
                        )]),
                    )),
                ))],
            ),
            (
                "fn() {}",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Function),
                    Expression::FunctionExpression(FunctionLiteral::new(
                        token!(TokenType::Function),
                        vec![],
                        BlockStatement::new(vec![]),
                    )),
                ))],
            ),
            (
                "fn(x,ya,z) {}",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Function),
                    Expression::FunctionExpression(FunctionLiteral::new(
                        token!(TokenType::Function),
                        vec![
                            Identifier::new(TokenType::Ident, "x"),
                            Identifier::new(TokenType::Ident, "ya"),
                            Identifier::new(TokenType::Ident, "z"),
                        ],
                        BlockStatement::new(vec![]),
                    )),
                ))],
            ),
            (
                "fn(a) {}",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Function),
                    Expression::FunctionExpression(FunctionLiteral::new(
                        token!(TokenType::Function),
                        vec![Identifier::new(TokenType::Ident, "a")],
                        BlockStatement::new(vec![]),
                    )),
                ))],
            ),
            (
                "fn(a,b,abc) {(x + y) * (a -b);}",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Function),
                    Expression::FunctionExpression(FunctionLiteral::new(
                        token!(TokenType::Function),
                        vec![
                            Identifier::new(TokenType::Ident, "a"),
                            Identifier::new(TokenType::Ident, "b"),
                            Identifier::new(TokenType::Ident, "abc"),
                        ],
                        BlockStatement::new(vec![Statement::ExpressionStatement(
                            ExpressionStatement::new(
                                token!(TokenType::LParen),
                                Expression::InfixExpression(InfixExpression::new(
                                    Expression::InfixExpression(InfixExpression::new(
                                        Expression::Identifier(Identifier::new(
                                            TokenType::Ident,
                                            "x",
                                        )),
                                        TokenType::Plus,
                                        Expression::Identifier(Identifier::new(
                                            TokenType::Ident,
                                            "y",
                                        )),
                                    )),
                                    TokenType::Asterisk,
                                    Expression::InfixExpression(InfixExpression::new(
                                        Expression::Identifier(Identifier::new(
                                            TokenType::Ident,
                                            "a",
                                        )),
                                        TokenType::Minus,
                                        Expression::Identifier(Identifier::new(
                                            TokenType::Ident,
                                            "b",
                                        )),
                                    )),
                                )),
                            ),
                        )]),
                    )),
                ))],
            ),
        ];

        check_test_cases(&test_cases);
    }
    #[test]
    fn call_expression_parsing() {
        let test_cases = [
            (
                "add(1, 2 * 3, 4 + 5);",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Ident, "add"),
                    Expression::CallExpression(CallExpression::new(
                        Expression::Identifier(Identifier::new(TokenType::Ident, "add")),
                        vec![
                            Expression::IntegerLiteral(IntegerLiteral::new(1)),
                            Expression::InfixExpression(InfixExpression::new(
                                Expression::IntegerLiteral(IntegerLiteral::new(2)),
                                TokenType::Asterisk,
                                Expression::IntegerLiteral(IntegerLiteral::new(3)),
                            )),
                            Expression::InfixExpression(InfixExpression::new(
                                Expression::IntegerLiteral(IntegerLiteral::new(4)),
                                TokenType::Plus,
                                Expression::IntegerLiteral(IntegerLiteral::new(5)),
                            )),
                        ],
                    )),
                ))],
            ),
            (
                "a + add(1, 2 * 3, 4 + 5) + d;",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Ident, "a"),
                    Expression::InfixExpression(InfixExpression::new(
                        Expression::InfixExpression(InfixExpression::new(
                            Expression::Identifier(Identifier::new(TokenType::Ident, "a")),
                            TokenType::Plus,
                            Expression::CallExpression(CallExpression::new(
                                Expression::Identifier(Identifier::new(TokenType::Ident, "add")),
                                vec![
                                    Expression::IntegerLiteral(IntegerLiteral::new(1)),
                                    Expression::InfixExpression(InfixExpression::new(
                                        Expression::IntegerLiteral(IntegerLiteral::new(2)),
                                        TokenType::Asterisk,
                                        Expression::IntegerLiteral(IntegerLiteral::new(3)),
                                    )),
                                    Expression::InfixExpression(InfixExpression::new(
                                        Expression::IntegerLiteral(IntegerLiteral::new(4)),
                                        TokenType::Plus,
                                        Expression::IntegerLiteral(IntegerLiteral::new(5)),
                                    )),
                                ],
                            )),
                        )),
                        TokenType::Plus,
                        Expression::Identifier(Identifier::new(TokenType::Ident, "d")),
                    )),
                ))],
            ),
            (
                "add(a,b,1, 2 * 3, 4 + 5, add(6,7 *8));",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Ident, "add"),
                    Expression::CallExpression(CallExpression::new(
                        Expression::Identifier(Identifier::new(TokenType::Ident, "add")),
                        vec![
                            Expression::Identifier(Identifier::new(TokenType::Ident, "a")),
                            Expression::Identifier(Identifier::new(TokenType::Ident, "b")),
                            Expression::IntegerLiteral(IntegerLiteral::new(1)),
                            Expression::InfixExpression(InfixExpression::new(
                                Expression::IntegerLiteral(IntegerLiteral::new(2)),
                                TokenType::Asterisk,
                                Expression::IntegerLiteral(IntegerLiteral::new(3)),
                            )),
                            Expression::InfixExpression(InfixExpression::new(
                                Expression::IntegerLiteral(IntegerLiteral::new(4)),
                                TokenType::Plus,
                                Expression::IntegerLiteral(IntegerLiteral::new(5)),
                            )),
                            Expression::CallExpression(CallExpression::new(
                                Expression::Identifier(Identifier::new(TokenType::Ident, "add")),
                                vec![
                                    Expression::IntegerLiteral(IntegerLiteral::new(6)),
                                    Expression::InfixExpression(InfixExpression::new(
                                        Expression::IntegerLiteral(IntegerLiteral::new(7)),
                                        TokenType::Asterisk,
                                        Expression::IntegerLiteral(IntegerLiteral::new(8)),
                                    )),
                                ],
                            )),
                        ],
                    )),
                ))],
            ),
            (
                "add(a + b + c * d / f + g);",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Ident, "add"),
                    Expression::CallExpression(CallExpression::new(
                        Expression::Identifier(Identifier::new(TokenType::Ident, "add")),
                        vec![Expression::InfixExpression(InfixExpression::new(
                            Expression::InfixExpression(InfixExpression::new(
                                Expression::InfixExpression(InfixExpression::new(
                                    Expression::Identifier(Identifier::new(TokenType::Ident, "a")),
                                    TokenType::Plus,
                                    Expression::Identifier(Identifier::new(TokenType::Ident, "b")),
                                )),
                                TokenType::Plus,
                                Expression::InfixExpression(InfixExpression::new(
                                    Expression::InfixExpression(InfixExpression::new(
                                        Expression::Identifier(Identifier::new(
                                            TokenType::Ident,
                                            "c",
                                        )),
                                        TokenType::Asterisk,
                                        Expression::Identifier(Identifier::new(
                                            TokenType::Ident,
                                            "d",
                                        )),
                                    )),
                                    TokenType::Slash,
                                    Expression::Identifier(Identifier::new(TokenType::Ident, "f")),
                                )),
                            )),
                            TokenType::Plus,
                            Expression::Identifier(Identifier::new(TokenType::Ident, "g")),
                        ))],
                    )),
                ))],
            ),
            (
                "add();",
                vec![Statement::ExpressionStatement(ExpressionStatement::new(
                    token!(TokenType::Ident, "add"),
                    Expression::CallExpression(CallExpression::new(
                        Expression::Identifier(Identifier::new(TokenType::Ident, "add")),
                        vec![],
                    )),
                ))],
            ),
        ];

        check_test_cases(&test_cases);
    }
    #[test]
    fn call_expression_parsing_string() {
        let test_cases = [
            ("add(1, 2 * 3, 4 + 5);", "add(1, (2 * 3), (4 + 5))"),
            (
                "a + add(1, 2 * 3, 4 + 5) + d;",
                "((a + add(1, (2 * 3), (4 + 5))) + d)",
            ),
            (
                "add(a,b,1, 2 * 3, 4 + 5, add(6,7 *8));",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g);",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            ("add();", "add()"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        ];

        for test in test_cases.iter() {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);
            assert_eq!(parser.errors.len(), 0);
            assert!(program.is_some());
            assert_eq!(program.unwrap().to_string(), test.1);
        }
    }

    #[test]
    fn array_literals() {
        let test_cases = [(
            "[1, 2 * 2, 3 + 3]",
            vec![Statement::ExpressionStatement(ExpressionStatement::new(
                token!(TokenType::LBracket),
                Expression::ArrayLiteral(ArrayLiteral::new(vec![
                    Expression::IntegerLiteral(IntegerLiteral::new(1)),
                    Expression::InfixExpression(InfixExpression::new(
                        Expression::IntegerLiteral(IntegerLiteral::new(2)),
                        TokenType::Asterisk,
                        Expression::IntegerLiteral(IntegerLiteral::new(2)),
                    )),
                    Expression::InfixExpression(InfixExpression::new(
                        Expression::IntegerLiteral(IntegerLiteral::new(3)),
                        TokenType::Plus,
                        Expression::IntegerLiteral(IntegerLiteral::new(3)),
                    )),
                ])),
            ))],
        )];

        check_test_cases(&test_cases);
    }
}

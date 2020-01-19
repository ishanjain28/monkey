pub mod ast;
use {
    crate::{
        lexer::{Lexer, Token, TokenType},
        parser::ast::{Expression, ExpressionPriority, Program, Statement},
    },
    std::{
        collections::HashMap,
        fmt::{Display, Error as FmtError, Formatter},
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
        m
    };
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    errors: Vec<Error>,
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

        parser.register_prefix(TokenType::Ident, Expression::parse_identifier);
        parser.register_prefix(TokenType::Int, Expression::parse_integer_literal);
        parser.register_prefix(TokenType::Bang, Expression::parse_prefix_expression);
        parser.register_prefix(TokenType::Minus, Expression::parse_prefix_expression);
        parser.register_prefix(TokenType::True, Expression::parse_boolean);
        parser.register_prefix(TokenType::False, Expression::parse_boolean);

        parser.register_infix(TokenType::Plus, Expression::parse_infix_expression);
        parser.register_infix(TokenType::Minus, Expression::parse_infix_expression);
        parser.register_infix(TokenType::Slash, Expression::parse_infix_expression);
        parser.register_infix(TokenType::Asterisk, Expression::parse_infix_expression);
        parser.register_infix(TokenType::Equals, Expression::parse_infix_expression);
        parser.register_infix(TokenType::NotEquals, Expression::parse_infix_expression);
        parser.register_infix(TokenType::LessThan, Expression::parse_infix_expression);
        parser.register_infix(TokenType::GreaterThan, Expression::parse_infix_expression);
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
            let got_token = match self.lexer.peek() {
                Some(v) => Some(v.name),
                None => None,
            };
            self.peek_error(token, got_token);
            None
        }
    }

    fn peek_error(&mut self, et: TokenType, gt: Option<TokenType>) {
        let msg = match gt {
            Some(v) => format!("expected next token to be {:?}, Got {:?} instead", et, v),
            None => format!(
                "expected next token to be {}, Got None instead",
                et.to_string()
            ),
        };
        self.errors.push(Error { reason: msg });
    }

    fn register_prefix(&mut self, token: TokenType, f: PrefixParseFn) {
        self.prefix_parse_fns.insert(token, f);
    }

    fn register_infix(&mut self, token: TokenType, f: InfixParseFn) {
        self.infix_parse_fns.insert(token, f);
    }

    fn no_prefix_parse_fn_error(&mut self, token: &TokenType) {
        self.errors.push(Error {
            reason: format!("no prefix parse function for {} found", token.to_string()),
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

    fn current_precedence(&mut self, token: &TokenType) -> ExpressionPriority {
        match PRECEDENCE_MAP.get(&token) {
            Some(p) => *p,
            None => ExpressionPriority::Lowest,
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Error {
    reason: String,
}

impl Display for Error {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), FmtError> {
        write!(fmt, "{}", self.reason)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::{Lexer, Token, TokenType},
        parser::{ast::*, Parser},
    };

    fn check_parser_errors(p: &Parser) {
        if p.errors.is_empty() {
            return;
        } else {
            let mut out = String::new();
            out.push_str(&format!("parser has {} errors\n", p.errors.len()));
            for error in &p.errors {
                out.push_str(&format!("parser error: {}\n", error));
            }
            eprintln!("{}", out);
        }
    }

    #[test]
    fn let_statements() {
        let mut lexer = Lexer::new("let x =5;let y=10; let foobar=538383;");
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert!(program.is_some());
        let program = program.unwrap();
        assert_eq!(program.statements.len(), 3);
        assert_eq!(
            program,
            Program {
                statements: vec![
                    Statement::Let(LetStatement {
                        name: Identifier::new(TokenType::Let, "x"),
                        value: None
                    }),
                    Statement::Let(LetStatement {
                        name: Identifier::new(TokenType::Let, "y"),
                        value: None
                    }),
                    Statement::Let(LetStatement {
                        name: Identifier::new(TokenType::Let, "foobar"),
                        value: None
                    })
                ],
            }
        );

        lexer = Lexer::new("let x 5; let 10; let 83838383;");
        parser = Parser::new(lexer);
        let _program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(parser.errors.len(), 3);
    }

    #[test]
    fn return_statements() {
        let lexer = Lexer::new("return 5; return 10; return add(10);");
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);
        assert!(program.is_some());
        let program = program.unwrap();
        assert_eq!(program.statements.len(), 3);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn identifier_expression() {
        let lexer = Lexer::new("foobar;");
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);
        assert!(program.is_some());
        let program = program.unwrap();
        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements,
            vec![Statement::ExpressionStatement(ExpressionStatement {
                token: Token::with_value(TokenType::Ident, "foobar"),
                expression: Expression::Identifier(Identifier::new(TokenType::Ident, "foobar")),
            })]
        );
    }

    #[test]
    fn integer_literal_expression() {
        let lexer = Lexer::new("5;");
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert!(program.is_some());

        assert_eq!(
            program.unwrap().statements,
            vec![Statement::ExpressionStatement(ExpressionStatement {
                token: Token::with_value(TokenType::Int, "5"),
                expression: Expression::IntegerLiteral(IntegerLiteral::new(TokenType::Int, 5))
            })]
        );
    }

    #[test]
    fn prefix_expressions() {
        let prefix_tests = [
            (
                "!5",
                vec![Statement::ExpressionStatement(ExpressionStatement {
                    token: Token::new(TokenType::Bang),
                    expression: Expression::PrefixExpression(PrefixExpression::new(
                        Token::new(TokenType::Bang),
                        "!",
                        Expression::IntegerLiteral(IntegerLiteral::new(TokenType::Int, 5)),
                    )),
                })],
            ),
            (
                "-15;",
                vec![Statement::ExpressionStatement(ExpressionStatement {
                    token: Token::new(TokenType::Minus),
                    expression: Expression::PrefixExpression(PrefixExpression::new(
                        Token::new(TokenType::Minus),
                        "-",
                        Expression::IntegerLiteral(IntegerLiteral::new(TokenType::Int, 15)),
                    )),
                })],
            ),
            (
                "!foobar;",
                vec![Statement::ExpressionStatement(ExpressionStatement {
                    token: Token::new(TokenType::Bang),
                    expression: Expression::PrefixExpression(PrefixExpression::new(
                        Token::new(TokenType::Bang),
                        "!",
                        Expression::Identifier(Identifier::new(TokenType::Ident, "foobar")),
                    )),
                })],
            ),
            (
                "!true;",
                vec![Statement::ExpressionStatement(ExpressionStatement {
                    token: Token::new(TokenType::Bang),
                    expression: Expression::PrefixExpression(PrefixExpression::new(
                        Token::new(TokenType::Bang),
                        "!",
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::True)),
                    )),
                })],
            ),
            (
                "!false;",
                vec![Statement::ExpressionStatement(ExpressionStatement {
                    token: Token::new(TokenType::Bang),
                    expression: Expression::PrefixExpression(PrefixExpression::new(
                        Token::new(TokenType::Bang),
                        "!",
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::False)),
                    )),
                })],
            ),
            // TODO: Add this test when we add function call parser
            // (
            //     "!isGreaterThanZero( 2);",
            //     vec![Statement::ExpressionStatement(ExpressionStatement {
            //         token: Token::new(TokenType::Bang),
            //         expression: Expression::PrefixExpression(PrefixExpression::new(
            //             Token::new(TokenType::Bang),
            //             "!",
            //             Expression::Identifier(Identifier::new(
            //                 TokenType::Function,
            //                 "",
            //             )),
            //         )),
            //     })
        ];

        for test in prefix_tests.iter() {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);
            assert!(program.is_some());
            assert_eq!(program.unwrap().statements, test.1);
        }
    }

    #[test]
    fn parsing_infix_expressions() {
        let infix_tests = [
            (
                "5 + 10;",
                vec![Statement::ExpressionStatement(ExpressionStatement {
                    token: Token::with_value(TokenType::Int, "5"),
                    expression: Expression::InfixExpression(InfixExpression::new(
                        Token::new(TokenType::Plus),
                        Expression::IntegerLiteral(IntegerLiteral::new(TokenType::Int, 5)),
                        "+",
                        Expression::IntegerLiteral(IntegerLiteral::new(TokenType::Int, 10)),
                    )),
                })],
            ),
            (
                "5 - 10;",
                vec![Statement::ExpressionStatement(ExpressionStatement {
                    token: Token::with_value(TokenType::Int, "5"),
                    expression: Expression::InfixExpression(InfixExpression::new(
                        Token::new(TokenType::Minus),
                        Expression::IntegerLiteral(IntegerLiteral::new(TokenType::Int, 5)),
                        "-",
                        Expression::IntegerLiteral(IntegerLiteral::new(TokenType::Int, 10)),
                    )),
                })],
            ),
            (
                "5 * 15;",
                vec![Statement::ExpressionStatement(ExpressionStatement {
                    token: Token::with_value(TokenType::Int, "5"),
                    expression: Expression::InfixExpression(InfixExpression::new(
                        Token::new(TokenType::Asterisk),
                        Expression::IntegerLiteral(IntegerLiteral::new(TokenType::Int, 5)),
                        "*",
                        Expression::IntegerLiteral(IntegerLiteral::new(TokenType::Int, 15)),
                    )),
                })],
            ),
            (
                "15 / 3;",
                vec![Statement::ExpressionStatement(ExpressionStatement {
                    token: Token::with_value(TokenType::Int, "15"),
                    expression: Expression::InfixExpression(InfixExpression::new(
                        Token::new(TokenType::Slash),
                        Expression::IntegerLiteral(IntegerLiteral::new(TokenType::Int, 15)),
                        "/",
                        Expression::IntegerLiteral(IntegerLiteral::new(TokenType::Int, 3)),
                    )),
                })],
            ),
            (
                "5 > 15;",
                vec![Statement::ExpressionStatement(ExpressionStatement {
                    token: Token::with_value(TokenType::Int, "5"),
                    expression: Expression::InfixExpression(InfixExpression::new(
                        Token::new(TokenType::GreaterThan),
                        Expression::IntegerLiteral(IntegerLiteral::new(TokenType::Int, 5)),
                        ">",
                        Expression::IntegerLiteral(IntegerLiteral::new(TokenType::Int, 15)),
                    )),
                })],
            ),
            (
                "a + b + c;",
                vec![Statement::ExpressionStatement(ExpressionStatement {
                    token: Token::with_value(TokenType::Ident, "a"),
                    expression: Expression::InfixExpression(InfixExpression::new(
                        Token::new(TokenType::Plus),
                        Expression::InfixExpression(InfixExpression::new(
                            Token::new(TokenType::Plus),
                            Expression::Identifier(Identifier::new(TokenType::Ident, "a")),
                            "+",
                            Expression::Identifier(Identifier::new(TokenType::Ident, "b")),
                        )),
                        "+",
                        Expression::Identifier(Identifier::new(TokenType::Ident, "c")),
                    )),
                })],
            ),
            (
                "true == true",
                vec![Statement::ExpressionStatement(ExpressionStatement {
                    token: Token::new(TokenType::True),
                    expression: Expression::InfixExpression(InfixExpression::new(
                        Token::new(TokenType::Equals),
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::True)),
                        "==",
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::True)),
                    )),
                })],
            ),
            (
                "true != false",
                vec![Statement::ExpressionStatement(ExpressionStatement {
                    token: Token::new(TokenType::True),
                    expression: Expression::InfixExpression(InfixExpression::new(
                        Token::new(TokenType::NotEquals),
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::True)),
                        "!=",
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::False)),
                    )),
                })],
            ),
            (
                "false == false",
                vec![Statement::ExpressionStatement(ExpressionStatement {
                    token: Token::new(TokenType::False),
                    expression: Expression::InfixExpression(InfixExpression::new(
                        Token::new(TokenType::Equals),
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::False)),
                        "==",
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::False)),
                    )),
                })],
            ),
        ];
        for test in infix_tests.iter() {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);
            assert!(program.is_some());
            assert_eq!(program.unwrap().statements, test.1);
        }
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
        ];

        for test in test_cases.iter() {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);
            assert!(program.is_some());
            assert_eq!(program.unwrap().to_string(), test.1);
        }
    }

    #[test]
    fn boolean_expression() {
        let test_cases = [
            (
                "true;",
                Program {
                    statements: vec![Statement::ExpressionStatement(ExpressionStatement::new(
                        Token::new(TokenType::True),
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::True)),
                    ))],
                },
            ),
            (
                "false;",
                Program {
                    statements: vec![Statement::ExpressionStatement(ExpressionStatement::new(
                        Token::new(TokenType::False),
                        Expression::BooleanExpression(BooleanExpression::new(TokenType::False)),
                    ))],
                },
            ),
            (
                "let foobar = true;",
                Program {
                    statements: vec![Statement::Let(LetStatement::new(
                        Identifier::new(TokenType::Let, "foobar"),
                        None, // TODO: fix this when we complete parsing of let statements
                    ))],
                },
            ),
        ];

        for test in test_cases.iter() {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);
            assert!(program.is_some());
            assert_eq!(program.unwrap(), test.1);
        }
    }
}

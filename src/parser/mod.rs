pub mod ast;
use {
    crate::{
        lexer::{Lexer, Token, TokenType},
        parser::ast::{Expression, Program, Statement},
    },
    std::{
        collections::HashMap,
        fmt::{Display, Error as FmtError, Formatter},
        iter::Peekable,
    },
};

type PrefixParseFn = fn(&mut Parser, token: Token) -> Option<Expression>;
type InfixParseFn = fn(Expression) -> Option<Expression>;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    errors: Vec<ParserError>,
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
        parser
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while let Some(token) = self.lexer.next() {
            if token.name == TokenType::EOF {
                break;
            }
            match Statement::parse(self, token) {
                Some(v) => program.statements.push(v),
                None => {} // This will happen in case of a parsing error or something
            }
        }

        program
    }

    fn peek_token_is(&mut self, token: TokenType) -> bool {
        match self.lexer.peek() {
            Some(v) => v.name == token,
            None => false,
        }
    }

    // TODO: Remove this. We most likely don't need it anywhere
    // fn current_token_is(&self, token: TokenType) -> bool {
    //     false
    // }

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
            None => format!("expected next token to be {:?}, Got None instead", et),
        };
        self.errors.push(ParserError { reason: msg });
    }

    fn register_prefix(&mut self, token: TokenType, f: PrefixParseFn) {
        self.prefix_parse_fns.insert(token, f);
    }

    fn register_infix(&mut self, token: TokenType, f: InfixParseFn) {
        self.infix_parse_fns.insert(token, f);
    }
}

pub struct ParserError {
    reason: String,
}

impl Display for ParserError {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), FmtError> {
        write!(fmt, "{}", self.reason)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::{Lexer, Token, TokenType},
        parser::{
            ast::{
                Expression, ExpressionStatement, Identifier, IntegerLiteral, LetStatement, Program,
                Statement,
            },
            Parser,
        },
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
        assert_eq!(program.statements.len(), 3);
        assert_eq!(parser.errors.len(), 0);
    }
    #[test]
    fn identifier_expression() {
        let lexer = Lexer::new("foobar;");
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);
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

        assert_eq!(
            program.statements,
            vec![Statement::ExpressionStatement(ExpressionStatement {
                token: Token::with_value(TokenType::Int, "5"),
                expression: Expression::IntegerLiteral(IntegerLiteral::new(TokenType::Int, 5))
            })]
        );
    }
}

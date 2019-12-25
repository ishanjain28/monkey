pub mod ast;
use {
    crate::{
        lexer::{Lexer, Token, TokenType},
        parser::ast::{Program, Statement},
    },
    std::{
        fmt::{Display, Error as FmtError, Formatter},
        iter::Peekable,
    },
};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    pub errors: Vec<ParserError>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable(),
            errors: vec![],
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while let Some(token) = self.lexer.next() {
            if token.name == TokenType::EOF {
                break;
            }
            match Statement::parse(self, token.clone()) {
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
        lexer::{Lexer, TokenType},
        parser::{
            ast::{Identifier, LetStatement, Program, Statement},
            Parser,
        },
    };
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

        lexer = Lexer::new("let x = 5;let x 5;let = 10; let 83838383;");
        parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(parser.errors.len(), 3);
        assert_eq!(program.statements.len(), 1);
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
}

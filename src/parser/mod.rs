pub mod ast;
use {
    crate::{
        lexer::{Lexer, Token, TokenType},
        parser::ast::{Program, Statement},
    },
    std::iter::Peekable,
};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable(),
        }
    }

    pub fn parse_program(mut self) -> Program {
        let mut program = Program { statements: vec![] };

        loop {
            let token = self.lexer.next().unwrap();
            if token.name == TokenType::EOF {
                break;
            }

            match Statement::parse(&mut self, token) {
                Some(v) => program.statements.push(v),
                None => todo!(), // This will happen in case of a parsing error or something
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
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::{Lexer, TokenType},
        parser::{
            ast::{Identifier, LetStatement, Statement},
            Parser,
        },
    };
    #[test]
    fn let_statements() {
        let lexer = Lexer::new("let x =5;let y=10; let foobar=538383;");
        let parser = Parser::new(lexer);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 3);

        assert_eq!(
            program.statements,
            vec![
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
            ]
        );
    }
}

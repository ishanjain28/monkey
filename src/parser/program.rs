use crate::lexer::{Lexer, Token};
use crate::parser::ast::{LetStatement, Statement};
use crate::parser::Parser;

pub struct Program {
    statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn token_literal(&self) -> &'static str {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }

    pub fn parse(mut lexer: Lexer) -> Program {
        let mut statements = vec![];

        loop {
            if let Some(token) = lexer.next() {
                if token == Token::EOF {
                    break;
                }

                if let Some(stmt) = Parser::parse_statement(token, &mut lexer) {
                    statements.push(stmt);
                } else {
                    continue;
                }
            } else {
                break;
            }
        }

        Program { statements }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Program;

    #[test]
    fn let_statements() {
        let ip = "
        let yr = 5;
        let qq = 10;
        let foobar = 8388383;
        ";

        let lexer = Lexer::new(ip);
        let ast_tree = Program::parse(lexer);

        if ast_tree.statements.len() != 3 {
            assert_eq!(ast_tree.statements.len(), 3);
        }
    }
}

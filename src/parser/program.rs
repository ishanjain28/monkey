use crate::lexer::{Lexer, Token};
use crate::parser::ast::Statement;
use crate::parser::Parser;

pub struct Program {
    statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn parse(lexer: Lexer) -> Program {
        let mut statements = vec![];
        let mut parser = Parser::new(lexer);
        loop {
            if let Some(token) = parser.lexer.next() {
                if token == Token::EOF {
                    break;
                }

                if let Some(stmt) = parser.parse_statement(token) {
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
    use crate::parser::ast::{Identifier, LetStatement};
    use crate::parser::Program;
    #[test]
    fn let_statements() {
        let ip = "
        let yr = 5;
        let qq = 10;
        let foobar = 8388383;
        ";
        let out = Program {
            statements: vec![
                Box::new(LetStatement::new(Identifier::new("yr"))),
                Box::new(LetStatement::new(Identifier::new("qq"))),
                Box::new(LetStatement::new(Identifier::new("foobar"))),
            ],
        };
        let lexer = Lexer::new(ip);
        let ast_tree = Program::parse(lexer);

        if ast_tree.statements.len() != 3 {
            assert_eq!(ast_tree.statements.len(), 3);
        }
        for (out, expected_out) in ast_tree.statements.iter().zip(out.statements.iter()) {
            //     assert!(out, expected_out);
        }
    }
}

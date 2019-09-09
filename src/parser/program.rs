use crate::lexer::{Lexer, Token};
use crate::parser::{ast::Statement, Parser};

pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn parse(lexer: Lexer) -> Program {
        let mut statements = vec![];
        let mut parser = Parser::new(lexer);
        loop {
            if let Some(token) = parser.lexer.next() {
                parser.current_token = Some(token.clone());
                if parser.current_token_is(Token::EOF) {
                    break;
                }

                match parser.parse_statement(token) {
                    Ok(v) => statements.push(v),
                    Err(e) => {
                        println!("{:?}", e);
                        continue;
                    }
                };
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
    use crate::parser::ast::{Identifier, Let, Statement};
    use crate::parser::Program;

    #[test]
    fn let_statements() {
        let ip = "
        let yr = 5;
        let qq = 10;
        let foobar = 8388383;
        ";

        let expected_out = vec![
            Statement::Let(Let::new(Identifier::new("yr"))),
            Statement::Let(Let::new(Identifier::new("qq"))),
            Statement::Let(Let::new(Identifier::new("foobar"))),
        ];
        let lexer = Lexer::new(ip);
        let as_tree = Program::parse(lexer);

        assert_eq!(as_tree.statements.len(), 3);

        for (out, expected_out) in as_tree.statements.into_iter().zip(expected_out.into_iter()) {
            assert_eq!(out, expected_out);
        }
    }

    #[test]
    fn return_statements() {
        let ip = "
        return 5;
        return 10;
        return 80932;
        ";

        let lexer = Lexer::new(ip);
        let as_tree = Program::parse(lexer);
        assert_eq!(as_tree.statements.len(), 3);
    }
}

use crate::lexer::{Lexer, Token, TokenType};
use crate::parser::{ast::Statement, Parser};

#[derive(Debug, PartialEq)]
pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn parse(lexer: Lexer) -> Program {
        let mut statements = vec![];
        let mut parser = Parser::new(lexer);
        while let Some(token) = parser.lexer.next() {
            parser.current_token = Some(token.clone());
            if parser.current_token_is(Token::new(TokenType::EOF)) {
                break;
            }

            match parser.parse_statement(token.name) {
                Ok(v) => statements.push(v),
                Err(e) => {
                    println!("{:?}", e);
                    continue;
                }
            };
        }

        Program { statements }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, Token, TokenType};
    use crate::parser::ast::{Expression, ExpressionStatement, Identifier, Let, Statement};
    use crate::parser::Program;

    #[test]
    fn let_statements() {
        let ip = "
        let yr = 5;
        let qq = 10;
        let foobar = 8388383;
        ";

        let expected_out = Program {
            statements: vec![
                Statement::Let(Let::new(
                    Identifier::new(Token::new(TokenType::Let), "yr"),
                    None
                    // Some(Expression::Ident(Identifier::new(
                    //     Token::new(TokenType::Let),
                    //     "5",
                    // ))),
                )),
                Statement::Let(Let::new(
                    Identifier::new(Token::new(TokenType::Let), "qq"),
                    None
                    // Some(Expression::Ident(Identifier::new(
                    //     Token::new(TokenType::Let),
                    //     "10",
                    // ))),
                )),
                Statement::Let(Let::new(
                    Identifier::new(Token::new(TokenType::Let), "foobar"),
                    None
                    // Some(Expression::Ident(Identifier::new(
                    //     Token::new(TokenType::Let),
                    //     "8388383",
                    // ))),
                )),
            ],
        };
        let lexer = Lexer::new(ip);
        let as_tree = Program::parse(lexer);

        assert_eq!(as_tree.statements.len(), 3);
        assert_eq!(as_tree, expected_out);
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

    #[test]
    fn identifier_expression() {
        let ip = "
        foobar;
        ";
        let lexer = Lexer::new(ip);
        let as_tree = Program::parse(lexer);
        let expected_out = Program {
            statements: vec![Statement::ExpressionStatement(ExpressionStatement::new(
                Some(Token::with_value(TokenType::Ident, "foobar".into())),
                Expression::Ident(Identifier::new(
                    Token::with_value(TokenType::Ident, "foobar".into()),
                    "foobar",
                )),
            ))],
        };

        println!("{:?}", as_tree);
        assert_eq!(as_tree.statements.len(), 1);
        assert_eq!(as_tree, expected_out);
    }
}

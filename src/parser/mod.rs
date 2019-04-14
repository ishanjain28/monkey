mod ast;
mod statement;

use self::statement::Statement;
use crate::lexer::{Lexer, Token};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        return Parser { lexer };
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Statement;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.next() {
            Some(Token::Let) => Statement::Let.parse(&mut self.lexer),
            _ => None,
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn let_statements() {
        let ip = "
        let yr = 5;
        let qq = 10;
        let foobar = 8388383;
        ";

        let lexer = Lexer::new(ip);

        let stmts = Parser::new(lexer);

        for stmt in stmts {
            println!("{:?}", stmt);
        }
    }
}

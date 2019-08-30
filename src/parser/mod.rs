mod ast;

use self::ast::{Let, Statement};
use super::lexer::Lexer;

pub struct Program {
    statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_owned()
        }
    }

    pub fn parse(lexer: Lexer) -> Program {
        let mut statements = vec![];

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
            eprintln!(
                "statements length not equal to 3. got {}",
                ast_tree.statements
            );
        }
    }
}

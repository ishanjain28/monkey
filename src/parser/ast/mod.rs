use crate::lexer::Lexer;

#[derive(Debug)]
pub struct LetStatement {
    name: String,
}

impl LetStatement {
    pub fn parse(lexer: &mut Lexer) -> Self {}
}

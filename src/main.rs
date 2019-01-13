#[macro_use]
extern crate lazy_static;

mod lexer;

fn main() {}

#[cfg(test)]
mod tests {
    use lexer::{Lexer, Token};
    #[test]
    fn new_token() {
        let input = "=+()[],;";
        let expected = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::Comma,
            Token::Semicolon,
            Token::EOF,
        ];

        let tokenized_output = Lexer::new(input).collect::<Vec<Token>>();

        println!("{:?}", tokenized_output);
        assert_eq!(expected.len(), tokenized_output.len());

        for (exp, actual) in expected.into_iter().zip(tokenized_output) {
            assert_eq!(actual, exp);
            println!("{:?} {:?}", actual, exp);
        }
    }
}

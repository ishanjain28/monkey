#[macro_use]
extern crate lazy_static;

mod lexer;

fn main() {}

#[cfg(test)]
mod tests {
    use lexer::{Lexer, Token};
    use std::collections::HashMap;

    #[test]
    fn new_token() {
        let mut tests = HashMap::new();

        tests.insert(
            "=+(){},;",
            vec![
                Token::Assign,
                Token::Plus,
                Token::LParen,
                Token::RParen,
                Token::LBrace,
                Token::RBrace,
                Token::Comma,
                Token::Semicolon,
                Token::EOF,
            ],
        );
        tests.insert(
            "let five = 5;
            let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);",
            vec![
                Token::Let,
                Token::Ident("five".to_string()),
                Token::Assign,
                Token::Int(5),
                Token::Semicolon,
                Token::Let,
                Token::Ident("ten".to_string()),
                Token::Assign,
                Token::Int(10),
                Token::Semicolon,
                Token::Let,
                Token::Ident("add".to_string()),
                Token::Assign,
                Token::Function,
                Token::LParen,
                Token::Ident("x".to_string()),
                Token::Comma,
                Token::Ident("y".to_string()),
                Token::RParen,
                Token::LBrace,
                Token::Ident("x".to_string()),
                Token::Plus,
                Token::Ident("y".to_string()),
                Token::Semicolon,
                Token::RBrace,
                Token::Semicolon,
                Token::Let,
                Token::Ident("result".to_string()),
                Token::Assign,
                Token::Ident("add".to_string()),
                Token::LParen,
                Token::Ident("five".to_string()),
                Token::Comma,
                Token::Ident("ten".to_string()),
                Token::RParen,
                Token::Semicolon,
                Token::EOF,
            ],
        );
        tests.insert(
            "let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if(5 < 10) {
            return true;
        } else {
            return false;
        }
        ",
            vec![
                Token::Let,
                Token::Ident("result".to_string()),
                Token::Assign,
                Token::Ident("add".to_string()),
                Token::LParen,
                Token::Ident("five".to_string()),
                Token::Comma,
                Token::Ident("ten".to_string()),
                Token::RParen,
                Token::Semicolon,
                Token::ExclamationMark,
                Token::Subtract,
                Token::Divide,
                Token::Multiply,
                Token::Int(5),
                Token::Semicolon,
                Token::Int(5),
                Token::LessThan,
                Token::Int(10),
                Token::GreaterThan,
                Token::Int(5),
                Token::Semicolon,
                Token::If,
                Token::LParen,
                Token::Int(5),
                Token::LessThan,
                Token::Int(10),
                Token::RParen,
                Token::LBrace,
                Token::Return,
                Token::True,
                Token::Semicolon,
                Token::RBrace,
                Token::Else,
                Token::LBrace,
                Token::Return,
                Token::False,
                Token::Semicolon,
                Token::RBrace,
                Token::EOF,
            ],
        );

        for (k, v) in tests {
            let tokenized_output = Lexer::new(k).collect::<Vec<Token>>();
            //            assert_eq!(v.len(), tokenized_output.len());

            for (exp, actual) in v.into_iter().zip(tokenized_output) {
                if actual != exp {
                    println!("Expect: {:?}, Actual: {:?}", actual, exp);
                }
                assert_eq!(actual, exp);
            }
        }
    }
}

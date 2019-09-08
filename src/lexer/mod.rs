use std::collections::HashMap;
use std::iter::Peekable;
use std::str::{self, Chars};

lazy_static! {
    static ref IDENTMAP: HashMap<&'static str, Token> = {
        let mut m = HashMap::new();
        m.insert("fn", Token::Function);
        m.insert("let", Token::Let);
        m.insert("true", Token::True);
        m.insert("false", Token::False);
        m.insert("return", Token::Return);
        m.insert("if", Token::If);
        m.insert("else", Token::Else);
        m
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Illegal,
    EOF,

    // Identifiers
    Int(i64),

    // Operators
    Assign,
    Plus,
    Multiply,
    Divide,
    Subtract,
    ExclamationMark,
    LessThan,
    GreaterThan,
    Equals,
    NotEquals,

    // Delimiter
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    If,
    Let,
    True,
    Else,
    False,
    Return,
    Ident(String),
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    eof_sent: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        let input = input.chars().peekable();
        Lexer {
            input,
            eof_sent: false,
        }
    }

    fn read_char(&mut self) -> Option<char> {
        self.input.next()
    }

    fn read_identifier(&mut self, first: char) -> String {
        let mut ident = Vec::new();
        ident.push(first);
        while self.peek_is_letter() {
            ident.push(self.read_char().unwrap());
        }
        ident.into_iter().collect::<String>()
    }

    fn peek_is_letter(&mut self) -> bool {
        match self.input.peek() {
            Some(v) => is_letter(*v),
            None => false,
        }
    }

    fn peek_is_ascii_digit(&mut self) -> bool {
        match self.input.peek() {
            Some(v) => v.is_ascii_digit(),
            None => false,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(&v) = self.input.peek() {
            if v == ' ' || v == '\t' || v == '\n' || v == '\r' {
                self.read_char();
            } else {
                break;
            }
        }
    }

    // use i64 for all numbers for now.
    fn read_number(&mut self, first: char) -> i64 {
        let mut number = Vec::new();
        number.push(first);
        while self.peek_is_ascii_digit() {
            number.push(self.read_char().unwrap());
        }
        number
            .into_iter()
            .collect::<String>()
            .parse::<i64>()
            .unwrap()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let ch = self.read_char();

        match ch {
            Some('=') => {
                let is_e = match self.input.peek() {
                    Some(v) if *v == '=' => true,
                    _ => false,
                };
                if is_e {
                    self.read_char();
                    Some(Token::Equals)
                } else {
                    Some(Token::Assign)
                }
            }
            Some('+') => Some(Token::Plus),
            Some('*') => Some(Token::Multiply),
            Some('/') => Some(Token::Divide),
            Some('-') => Some(Token::Subtract),
            Some(',') => Some(Token::Comma),
            Some(';') => Some(Token::Semicolon),
            Some('(') => Some(Token::LParen),
            Some(')') => Some(Token::RParen),
            Some('{') => Some(Token::LBrace),
            Some('}') => Some(Token::RBrace),
            Some('!') => {
                let is_ne = match self.input.peek() {
                    Some(v) if *v == '=' => true,
                    _ => false,
                };
                if is_ne {
                    self.read_char();
                    Some(Token::NotEquals)
                } else {
                    Some(Token::ExclamationMark)
                }
            }
            Some('>') => Some(Token::GreaterThan),
            Some('<') => Some(Token::LessThan),
            Some(ch) if is_letter(ch) => {
                let ident = self.read_identifier(ch);
                Some(lookup_ident(&ident))
            }
            Some(ch) if ch.is_ascii_digit() => {
                let number = self.read_number(ch);
                Some(Token::Int(number))
            }
            None if !self.eof_sent => {
                self.eof_sent = true;
                Some(Token::EOF)
            }
            None => None,
            _ => Some(Token::Illegal),
        }
    }
}

fn is_letter(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn lookup_ident(ident: &str) -> Token {
    match IDENTMAP.get(ident) {
        Some(v) => v.clone(),
        None => Token::Ident(ident.to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Token};
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

        10 == 10;
        9 != 10;

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
                Token::Int(10),
                Token::Equals,
                Token::Int(10),
                Token::Semicolon,
                Token::Int(9),
                Token::NotEquals,
                Token::Int(10),
                Token::Semicolon,
                Token::EOF,
            ],
        );

        for (k, v) in tests {
            let tokenized_output = Lexer::new(k).collect::<Vec<Token>>();
            assert_eq!(v.len(), tokenized_output.len());

            for (exp, actual) in v.into_iter().zip(tokenized_output) {
                if actual != exp {
                    println!("Expect: {:?}, Actual: {:?}", exp, actual);
                }
                assert_eq!(actual, exp);
            }
        }
    }
}

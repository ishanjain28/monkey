use std::collections::HashMap;
use std::convert::TryFrom;
use std::iter::Peekable;
use std::str::{self, Chars};

lazy_static! {
    static ref IDENTMAP: HashMap<&'static str, Token> = {
        let mut m = HashMap::new();
        m.insert("fn", Token::new(TokenType::Function));
        m.insert("let", Token::new(TokenType::Let));
        m.insert("true", Token::new(TokenType::True));
        m.insert("false", Token::new(TokenType::False));
        m.insert("return", Token::new(TokenType::Return));
        m.insert("if", Token::new(TokenType::If));
        m.insert("else", Token::new(TokenType::Else));
        m
    };
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TokenType {
    Illegal,
    EOF,

    // Identifiers
    Int,

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
    Ident,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Token {
    pub name: TokenType,
    pub value: Option<Literal>,
}

impl Token {
    #[inline]
    pub fn new(name: TokenType) -> Self {
        Token { name, value: None }
    }

    #[inline]
    pub fn with_value(name: TokenType, value: Literal) -> Self {
        Token {
            name,
            value: Some(value),
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Literal {
    String(String),
    Int(i64),
}

impl From<String> for Literal {
    fn from(s: String) -> Literal {
        Literal::String(s)
    }
}

impl From<&str> for Literal {
    fn from(s: &str) -> Literal {
        Literal::String(s.to_owned())
    }
}

impl From<i64> for Literal {
    fn from(i: i64) -> Literal {
        Literal::Int(i)
    }
}

impl TryFrom<Literal> for String {
    type Error = &'static str;
    fn try_from(l: Literal) -> Result<String, Self::Error> {
        match l {
            Literal::String(v) => Ok(v),
            Literal::Int(_) => Err("can not convert Int to String"),
        }
    }
}

impl TryFrom<Literal> for i64 {
    type Error = &'static str;
    fn try_from(l: Literal) -> Result<i64, Self::Error> {
        match l {
            Literal::Int(v) => Ok(v),
            Literal::String(_) => Err("can not convert String to Int"),
        }
    }
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
                    Some(Token::new(TokenType::Equals))
                } else {
                    Some(Token::new(TokenType::Assign))
                }
            }
            Some('+') => Some(Token::new(TokenType::Plus)),
            Some('*') => Some(Token::new(TokenType::Multiply)),
            Some('/') => Some(Token::new(TokenType::Divide)),
            Some('-') => Some(Token::new(TokenType::Subtract)),
            Some(',') => Some(Token::new(TokenType::Comma)),
            Some(';') => Some(Token::new(TokenType::Semicolon)),
            Some('(') => Some(Token::new(TokenType::LParen)),
            Some(')') => Some(Token::new(TokenType::RParen)),
            Some('{') => Some(Token::new(TokenType::LBrace)),
            Some('}') => Some(Token::new(TokenType::RBrace)),
            Some('!') => {
                let is_ne = match self.input.peek() {
                    Some(v) if *v == '=' => true,
                    _ => false,
                };
                if is_ne {
                    self.read_char();
                    Some(Token::new(TokenType::NotEquals))
                } else {
                    Some(Token::new(TokenType::ExclamationMark))
                }
            }
            Some('>') => Some(Token::new(TokenType::GreaterThan)),
            Some('<') => Some(Token::new(TokenType::LessThan)),
            Some(ch) if is_letter(ch) => {
                let ident = self.read_identifier(ch);
                Some(lookup_ident(&ident))
            }
            Some(ch) if ch.is_ascii_digit() => {
                let number = self.read_number(ch);
                Some(Token::with_value(TokenType::Int, (number as i64).into()))
            }
            None if !self.eof_sent => {
                self.eof_sent = true;
                Some(Token::new(TokenType::EOF))
            }
            None => None,
            _ => Some(Token::new(TokenType::Illegal)),
        }
    }
}

fn is_letter(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn lookup_ident(ident: &str) -> Token {
    match IDENTMAP.get(ident) {
        Some(v) => v.clone(),
        None => Token::with_value(TokenType::Ident, ident.into()),
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Literal, Token, TokenType};
    use std::collections::HashMap;

    #[test]
    fn new() {
        let mut tests = HashMap::new();

        tests.insert(
            "=+(){},;",
            vec![
                Token::new(TokenType::Assign),
                Token::new(TokenType::Plus),
                Token::new(TokenType::LParen),
                Token::new(TokenType::RParen),
                Token::new(TokenType::LBrace),
                Token::new(TokenType::RBrace),
                Token::new(TokenType::Comma),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::EOF),
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
                Token::new(TokenType::Let),
                Token::with_value(TokenType::Ident, "five".into()),
                Token::new(TokenType::Assign),
                Token::with_value(TokenType::Int, 5.into()),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::Let),
                Token::with_value(TokenType::Ident, "ten".into()),
                Token::new(TokenType::Assign),
                Token::with_value(TokenType::Int, 10.into()),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::Let),
                Token::with_value(TokenType::Ident, "add".into()),
                Token::new(TokenType::Assign),
                Token::new(TokenType::Function),
                Token::new(TokenType::LParen),
                Token::with_value(TokenType::Ident, "x".into()),
                Token::new(TokenType::Comma),
                Token::with_value(TokenType::Ident, "y".into()),
                Token::new(TokenType::RParen),
                Token::new(TokenType::LBrace),
                Token::with_value(TokenType::Ident, "x".into()),
                Token::new(TokenType::Plus),
                Token::with_value(TokenType::Ident, "y".into()),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::RBrace),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::Let),
                Token::with_value(TokenType::Ident, "result".into()),
                Token::new(TokenType::Assign),
                Token::with_value(TokenType::Ident, "add".into()),
                Token::new(TokenType::LParen),
                Token::with_value(TokenType::Ident, "five".into()),
                Token::new(TokenType::Comma),
                Token::with_value(TokenType::Ident, "ten".into()),
                Token::new(TokenType::RParen),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::EOF),
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
                Token::new(TokenType::Let),
                Token::with_value(TokenType::Ident, "result".into()),
                Token::new(TokenType::Assign),
                Token::with_value(TokenType::Ident, "add".into()),
                Token::new(TokenType::LParen),
                Token::with_value(TokenType::Ident, "five".into()),
                Token::new(TokenType::Comma),
                Token::with_value(TokenType::Ident, "ten".into()),
                Token::new(TokenType::RParen),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::ExclamationMark),
                Token::new(TokenType::Subtract),
                Token::new(TokenType::Divide),
                Token::new(TokenType::Multiply),
                Token::with_value(TokenType::Int, 5.into()),
                Token::new(TokenType::Semicolon),
                Token::with_value(TokenType::Int, 5.into()),
                Token::new(TokenType::LessThan),
                Token::with_value(TokenType::Int, 10.into()),
                Token::new(TokenType::GreaterThan),
                Token::with_value(TokenType::Int, 5.into()),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::If),
                Token::new(TokenType::LParen),
                Token::with_value(TokenType::Int, 5.into()),
                Token::new(TokenType::LessThan),
                Token::with_value(TokenType::Int, 10.into()),
                Token::new(TokenType::RParen),
                Token::new(TokenType::LBrace),
                Token::new(TokenType::Return),
                Token::new(TokenType::True),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::RBrace),
                Token::new(TokenType::Else),
                Token::new(TokenType::LBrace),
                Token::new(TokenType::Return),
                Token::new(TokenType::False),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::RBrace),
                Token::with_value(TokenType::Int, 10.into()),
                Token::new(TokenType::Equals),
                Token::with_value(TokenType::Int, 10.into()),
                Token::new(TokenType::Semicolon),
                Token::with_value(TokenType::Int, 9.into()),
                Token::new(TokenType::NotEquals),
                Token::with_value(TokenType::Int, 10.into()),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::EOF),
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

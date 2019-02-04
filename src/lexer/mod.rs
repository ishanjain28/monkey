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

#[derive(Debug)]
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
            Some(v) => is_letter(v),
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

        let v = match ch {
            Some('=') => Some(Token::Assign),
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
            Some('!') => Some(Token::ExclamationMark),
            Some('>') => Some(Token::GreaterThan),
            Some('<') => Some(Token::LessThan),
            Some(ch @ _) if is_letter(&ch) => {
                let ident = self.read_identifier(ch);
                Some(lookup_ident(&ident))
            }
            Some(ch @ _) if ch.is_ascii_digit() => {
                let number = self.read_number(ch);
                Some(Token::Int(number))
            }
            None if !self.eof_sent => {
                self.eof_sent = true;
                Some(Token::EOF)
            }
            None => None,
            _ => Some(Token::Illegal),
        };
        v
    }
}

fn is_letter(c: &char) -> bool {
    c.is_ascii_alphabetic() || c == &'_'
}

fn lookup_ident(ident: &str) -> Token {
    match IDENTMAP.get(ident) {
        Some(v) => v.clone(),
        None => Token::Ident(ident.to_string()),
    }
}

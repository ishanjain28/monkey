use std::collections::HashMap;

lazy_static! {
    static ref IDENTMAP: HashMap<&'static str, Token> = {
        let mut m = HashMap::new();
        m.insert("fn", Token::Function);
        m.insert("let", Token::Let);
        m
    };
}

#[derive(Debug, PartialEq)]
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

    // Delimiter
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
}

#[derive(Debug)]
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            input: input.chars().collect::<Vec<char>>(),
            position: 0,
            read_position: 0,
            ch: '0',
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '0';
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }

        self.input[pos..self.position].iter().collect::<String>()
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.read_char();

        match self.ch {
            '=' => Some(Token::Assign),
            '+' => Some(Token::Plus),
            '*' => Some(Token::Multiply),
            '/' => Some(Token::Divide),
            '-' => Some(Token::Subtract),
            ',' => Some(Token::Comma),
            ';' => Some(Token::Semicolon),
            '(' => Some(Token::LParen),
            ')' => Some(Token::RParen),
            '[' => Some(Token::LBrace),
            ']' => Some(Token::RBrace),
            _ => None,
        }
    }
}

fn is_letter(c: char) -> bool {
    c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_'
}

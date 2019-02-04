use std::collections::HashMap;
use std::str;

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
pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            input: input.bytes().collect::<Vec<u8>>(),
            position: 0,
            read_position: 0,
            ch: 0,
        }
    }

    fn read_char(&mut self) {
        if self.read_position == self.input.len() {
            self.ch = 0;
        } else if self.read_position > self.input.len() {
            // 3 = ETX
            self.ch = 3;
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
        self.read_position -= 1;
        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char();
        }
    }

    // use i64 for all numbers for now.
    fn read_number(&mut self) -> i64 {
        let pos = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        self.read_position -= 1;
        String::from_utf8_lossy(&self.input[pos..self.position])
            .parse::<i64>()
            .unwrap()
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.read_char();
        self.skip_whitespace();

        let v = match self.ch {
            b'=' => Some(Token::Assign),
            b'+' => Some(Token::Plus),
            b'*' => Some(Token::Multiply),
            b'/' => Some(Token::Divide),
            b'-' => Some(Token::Subtract),
            b',' => Some(Token::Comma),
            b';' => Some(Token::Semicolon),
            b'(' => Some(Token::LParen),
            b')' => Some(Token::RParen),
            b'{' => Some(Token::LBrace),
            b'}' => Some(Token::RBrace),
            b'!' => Some(Token::ExclamationMark),
            b'>' => Some(Token::GreaterThan),
            b'<' => Some(Token::LessThan),
            0 => Some(Token::EOF),
            //ETX-> End of text. It's the value of self.ch after all the text is parsed.
            3 => None,
            _ if is_letter(self.ch) => {
                let ident = self.read_identifier();
                Some(lookup_ident(&ident))
            }
            _ if self.ch.is_ascii_digit() => {
                let number = self.read_number();
                Some(Token::Int(number))
            }
            _ => Some(Token::Illegal),
        };
        v
    }
}

fn is_letter(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_'
}

fn lookup_ident(ident: &str) -> Token {
    match IDENTMAP.get(ident) {
        Some(v) => v.clone(),
        None => Token::Ident(ident.to_string()),
    }
}

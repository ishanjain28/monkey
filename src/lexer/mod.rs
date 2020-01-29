use std::{
    collections::HashMap,
    fmt::{Display, Formatter, Result as FmtResult},
    iter::Peekable,
    str::{self, Chars},
};

lazy_static! {
    static ref IDENTMAP: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("fn", TokenType::Function);
        m.insert("let", TokenType::Let);
        m.insert("true", TokenType::True);
        m.insert("false", TokenType::False);
        m.insert("return", TokenType::Return);
        m.insert("if", TokenType::If);
        m.insert("else", TokenType::Else);
        m
    };
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum TokenType {
    Illegal,
    EOF,

    // Identifiers and Literals

    // Ident is basically most things that are not covered
    // by other variants of this enum.
    Ident,
    Int,
    // Operators
    Assign,
    Plus,
    Asterisk,
    Slash,
    Minus,
    Bang,
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
    Else,
    Let,
    True,
    False,
    Return,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(match self {
            TokenType::Assign => "=",
            TokenType::Plus => "+",
            TokenType::Asterisk => "*",
            TokenType::Slash => "/",
            TokenType::Minus => "-",
            TokenType::Bang => "!",
            TokenType::LessThan => "<",
            TokenType::GreaterThan => ">",
            TokenType::Equals => "==",
            TokenType::NotEquals => "!=",
            TokenType::Comma => ",",
            TokenType::Semicolon => ";",
            TokenType::LParen => "(",
            TokenType::RParen => ")",
            TokenType::LBrace => "{",
            TokenType::RBrace => "}",
            TokenType::Function => "fn",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::Let => "let",
            TokenType::True => "true",
            TokenType::False => "false",
            TokenType::Return => "return",
            TokenType::EOF => "EOF",
            TokenType::Illegal => "illegal",
            TokenType::Ident => "ident",
            TokenType::Int => "int",
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Token {
    pub name: TokenType,
    pub literal: Option<String>,
}

impl Token {
    #[inline]
    pub const fn new(name: TokenType) -> Self {
        Token {
            name,
            literal: None,
        }
    }

    #[inline]
    pub fn with_value(name: TokenType, value: &str) -> Self {
        Token {
            name,
            literal: Some(value.to_string()),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_fmt(format_args!("{}", self.name))
    }
}

macro_rules! token {
    ($token_name:expr) => {
        Token::new($token_name)
    };
    ($token_name:expr, $value:expr) => {
        Token::with_value($token_name, $value)
    };
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    eof_sent: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer {
        let input = input.chars().peekable();
        Lexer {
            input,
            eof_sent: false,
        }
    }

    // This consumes one char from input
    #[inline]
    fn read_char(&mut self) -> Option<char> {
        self.input.next()
    }

    // This reads an Identifier from input
    fn read_identifier(&mut self, first: char) -> String {
        let mut ident = String::new();
        ident.push(first);
        while self.peek_is_letter() {
            ident.push(self.read_char().unwrap());
        }
        ident
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
            match v {
                ' ' | '\t' | '\n' | '\r' => {
                    self.read_char();
                }
                _ => break,
            }
        }
    }

    // use i64 for all numbers for now.
    fn read_number(&mut self, first: char) -> String {
        let mut number = Vec::new();
        number.push(first);
        while self.peek_is_ascii_digit() {
            number.push(self.read_char().unwrap());
        }
        number.into_iter().collect()
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
                    Some(token!(TokenType::Equals))
                } else {
                    Some(token!(TokenType::Assign))
                }
            }
            Some('+') => Some(token!(TokenType::Plus)),
            Some('*') => Some(token!(TokenType::Asterisk)),
            Some('/') => Some(token!(TokenType::Slash)),
            Some('-') => Some(token!(TokenType::Minus)),
            Some(',') => Some(token!(TokenType::Comma)),
            Some(';') => Some(token!(TokenType::Semicolon)),
            Some('(') => Some(token!(TokenType::LParen)),
            Some(')') => Some(token!(TokenType::RParen)),
            Some('{') => Some(token!(TokenType::LBrace)),
            Some('}') => Some(token!(TokenType::RBrace)),
            Some('!') => {
                let is_ne = match self.input.peek() {
                    Some(v) if *v == '=' => true,
                    _ => false,
                };
                if is_ne {
                    self.read_char();
                    Some(token!(TokenType::NotEquals))
                } else {
                    Some(token!(TokenType::Bang))
                }
            }
            Some('>') => Some(token!(TokenType::GreaterThan)),
            Some('<') => Some(token!(TokenType::LessThan)),
            Some(ch) if is_letter(ch) => {
                let ident = self.read_identifier(ch);
                Some(lookup_ident(&ident))
            }
            Some(ch) if ch.is_ascii_digit() => {
                let number = self.read_number(ch);
                Some(token!(TokenType::Int, &number))
            }
            None if !self.eof_sent => {
                self.eof_sent = true;
                Some(token!(TokenType::EOF))
            }
            None => None,
            _ => Some(token!(TokenType::Illegal)),
        }
    }
}

#[inline]
fn is_letter(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn lookup_ident(ident: &str) -> Token {
    match IDENTMAP.get(&ident) {
        Some(v) => token!(*v),
        None => token!(TokenType::Ident, ident),
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Token, TokenType};

    #[test]
    fn new() {
        assert_eq!(
            Lexer::new("=+(){},;").collect::<Vec<Token>>(),
            vec![
                token!(TokenType::Assign),
                token!(TokenType::Plus),
                token!(TokenType::LParen),
                token!(TokenType::RParen),
                token!(TokenType::LBrace),
                token!(TokenType::RBrace),
                token!(TokenType::Comma),
                token!(TokenType::Semicolon),
                token!(TokenType::EOF),
            ],
        );

        assert_eq!(
            Lexer::new(
                "let five = 5;
                let ten = 10;

                let add = fn(x, y) {
                    x + y;
                };

                let result = add(five, ten);"
            )
            .collect::<Vec<Token>>(),
            vec![
                token!(TokenType::Let),
                token!(TokenType::Ident, "five"),
                token!(TokenType::Assign),
                token!(TokenType::Int, "5"),
                token!(TokenType::Semicolon),
                token!(TokenType::Let),
                token!(TokenType::Ident, "ten"),
                token!(TokenType::Assign),
                token!(TokenType::Int, "10"),
                token!(TokenType::Semicolon),
                token!(TokenType::Let),
                token!(TokenType::Ident, "add"),
                token!(TokenType::Assign),
                token!(TokenType::Function),
                token!(TokenType::LParen),
                token!(TokenType::Ident, "x"),
                token!(TokenType::Comma),
                token!(TokenType::Ident, "y"),
                token!(TokenType::RParen),
                token!(TokenType::LBrace),
                token!(TokenType::Ident, "x"),
                token!(TokenType::Plus),
                token!(TokenType::Ident, "y"),
                token!(TokenType::Semicolon),
                token!(TokenType::RBrace),
                token!(TokenType::Semicolon),
                token!(TokenType::Let),
                token!(TokenType::Ident, "result"),
                token!(TokenType::Assign),
                token!(TokenType::Ident, "add"),
                token!(TokenType::LParen),
                token!(TokenType::Ident, "five"),
                token!(TokenType::Comma),
                token!(TokenType::Ident, "ten"),
                token!(TokenType::RParen),
                token!(TokenType::Semicolon),
                token!(TokenType::EOF),
            ],
        );

        assert_eq!(
            Lexer::new(
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

                "
            )
            .collect::<Vec<Token>>(),
            vec![
                token!(TokenType::Let),
                token!(TokenType::Ident, "result"),
                token!(TokenType::Assign),
                token!(TokenType::Ident, "add"),
                token!(TokenType::LParen),
                token!(TokenType::Ident, "five"),
                token!(TokenType::Comma),
                token!(TokenType::Ident, "ten"),
                token!(TokenType::RParen),
                token!(TokenType::Semicolon),
                token!(TokenType::Bang),
                token!(TokenType::Minus),
                token!(TokenType::Slash),
                token!(TokenType::Asterisk),
                token!(TokenType::Int, "5"),
                token!(TokenType::Semicolon),
                token!(TokenType::Int, "5"),
                token!(TokenType::LessThan),
                token!(TokenType::Int, "10"),
                token!(TokenType::GreaterThan),
                token!(TokenType::Int, "5"),
                token!(TokenType::Semicolon),
                token!(TokenType::If),
                token!(TokenType::LParen),
                token!(TokenType::Int, "5"),
                token!(TokenType::LessThan),
                token!(TokenType::Int, "10"),
                token!(TokenType::RParen),
                token!(TokenType::LBrace),
                token!(TokenType::Return),
                token!(TokenType::True),
                token!(TokenType::Semicolon),
                token!(TokenType::RBrace),
                token!(TokenType::Else),
                token!(TokenType::LBrace),
                token!(TokenType::Return),
                token!(TokenType::False),
                token!(TokenType::Semicolon),
                token!(TokenType::RBrace),
                token!(TokenType::Int, "10"),
                token!(TokenType::Equals),
                token!(TokenType::Int, "10"),
                token!(TokenType::Semicolon),
                token!(TokenType::Int, "9"),
                token!(TokenType::NotEquals),
                token!(TokenType::Int, "10"),
                token!(TokenType::Semicolon),
                token!(TokenType::EOF),
            ],
        );
    }
}

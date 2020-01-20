use std::{
    collections::HashMap,
    fmt::{Display, Formatter, Result as FmtResult},
    iter::Peekable,
    str::{self, Chars},
};

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
            _ => {
                eprintln!("{:?}", self);
                unreachable!()
            }
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
    pub fn new(name: TokenType) -> Self {
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
                    Some(Token::new(TokenType::Equals))
                } else {
                    Some(Token::new(TokenType::Assign))
                }
            }
            Some('+') => Some(Token::new(TokenType::Plus)),
            Some('*') => Some(Token::new(TokenType::Asterisk)),
            Some('/') => Some(Token::new(TokenType::Slash)),
            Some('-') => Some(Token::new(TokenType::Minus)),
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
                    Some(Token::new(TokenType::Bang))
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
                Some(Token::with_value(TokenType::Int, &number))
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

#[inline]
fn is_letter(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn lookup_ident(ident: &str) -> Token {
    match IDENTMAP.get(&ident) {
        Some(v) => v.clone(),
        None => Token::with_value(TokenType::Ident, ident),
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
                Token::new(TokenType::Let),
                Token::with_value(TokenType::Ident, "five"),
                Token::new(TokenType::Assign),
                Token::with_value(TokenType::Int, "5"),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::Let),
                Token::with_value(TokenType::Ident, "ten"),
                Token::new(TokenType::Assign),
                Token::with_value(TokenType::Int, "10"),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::Let),
                Token::with_value(TokenType::Ident, "add"),
                Token::new(TokenType::Assign),
                Token::new(TokenType::Function),
                Token::new(TokenType::LParen),
                Token::with_value(TokenType::Ident, "x"),
                Token::new(TokenType::Comma),
                Token::with_value(TokenType::Ident, "y"),
                Token::new(TokenType::RParen),
                Token::new(TokenType::LBrace),
                Token::with_value(TokenType::Ident, "x"),
                Token::new(TokenType::Plus),
                Token::with_value(TokenType::Ident, "y"),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::RBrace),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::Let),
                Token::with_value(TokenType::Ident, "result"),
                Token::new(TokenType::Assign),
                Token::with_value(TokenType::Ident, "add"),
                Token::new(TokenType::LParen),
                Token::with_value(TokenType::Ident, "five"),
                Token::new(TokenType::Comma),
                Token::with_value(TokenType::Ident, "ten"),
                Token::new(TokenType::RParen),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::EOF),
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
                Token::new(TokenType::Let),
                Token::with_value(TokenType::Ident, "result"),
                Token::new(TokenType::Assign),
                Token::with_value(TokenType::Ident, "add"),
                Token::new(TokenType::LParen),
                Token::with_value(TokenType::Ident, "five"),
                Token::new(TokenType::Comma),
                Token::with_value(TokenType::Ident, "ten"),
                Token::new(TokenType::RParen),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::Bang),
                Token::new(TokenType::Minus),
                Token::new(TokenType::Slash),
                Token::new(TokenType::Asterisk),
                Token::with_value(TokenType::Int, "5"),
                Token::new(TokenType::Semicolon),
                Token::with_value(TokenType::Int, "5"),
                Token::new(TokenType::LessThan),
                Token::with_value(TokenType::Int, "10"),
                Token::new(TokenType::GreaterThan),
                Token::with_value(TokenType::Int, "5"),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::If),
                Token::new(TokenType::LParen),
                Token::with_value(TokenType::Int, "5"),
                Token::new(TokenType::LessThan),
                Token::with_value(TokenType::Int, "10"),
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
                Token::with_value(TokenType::Int, "10"),
                Token::new(TokenType::Equals),
                Token::with_value(TokenType::Int, "10"),
                Token::new(TokenType::Semicolon),
                Token::with_value(TokenType::Int, "9"),
                Token::new(TokenType::NotEquals),
                Token::with_value(TokenType::Int, "10"),
                Token::new(TokenType::Semicolon),
                Token::new(TokenType::EOF),
            ],
        );
    }
}

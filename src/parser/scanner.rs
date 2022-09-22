use std::{io, result};
use std::iter::Peekable;
use std::ops::Range;
use std::path::Path;
use std::str::Chars;

#[derive(Debug, Clone)]
pub struct Token {
    pub line: u32,
    pub columns: Range<u32>,
    pub kind: TokenKind,
    pub lexeme: String,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    Integer,
    Decimal,
    Dot,
    DoubleDot,
    Ident,
    Comma,
    Semicolon,
    LeftBracket,
    RightBracket,
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    LeftParen,
    RightParen,
    Newline,
    Space,
}

#[derive(Debug, Clone)]
pub struct Error {
    line: u32,
    columns: Range<u32>,
    kind: ErrorKind,
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    UnexpectedChar(char),
}

pub type Result<T> = result::Result<T, Error>;

pub struct Scanner<Iter: Iterator<Item=char> + Clone> {
    chars: Peekable<Iter>,
    column: u32,
    line: u32,
}

impl<Iter: Iterator<Item=char> + Clone> Scanner<Iter> {
    pub fn new(into_iter: impl IntoIterator<IntoIter=Iter>) -> Self {
        Self {
            line: 1,
            column: 0,
            chars: into_iter.into_iter().peekable(),
        }
    }

    fn number(&mut self, c: char) -> Result<Token> {
        let mut col = self.column;
        self.column += 1;
        let mut out = String::from(c);

        while self.matches(|v| v.is_numeric()) {
            let c = self.chars.next().unwrap();
            self.column += 1;
            out.push(c);
        }

        if !self.matches(|v| '.'.eq(v)) || self.chars.clone().nth(1).filter(|v| '.'.eq(&v)).is_some() {
            Ok(Token {
                line: self.line,
                columns: col..self.column,
                kind: TokenKind::Integer,
                lexeme: out,
            })
        } else {
            self.column += 1;
            out.push(self.chars.next().unwrap());
            while self.matches(|v| v.is_numeric()) {
                let c = self.chars.next().unwrap();
                self.column += 1;
                out.push(c);
            }

            Ok(Token {
                line: self.line,
                columns: col..self.column,
                kind: TokenKind::Decimal,
                lexeme: out,
            })
        }
    }

    fn simple(&mut self, str: impl AsRef<str>, kind: TokenKind) -> Result<Token> {
        let str = str.as_ref();
        let col = self.column;
        self.column += str.len() as u32;
        for _ in 1..str.len() {
            self.chars.next();
        }
        Ok(Token {
            line: self.line,
            columns: col..self.column,
            kind,
            lexeme: str.to_string(),
        })
    }

    fn matches(&mut self, f: impl FnOnce(&char) -> bool) -> bool {
        self.chars.peek().filter(|v| f(*v)).is_some()
    }
}

impl<Iter: Iterator<Item=char> + Clone> Iterator for Scanner<Iter> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let top = self.chars.next()?;
        Some(match top {
            '.' => if self.chars.peek().filter(|v| '.'.eq(*v)).is_some() {
                self.simple("..", TokenKind::DoubleDot)
            } else {
                self.simple(".", TokenKind::Dot)
            },
            '\n' => {
                let col = self.column;
                self.line += 1;
                self.column = 0;
                Ok(Token {
                    kind: TokenKind::Newline,
                    columns: col - 1..col,
                    line: self.line - 1,
                    lexeme: "\n".into(),
                })
            }
            v if v.is_whitespace() => {
                let col = self.column;
                let mut out = String::from(v);
                self.column += 1;
                while self.matches(|v| v.is_whitespace() && '\n'.ne(v)) {
                    out.push(self.chars.next().unwrap());
                    self.column += 1;
                }
                Ok(Token {
                    kind: TokenKind::Space,
                    columns: col - 1..self.column,
                    line: self.line,
                    lexeme: out,
                })
            }
            '[' => self.simple("[", TokenKind::LeftBracket),
            ']' => self.simple("]", TokenKind::RightBracket),
            '(' => self.simple("(", TokenKind::LeftParen),
            ')' => self.simple(")", TokenKind::RightParen),
            ';' => self.simple(";", TokenKind::Semicolon),
            ',' => self.simple(",", TokenKind::Comma),
            '=' => self.simple("=", TokenKind::Assign),
            '+' => self.simple("+", TokenKind::Add),
            '-' => self.simple("-", TokenKind::Sub),
            '*' => self.simple("*", TokenKind::Mul),
            '/' => self.simple("/", TokenKind::Div),
            '%' => self.simple("%", TokenKind::Rem),
            c @ '0'..='9' => self.number(c),
            '-' if self.matches(|v| v.is_numeric()) => self.number('-'),
            c @ '_' | c if c.is_alphabetic() => {
                let col = self.column;
                self.column += 1;
                let mut out = String::from(c);

                while self.matches(|v| v.is_alphabetic() || '_'.eq(v)) {
                    self.column += 1;
                    out.push(self.chars.next().unwrap());
                }

                Ok(Token {
                    line: self.line,
                    columns: col..self.column,
                    kind: TokenKind::Ident,
                    lexeme: out,
                })
            }
            c => Err(Error {
                kind: ErrorKind::UnexpectedChar(c),
                line: self.line,
                columns: self.column..self.column + 1,
            })
        })
    }
}
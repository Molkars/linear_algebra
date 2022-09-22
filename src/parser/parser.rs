use std::iter::Peekable;
use std::result;

use crate::parser::TokenKind;

use super::Token;

#[derive(Debug, Clone)]
pub struct Error {
    expr: Option<Expr>,
    token: Option<Token>,
    kind: ErrorKind,
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    EndOfInput,
    UnterminatedCall,
    InvalidPrimary,
    ParseInteger,
    ParseFloat,
}

type Result<T> = result::Result<T, Error>;

pub struct Parser<Iter: Iterator<Item=Token>> {
    tokens: Peekable<Iter>,
}

impl<Iter: Iterator<Item=Token>> Parser<Iter> {
    fn expr(&mut self) -> Result<Expr> {
        self.assign()
    }

    fn assign(&mut self) -> Result<Expr> {
        let mut expr = self.add()?;

        self.consume_whitespace();
        if self.tokens.peek().filter(|v| matches!(&v.kind, TokenKind::Assign)).is_some() {
            self.tokens.next();
            expr = Expr::Assign(Box::new(expr), Box::new(self.add()?));
        }

        Ok(expr)
    }

    fn add(&mut self) -> Result<Expr> {
        let mut expr = self.mul()?;

        loop {
            self.consume_whitespace();
            expr = match self.tokens.peek() {
                Some(Token { kind: TokenKind::Add, .. }) => {
                    self.tokens.next();
                    Expr::Add(Box::new(expr), Box::new(self.mul()?))
                }
                Some(Token { kind: TokenKind::Sub, .. }) => {
                    self.tokens.next();
                    Expr::Sub(Box::new(expr), Box::new(self.mul()?))
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn mul(&mut self) -> Result<Expr> {
        let mut expr = self.invoke()?;

        loop {
            self.consume_whitespace();
            expr = match self.tokens.peek() {
                Some(Token { kind: TokenKind::Mul, .. }) => {
                    self.tokens.next();
                    Expr::Mul(Box::new(expr), Box::new(self.invoke()?))
                }
                Some(Token { kind: TokenKind::Div, .. }) => {
                    self.tokens.next();
                    Expr::Div(Box::new(expr), Box::new(self.invoke()?))
                }
                Some(Token { kind: TokenKind::Rem, .. }) => {
                    self.tokens.next();
                    Expr::Rem(Box::new(expr), Box::new(self.invoke()?))
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn invoke(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;

        loop {
            self.consume_whitespace();
            match self.tokens.peek() {
                Some(Token { kind: TokenKind::LeftParen, .. }) => {
                    self.tokens.next();

                    let mut args = vec![];
                    loop {
                        self.consume_whitespace();
                        if self.tokens.peek().filter(|v| matches!(&v.kind, TokenKind::RightParen)).is_some() {
                            self.tokens.next();
                            break;
                        }

                        args.push(self.expr()?);
                        self.consume_whitespace();
                        let top = match self.tokens.next() {
                            None => return Err(Error {
                                expr: Some(expr),
                                kind: ErrorKind::EndOfInput,
                                token: None,
                            }),
                            Some(v) => v,
                        };
                        if matches!(&top.kind, TokenKind::Comma) {
                            continue;
                        }

                        if matches!(&top.kind, TokenKind::RightParen) {
                            break;
                        }

                        return Err(Error {
                            token: Some(top),
                            expr: Some(expr),
                            kind: ErrorKind::UnterminatedCall,
                        });
                    }

                    expr = Expr::Call(Box::new(expr), args);
                }
                Some(Token { kind: TokenKind::LeftBracket, .. }) => {
                    self.tokens.next();

                    let mut args = vec![];
                    loop {
                        if self.tokens.peek().filter(|v| matches!(&v.kind, TokenKind::RightBracket)).is_some() {
                            self.tokens.next();
                            break;
                        }

                        args.push(self.expr()?);

                        let top = match self.tokens.next() {
                            None => return Err(Error {
                                expr: Some(expr),
                                kind: ErrorKind::EndOfInput,
                                token: None,
                            }),
                            Some(v) => v,
                        };
                        if matches!(&top.kind, TokenKind::Comma) {
                            continue;
                        }

                        if matches!(&top.kind, TokenKind::RightParen) {
                            break;
                        }

                        return Err(Error {
                            token: Some(top),
                            expr: Some(expr),
                            kind: ErrorKind::UnterminatedCall,
                        });
                    }

                    expr = Expr::Index(Box::new(expr), args);
                }
                _ => break
            };
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr> {
        let top = self.tokens.next()
            .ok_or_else(|| Error { kind: ErrorKind::EndOfInput, token: None, expr: None })?;
        Ok(match top {
            token @ Token { kind: TokenKind::Integer, .. } => {
                let val = token.lexeme.parse::<i64>()
                    .map_err(|e| Error { kind: ErrorKind::ParseInteger, token: Some(token), expr: None })?;
                Expr::Integer(val)
            }
            token @ Token { kind: TokenKind::Decimal, .. } => {
                let val = token.lexeme.parse::<f64>()
                    .map_err(|e| Error { kind: ErrorKind::ParseFloat, token: Some(token), expr: None })?;
                Expr::Decimal(val)
            }
            token @ Token { kind: TokenKind::Ident, .. } => Expr::Sym(token),
            token @ Token { kind: TokenKind::LeftBracket, .. } => {
                let mut rows = vec![];
                loop {
                    self.consume_whitespace();
                    loop {
                    }
                }
            }
            token => return Err(Error {
                kind: ErrorKind::InvalidPrimary,
                token: Some(token),
                expr: None,
            })
        })
    }

    fn consume_whitespace(&mut self) {
        fn filter(v: &&Token) -> bool {
            matches!(&v.kind, TokenKind::Newline | TokenKind::Space)
        }

        while self.tokens.peek().filter(filter).is_some() {
            self.tokens.next();
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Assign(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Rem(Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Index(Box<Expr>, Vec<Expr>),
    Integer(i64),
    Decimal(f64),
    Sym(Token),
    Matrix(Vec<Vec<f64>>),
}

impl<Iter: Iterator<Item=Token>> Iterator for Parser<Iter> {
    type Item = Result<Expr>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}
#![allow(unused)]

use std::fmt::{Display, Formatter};
use std::io::{stdin, stdout, Write};
use inquire::Select;
use crate::parser::{Parser, Scanner};

mod choose_file;
mod parser;

fn main() {
    let mut stdin = stdin();
    loop {
        let mut line = String::new();
        {
            let mut out = stdout();
            out.write(b"> ");
            out.flush();
        }
        stdin.read_line(&mut line);

        let items = Scanner::new(line.chars())
            .collect::<Vec<_>>();

        let mut tokens = Vec::new();
        let mut has_err = false;
        for item in items {
            match item {
                Ok(t) => tokens.push(t),
                Err(e) => {
                    has_err = true;
                    eprintln!("{:?}", e);
                }
            }
        }

        if has_err {
            continue;
        }

        let parser = Parser::new(tokens);
        let items = parser.collect::<Vec<_>>();
        let mut exprs = Vec::new();
        let mut has_err = false;
        for item in items {
            match item {
                Ok(expr) => exprs.push(expr),
                Err(e) => {
                    has_err = true;
                    eprintln!("{:?}", e);
                }
            }
        }
        if has_err {
            continue;
        }
        for expr in &exprs {
            println!("{:?}", expr);
        }
    }
}

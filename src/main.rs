#![allow(unused)]

use std::fmt::{Display, Formatter};
use std::io::stdin;
use inquire::Select;
use crate::parser::Scanner;

mod choose_file;
mod parser;

fn main() {
    let mut stdin = stdin();
    loop {
        let mut line = String::new();
        stdin.read_line(&mut line);

        let items = Scanner::new(line.chars())
            .collect::<Vec<_>>();

        for item in items {
            if let Err(e) = item {
                println!("{:?}", e);
            } else {
                println!("{:?}", item.unwrap());
            }
        }
    }
}

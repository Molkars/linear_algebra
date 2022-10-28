#![allow(unused_imports)]

extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::hash::Hash;
use std::io::{stdin, stdout, Write};
use std::rc::Rc;

use pest::iterators::Pair;
use crate::evaluate::evaluate;

use crate::expr::{Decl, Expr, Matrix, Ty};
use crate::expr::convert::convert_decl;
use crate::parser::Rule;
use crate::pest::Parser;
use crate::resolve::resolve;

mod expr;
mod resolve;
mod evaluate;

mod parser {
    #[derive(Parser)]
    #[grammar = "matrix.pest"]
    pub struct MatrixParser;
}

fn main() {
    // let contents = "[1 + 4 + 3, 1 * (4 + 3), 1; 1, 0, 1; 2, 1, 2; 0, 1, 0 + 3]";
    let contents = fs::read_to_string("file.math").unwrap();

    let parser = parser::MatrixParser::parse(parser::Rule::Program, &contents)
        .unwrap();

    let mut scope = BasicScope::default();
    let mut instructions = Vec::new();
    for decl in parser {
        if matches!(decl.as_rule(), Rule::EOI) {
            break;
        }
        let decl = convert_decl(decl);
        println!("{}", decl);
        match decl {
            Decl::Func(name, args, ret, body) => {
                scope.functions.entry(name)
                    .or_insert_with(Vec::new)
                    .push((args, ret, body));
            }
            Decl::Expr(expr) => instructions.push(expr),
        };
    }

    let scope_name = "crate".to_string();
    let mut stack = BasicStack {
        stack: vec![(scope_name.clone(), vec![])],
        scopes: HashMap::from([(scope_name, scope)]),
    };
    for instruction in &instructions {
        resolve(&instruction, &mut stack);
    }
    for instruction in &instructions {
        evaluate(&instruction);
    }
}

#[derive(Debug, Clone, Default)]
pub struct BasicStack {
    stack: Vec<(String, Vec<BasicScope>)>,
    scopes: HashMap<String, BasicScope>,
}

#[derive(Debug, Clone, Default)]
pub struct BasicScope {
    // imports: HashSet<String>,
    functions: HashMap<String, Vec<(Vec<(String, Ty)>, Ty, Expr)>>,
}
extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::fs;

use pest::iterators::Pair;

use crate::expr::{Expr, Matrix, Ty};
use crate::parser::Rule;
use crate::pest::Parser;

mod expr;

mod parser {
    #[derive(Parser)]
    #[grammar = "matrix.pest"]
    pub struct MatrixParser;
}

fn main() {
    // let contents = "[1 + 4 + 3, 1 * (4 + 3), 1; 1, 0, 1; 2, 1, 2; 0, 1, 0 + 3]";
    let contents = fs::read_to_string("file.math").unwrap();
    println!("{}", contents);

    let expr = parser::MatrixParser::parse(parser::Rule::Program, &contents)
        .unwrap()
        .next().unwrap();

    println!("{:#?}\n\n", expr);

    // let mut mat = Vec::<Vec<Expr>>::new();
    // for row in matrix.into_inner() {
    //     let mut new_row = vec![];
    //     for elem in row.into_inner() {
    //         // println!("Col: {:#?}", elem);
    //         let element = convert(elem);
    //         println!("{:?}", element);
    //         new_row.push(element);
    //     }
    //     mat.push(new_row);
    // }
    let expr = convert(expr);
    println!("{:?}", expr);
}

fn convert(pair: Pair<Rule>) -> Expr {
    match pair.as_rule() {
        Rule::Func => {
            let mut pairs = pair.into_inner();
            let name = pairs.next().unwrap().as_str().to_string();
            let mut args = vec![];
            while pairs.peek().filter(|v| matches!(v.as_rule(), Rule::Arg)).is_some() {
                let rule = pairs.next().unwrap();
                let mut pairs = rule.into_inner();
                let name = pairs.next().unwrap().as_str().to_string();
                let ty = convert(pairs.next().unwrap()).into_ty();
                args.push((name, ty));
            }
            let ty = convert(pairs.next().unwrap()).into_ty();
            let body = convert(pairs.next().unwrap());
            Expr::Func(name, args, ty, Box::new(body))
        }
        Rule::Block => {
            let mut out = vec![];

            for expr in pair.into_inner() {
                out.push(convert(expr));
            }

            Expr::Block(out)
        }
        Rule::Expr => {
            let mut pairs = pair.into_inner();
            convert(pairs.next().unwrap())
        }
        Rule::Assign => {
            let mut pairs = pair.into_inner();
            let mut expr = convert(pairs.next().unwrap());
            while let Some(op) = pairs.next() {
                let right = convert(pairs.next().unwrap());
                expr = match op.as_str() {
                    "=" => Expr::Set(Box::new(expr), Box::new(right)),
                    v => unreachable!("Unknown assign op: {}", v),
                };
            }
            expr
        }
        Rule::Or => {
            let mut pairs = pair.into_inner();
            let mut expr = convert(pairs.next().unwrap());
            while let Some(op) = pairs.next() {
                let right = convert(pairs.next().unwrap());
                expr = match op.as_str() {
                    "|" => Expr::Or(Box::new(expr), Box::new(right)),
                    v => unreachable!("Unknown or op: {}", v),
                };
            }
            expr
        }
        Rule::And => {
            let mut pairs = pair.into_inner();
            let mut expr = convert(pairs.next().unwrap());
            while let Some(op) = pairs.next() {
                let right = convert(pairs.next().unwrap());
                expr = match op.as_str() {
                    "&" => Expr::And(Box::new(expr), Box::new(right)),
                    v => unreachable!("Unknown and op: {}", v),
                };
            }
            expr
        }
        Rule::Equality => {
            let mut pairs = pair.into_inner();
            let mut expr = convert(pairs.next().unwrap());
            while let Some(op) = pairs.next() {
                let right = convert(pairs.next().unwrap());
                expr = match op.as_str() {
                    "==" => Expr::Eq(Box::new(expr), Box::new(right)),
                    "!=" => Expr::Ne(Box::new(expr), Box::new(right)),
                    v => unreachable!("Unknown eq op: {}", v),
                };
            }
            expr
        }
        Rule::Comparative => {
            let mut pairs = pair.into_inner();
            let mut expr = convert(pairs.next().unwrap());
            while let Some(op) = pairs.next() {
                let right = convert(pairs.next().unwrap());
                expr = match op.as_str() {
                    ">=" => Expr::Ge(Box::new(expr), Box::new(right)),
                    ">" => Expr::Gt(Box::new(expr), Box::new(right)),
                    "<=" => Expr::Le(Box::new(expr), Box::new(right)),
                    "<" => Expr::Lt(Box::new(expr), Box::new(right)),
                    v => unreachable!("Unknown cmp op: {}", v),
                };
            }
            expr
        }
        Rule::Additive => {
            let mut pairs = pair.into_inner();
            let mut expr = convert(pairs.next().unwrap());
            while let Some(op) = pairs.next() {
                let right = convert(pairs.next().unwrap());
                expr = match op.as_str() {
                    "+" => Expr::Add(Box::new(expr), Box::new(right)),
                    "-" => Expr::Sub(Box::new(expr), Box::new(right)),
                    v => unreachable!("Unknown add op: {}", v),
                };
            }
            expr
        }
        Rule::Multiplicative => {
            let mut pairs = pair.into_inner();
            let mut expr = convert(pairs.next().unwrap());
            while let Some(op) = pairs.next() {
                let right = convert(pairs.next().unwrap());
                expr = match op.as_str() {
                    "*" => Expr::Mul(Box::new(expr), Box::new(right)),
                    "/" => Expr::Div(Box::new(expr), Box::new(right)),
                    "%" => Expr::Rem(Box::new(expr), Box::new(right)),
                    v => unreachable!("Unknown mul op: {}", v),
                };
            }
            expr
        }
        Rule::Exponent => {
            let mut pairs = pair.into_inner();
            let mut expr = convert(pairs.next().unwrap());
            while let Some(op) = pairs.next() {
                let right = convert(pairs.next().unwrap());
                expr = match op.as_str() {
                    "^" => Expr::Pow(Box::new(expr), Box::new(right)),
                    v => unreachable!("Unknown exp op: {}", v),
                };
            }
            expr
        }
        Rule::Term => {
            convert(pair.into_inner().next().unwrap())
        }
        Rule::Matrix => {
            let mut out = Vec::new();
            for row in pair.into_inner() {
                let mut crow = vec![];
                for elem in row.into_inner() {
                    crow.push(convert(elem));
                }
                out.push(crow);
            }

            Expr::Mat(Matrix::try_from(out).unwrap())
        }
        Rule::Int => Expr::Int(pair.as_str().parse().unwrap()),
        Rule::Dec => Expr::Dec(pair.as_str().parse().unwrap()),
        Rule::Hex => Expr::Int(isize::from_str_radix(pair.as_str(), 16).unwrap()),
        Rule::Ty => {
            let mut pairs = pair.into_inner();
            let next = pairs.next().unwrap();
            if matches!(next.as_rule(), Rule::Ident) {
                match next.as_str() {
                    "int" => Expr::Ty(Ty::Int),
                    "dec" => Expr::Ty(Ty::Dec),
                    "num" => Expr::Ty(Ty::Num),
                    t => panic!("Unknown type: {:?}", t),
                }
            } else {
                convert(next)
            }
        },
        Rule::ArrTy => {
            let mut pairs = pair.into_inner();
            let ty = convert(pairs.next().unwrap()).into_ty();
            let dim = const_eval(&convert(pairs.next().unwrap()));
            Expr::Ty(Ty::Array(Box::new(ty), dim.into_uint()))
        }
        Rule::MatrixTy => {
            let mut pairs = pair.into_inner();
            let ty = convert(pairs.next().unwrap()).into_ty();
            let dim_y = const_eval(&convert(pairs.next().unwrap())).into_uint();
            let dim_x = const_eval(&convert(pairs.next().unwrap())).into_uint();
            assert!(dim_x > 0 && dim_y > 0, "Matrix must be at least [1x1]");
            Expr::Ty(Ty::Mat(Box::new(ty), dim_x, dim_y))
        }
        Rule::Ident => Expr::Ident(pair.as_str().to_string()),
        v => unreachable!("{:?}", v),
    }
}

fn const_eval(expr: &Expr) -> Value {
    match expr {
        Expr::Int(v) => Value::Int(*v),
        Expr::Uint(v) => Value::Uint(*v),
        Expr::Add(l, r) => match (const_eval(l.as_ref()), const_eval(r.as_ref())) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l + r),
            (Value::Uint(l), Value::Uint(r)) => Value::Uint(l + r),
            (a, b) => unimplemented!("Add: {a:?} + {b:?}"),
        }
        Expr::Sub(l, r) => match (const_eval(l.as_ref()), const_eval(r.as_ref())) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l - r),
            (Value::Uint(l), Value::Uint(r)) => Value::Uint(l - r),
            (a, b) => unimplemented!("Sub: {a:?} - {b:?}"),
        }
        Expr::Mul(l, r) => match (const_eval(l.as_ref()), const_eval(r.as_ref())) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l * r),
            (Value::Uint(l), Value::Uint(r)) => Value::Uint(l * r),
            (a, b) => unimplemented!("Mul: {a:?} * {b:?}"),
        }
        Expr::Div(l, r) => match (const_eval(l.as_ref()), const_eval(r.as_ref())) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l / r),
            (Value::Uint(l), Value::Uint(r)) => Value::Uint(l / r),
            (a, b) => unimplemented!("Div: {a:?} / {b:?}"),
        }
        Expr::Rem(l, r) => match (const_eval(l.as_ref()), const_eval(r.as_ref())) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l % r),
            (Value::Uint(l), Value::Uint(r)) => Value::Uint(l % r),
            (a, b) => unimplemented!("Rem: {a:?} % {b:?}"),
        }
        Expr::Pow(l, r) => match (const_eval(l.as_ref()), const_eval(r.as_ref())) {
            (Value::Int(l), Value::Int(r)) => Value::Int(l.pow(r as _)),
            (Value::Uint(l), Value::Uint(r)) => Value::Uint(l.pow(r as _)),
            (a, b) => unimplemented!("Pow: {a:?} ^ {b:?}"),
        }
        expr => unimplemented!("const_eval: {:?}", expr),
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(isize),
    Uint(usize),
}

impl Value {
    pub fn into_int(self) -> isize {
        match self {
            Value::Int(v) => v,
            Value::Uint(_) => panic!("Value::into_int called on variant other than Value::Int"),
        }
    }

    pub fn into_uint(self) -> usize {
        match self {
            Value::Uint(v) => v,
            Value::Int(_) => panic!("Value::into_uint called on variant other than Value::Uint"),
        }
    }
}
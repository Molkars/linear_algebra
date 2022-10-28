use pest::iterators::Pair;

use crate::{Decl, Expr, Matrix, Ty};
use crate::parser::Rule;

pub fn convert_decl(pair: Pair<Rule>) -> Decl {
    match pair.as_rule() {
        Rule::Func => {
            let mut pairs = pair.into_inner();
            let name = pairs.next().unwrap().as_str().to_string();
            let mut args = vec![];
            while pairs.peek().filter(|v| matches!(v.as_rule(), Rule::Arg)).is_some() {
                let mut pairs = pairs.next().unwrap().into_inner();
                let name = pairs.next().unwrap().as_str().to_string();
                let ty = convert_type(pairs.next().unwrap());
                args.push((name, ty));
            }
            let ty = convert_type(pairs.next().unwrap());
            let body = convert_expr(pairs.next().unwrap());
            Decl::Func(name, args, ty, body)
        }
        Rule::Expr => Decl::Expr(convert_expr(pair)),
        v => unreachable!("convert_decl: {:?}", v),
    }
}

pub fn convert_expr(pair: Pair<Rule>) -> Expr {
    match pair.as_rule() {
        Rule::Block => {
            let mut out = vec![];

            for expr in pair.into_inner() {
                out.push(convert_expr(expr));
            }

            Expr::Block(out)
        }
        Rule::Expr => {
            let mut pairs = pair.into_inner();
            convert_expr(pairs.next().unwrap())
        }
        Rule::Assign => {
            let mut pairs = pair.into_inner();
            let mut expr = convert_expr(pairs.next().unwrap());
            while let Some(op) = pairs.next() {
                let right = convert_expr(pairs.next().unwrap());
                expr = match op.as_str() {
                    "=" => Expr::Set(Box::new(expr), Box::new(right)),
                    v => unreachable!("Unknown assign op: {}", v),
                };
            }
            expr
        }
        Rule::Or => {
            let mut pairs = pair.into_inner();
            let mut expr = convert_expr(pairs.next().unwrap());
            while let Some(op) = pairs.next() {
                let right = convert_expr(pairs.next().unwrap());
                expr = match op.as_str() {
                    "|" => Expr::Or(Box::new(expr), Box::new(right)),
                    v => unreachable!("Unknown or op: {}", v),
                };
            }
            expr
        }
        Rule::And => {
            let mut pairs = pair.into_inner();
            let mut expr = convert_expr(pairs.next().unwrap());
            while let Some(op) = pairs.next() {
                let right = convert_expr(pairs.next().unwrap());
                expr = match op.as_str() {
                    "&" => Expr::And(Box::new(expr), Box::new(right)),
                    v => unreachable!("Unknown and op: {}", v),
                };
            }
            expr
        }
        Rule::Equality => {
            let mut pairs = pair.into_inner();
            let mut expr = convert_expr(pairs.next().unwrap());
            while let Some(op) = pairs.next() {
                let right = convert_expr(pairs.next().unwrap());
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
            let mut expr = convert_expr(pairs.next().unwrap());
            while let Some(op) = pairs.next() {
                let right = convert_expr(pairs.next().unwrap());
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
            let mut expr = convert_expr(pairs.next().unwrap());
            while let Some(op) = pairs.next() {
                let right = convert_expr(pairs.next().unwrap());
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
            let mut expr = convert_expr(pairs.next().unwrap());
            while let Some(op) = pairs.next() {
                let right = convert_expr(pairs.next().unwrap());
                expr = match op.as_str() {
                    "*" => Expr::Mul(Box::new(expr), Box::new(right)),
                    "/" => Expr::Div(Box::new(expr), Box::new(right)),
                    "%" => Expr::Rem(Box::new(expr), Box::new(right)),
                    v => unreachable!("Unknown mul op: {}", v),
                };
            }
            expr
        }
        Rule::Unary => {
            enum Op { Pos, Neg, Not }
            let mut ops = vec![];
            let mut pairs = pair.into_inner();
            while pairs.peek().filter(|v| matches!(v.as_rule(), Rule::UnaryOp)).is_some() {
                ops.push(match pairs.next().unwrap().as_str() {
                    "+" => Op::Pos,
                    "-" => Op::Neg,
                    "!" => Op::Not,
                    v => unreachable!("Unknown unary op: {v}"),
                })
            }
            let mut expr = convert_expr(pairs.next().unwrap());
            for op in ops {
                expr = match op {
                    Op::Pos => Expr::Pos(Box::new(expr)),
                    Op::Neg => Expr::Neg(Box::new(expr)),
                    Op::Not => Expr::Not(Box::new(expr)),
                };
            }
            expr
        }
        Rule::Exponential => {
            let mut pairs = pair.into_inner();
            let mut expr = convert_expr(pairs.next().unwrap());
            while let Some(op) = pairs.next() {
                let right = convert_expr(pairs.next().unwrap());
                expr = match op.as_str() {
                    "^" => Expr::Pow(Box::new(expr), Box::new(right)),
                    v => unreachable!("Unknown exp op: {}", v),
                };
            }
            expr
        }
        Rule::Invoke => {
            let mut pairs = pair.into_inner();
            let expr = convert_expr(pairs.next().unwrap());
            if let Some(args) = pairs.next() {
                let args = args
                    .into_inner()
                    .map(|pair| convert_expr(pair))
                    .collect::<Vec<_>>();
                Expr::Call(Box::new(expr), args)
            } else {
                expr
            }
        }
        Rule::Term => convert_expr(pair.into_inner().next().unwrap()),
        Rule::Matrix => {
            let mut out = Vec::new();
            for row in pair.into_inner() {
                let mut crow = vec![];
                for elem in row.into_inner() {
                    crow.push(convert_expr(elem));
                }
                out.push(crow);
            }

            Expr::Mat(Matrix::try_from(out).unwrap())
        }
        Rule::Unit => Expr::Unit,
        Rule::Uint => Expr::Int(pair.as_str().parse().unwrap()),
        Rule::Bool => Expr::Bool(pair.as_str() == "true"),
        Rule::Dec => Expr::Dec(pair.as_str().parse().unwrap()),
        Rule::Hex => Expr::Int(u64::from_str_radix(pair.as_str(), 16).unwrap()),
        Rule::Ident => Expr::Ident(pair.as_str().to_string()),
        Rule::Group => Expr::Group(Box::new(convert_expr(pair.into_inner().next().unwrap()))),
        v => unreachable!("{:?}", v),
    }
}

pub fn convert_type(pair: Pair<Rule>) -> Ty {
    match pair.as_rule() {
        Rule::Ty => convert_type(pair.into_inner().next().unwrap()),
        Rule::Ident => match pair.as_str() {
            "int" => Ty::Int,
            "dec" => Ty::Dec,
            "num" => Ty::Num,
            "bool" => Ty::Bool,
            t => panic!("Unknown type: {:?}", t),
        }
        Rule::Unit => Ty::Unit,
        Rule::ArrTy => {
            let mut pairs = pair.into_inner();
            let ty = convert_type(pairs.next().unwrap());
            let dim = const_eval(&convert_expr(pairs.next().unwrap()));
            Ty::Array(Box::new(ty), dim.into_uint())
        }
        Rule::MatrixTy => {
            let mut pairs = pair.into_inner();
            let ty = convert_type(pairs.next().unwrap());
            let dim_y = const_eval(&convert_expr(pairs.next().unwrap())).into_uint();
            let dim_x = const_eval(&convert_expr(pairs.next().unwrap())).into_uint();
            assert!(dim_x > 0 && dim_y > 0, "Matrix must be at least [1x1]");
            Ty::Mat(Box::new(ty), dim_y, dim_x)
        }
        v => unreachable!("convert_type: {:?}", v),
    }
}

pub fn const_eval(expr: &Expr) -> ConstValue {
    println!("const_eval: {:?}", expr);
    match expr {
        Expr::Int(v) => ConstValue::Uint(*v),
        Expr::Add(l, r) => match (const_eval(l.as_ref()), const_eval(r.as_ref())) {
            (ConstValue::Int(l), ConstValue::Int(r)) => ConstValue::Int(l + r),
            (ConstValue::Uint(l), ConstValue::Uint(r)) => ConstValue::Uint(l + r),
            (a, b) => unimplemented!("Add: {a:?} + {b:?}"),
        }
        Expr::Sub(l, r) => match (const_eval(l.as_ref()), const_eval(r.as_ref())) {
            (ConstValue::Int(l), ConstValue::Int(r)) => ConstValue::Int(l - r),
            (ConstValue::Uint(l), ConstValue::Uint(r)) => ConstValue::Uint(l - r),
            (a, b) => unimplemented!("Sub: {a:?} - {b:?}"),
        }
        Expr::Mul(l, r) => match (const_eval(l.as_ref()), const_eval(r.as_ref())) {
            (ConstValue::Int(l), ConstValue::Int(r)) => ConstValue::Int(l * r),
            (ConstValue::Uint(l), ConstValue::Uint(r)) => ConstValue::Uint(l * r),
            (a, b) => unimplemented!("Mul: {a:?} * {b:?}"),
        }
        Expr::Div(l, r) => match (const_eval(l.as_ref()), const_eval(r.as_ref())) {
            (ConstValue::Int(l), ConstValue::Int(r)) => ConstValue::Int(l / r),
            (ConstValue::Uint(l), ConstValue::Uint(r)) => ConstValue::Uint(l / r),
            (a, b) => unimplemented!("Div: {a:?} / {b:?}"),
        }
        Expr::Rem(l, r) => match (const_eval(l.as_ref()), const_eval(r.as_ref())) {
            (ConstValue::Int(l), ConstValue::Int(r)) => ConstValue::Int(l % r),
            (ConstValue::Uint(l), ConstValue::Uint(r)) => ConstValue::Uint(l % r),
            (a, b) => unimplemented!("Rem: {a:?} % {b:?}"),
        }
        Expr::Pow(l, r) => match (const_eval(l.as_ref()), const_eval(r.as_ref())) {
            (ConstValue::Int(l), ConstValue::Int(r)) => ConstValue::Int(l.pow(r as _)),
            (ConstValue::Uint(l), ConstValue::Uint(r)) => ConstValue::Uint(l.pow(r as _)),
            (a, b) => unimplemented!("Pow: {a:?} ^ {b:?}"),
        }
        expr => unimplemented!("const_eval: {:?}", expr),
    }
}

#[derive(Debug, Clone)]
pub enum ConstValue {
    Int(i64),
    Uint(u64),
}

impl ConstValue {
    pub fn into_int(self) -> i64 {
        match self {
            ConstValue::Int(v) => v,
            ConstValue::Uint(_) => panic!("Value::into_int called on variant other than Value::Int"),
        }
    }

    pub fn into_uint(self) -> u64 {
        match self {
            ConstValue::Uint(v) => v,
            ConstValue::Int(_) => panic!("Value::into_uint called on variant other than Value::Uint"),
        }
    }
}
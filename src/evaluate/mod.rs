use std::collections::HashMap;
use crate::Expr;

mod value;

use value::Value;

pub struct Stack {
    scopes: Vec<(String, Vec<Scope>)>,
}

pub struct Scope {
    variables: HashMap<String, Option<Value>>,
}

pub fn evaluate(instruction: &Expr) {

}
use std::collections::HashMap;

use crate::{BasicStack, Expr, expr::ExprVisitor, Matrix, Ty};

pub fn resolve(expr: &Expr, stack: &mut BasicStack) {
    let mut trace_stack = Stack::default();
    trace_stack.global_scopes.insert("self".to_string(), Scope::default());
    trace_stack.stack.push(("self".to_string(), vec![]));
    Resolver { stack, trace_stack }.visit_expr(expr);
}

struct Resolver<'a> {
    stack: &'a mut BasicStack,
    trace_stack: Stack,
}

#[derive(Debug, Clone, Default)]
pub struct Stack {
    stack: Vec<(String, Vec<Scope>)>,
    global_scopes: HashMap<String, Scope>,
}

#[derive(Debug, Clone, Default)]
pub struct Scope {
    variables: HashMap<String, Option<Ty>>,
    ret: Option<Ty>,
}

impl<'a> Resolver<'a> {
    fn push(&mut self) {
        self.trace_stack.stack.last_mut().expect("push: empty stack")
            .1.push(Scope::default());
    }

    fn pop(&mut self) {
        let (_, stack) = self.trace_stack.stack.last_mut()
            .expect("pop: empty_stack");
        stack.pop().expect("Non popped stack");
    }
}

impl<'a> ExprVisitor for Resolver<'a> {
    type Output = Ty;

    fn visit_default(&mut self) -> Self::Output {
        unreachable!()
    }

    fn visit_block_expr(&mut self, expressions: &[Expr]) -> Self::Output {
        let mut out = None;
        self.push();
        for expr in expressions {
            out = Some(self.visit_expr(expr));
        }
        self.pop();
        out.unwrap_or(Ty::Unit)
    }

    fn visit_set_expr(&mut self, left: &Expr, right: &Expr) -> Self::Output {
        let ty = SetVar { stack: &mut self.trace_stack }.visit_expr(left);
        if let Ty::Ref(inner) = ty {
            let right = self.visit_expr(right);
            match (inner.as_ref(), &right) {
                (Some(a), b) if a.as_ref() == b => Ty::Unit,
                (None, _) => Ty::Unit,
                (l, r) => panic!("Cannot assign {} to {}", r, l.unwrap()),
            }
        } else {
            panic!("Cannot assign to non-ref type:\n  {left} = {right}");
        }
    }

    fn visit_or_expr(&mut self, left: &Expr, right: &Expr) -> Self::Output {
        let lty = self.visit_expr(left);
        let rty = self.visit_expr(right);
        match (lty, rty) {
            (Ty::Bool, Ty::Bool) => Ty::Bool,
            (l, r) => panic!("or: cannot compare {} and {}:\n  {left} || {right}", l, r),
        }
    }

    fn visit_and_expr(&mut self, left: &Expr, right: &Expr) -> Self::Output {
        let lty = self.visit_expr(left);
        let rty = self.visit_expr(right);
        match (lty, rty) {
            (Ty::Bool, Ty::Bool) => Ty::Bool,
            (l, r) => panic!("or: cannot compare {} and {}:\n  {left} && {right}", l, r),
        }
    }

    fn visit_eq_expr(&mut self, left: &Expr, right: &Expr) -> Self::Output {
        let lty = self.visit_expr(left);
        let rty = self.visit_expr(right);
        lty.cmp(&rty)
            .unwrap_or_else(|| panic!("Cannot compare {lty} and {rty}:\n  {left} == {right}"))
    }

    fn visit_ne_expr(&mut self, left: &Expr, right: &Expr) -> Self::Output {
        let lty = self.visit_expr(left);
        let rty = self.visit_expr(right);
        lty.cmp(&rty)
            .unwrap_or_else(|| panic!("Cannot compare {lty} and {rty}:\n  {left} != {right}"))
    }

    fn visit_le_expr(&mut self, left: &Expr, right: &Expr) -> Self::Output {
        let lty = self.visit_expr(left);
        let rty = self.visit_expr(right);
        lty.ord(&rty)
            .unwrap_or_else(|| panic!("Cannot compare {lty} and {rty}:\n  {left} <= {right}"))
    }

    fn visit_lt_expr(&mut self, left: &Expr, right: &Expr) -> Self::Output {
        let lty = self.visit_expr(left);
        let rty = self.visit_expr(right);
        lty.ord(&rty)
            .unwrap_or_else(|| panic!("Cannot compare {lty} and {rty}:\n  {left} < {right}"))
    }

    fn visit_ge_expr(&mut self, left: &Expr, right: &Expr) -> Self::Output {
        let lty = self.visit_expr(left);
        let rty = self.visit_expr(right);
        lty.ord(&rty)
            .unwrap_or_else(|| panic!("Cannot compare {lty} and {rty}:\n  {left} >= {right}"))
    }

    fn visit_gt_expr(&mut self, left: &Expr, right: &Expr) -> Self::Output {
        let lty = self.visit_expr(left);
        let rty = self.visit_expr(right);
        lty.ord(&rty)
            .unwrap_or_else(|| panic!("Cannot compare {lty} and {rty}:\n  {left} > {right}"))
    }

    fn visit_add_expr(&mut self, left: &Expr, right: &Expr) -> Self::Output {
        let lty = self.visit_expr(left);
        let rty = self.visit_expr(right);
        lty.add(&rty)
            .unwrap_or_else(|| panic!("Cannot take add of {lty} and {rty}:\n  {left} + {right}"))
    }

    fn visit_sub_expr(&mut self, left: &Expr, right: &Expr) -> Self::Output {
        let lty = self.visit_expr(left);
        let rty = self.visit_expr(right);
        lty.sub(&rty)
            .unwrap_or_else(|| panic!("Cannot take sub of {lty} and {rty}:\n  {left} - {right}"))
    }

    fn visit_mul_expr(&mut self, left: &Expr, right: &Expr) -> Self::Output {
        let lty = self.visit_expr(left);
        let rty = self.visit_expr(right);
        lty.mul(&rty)
            .unwrap_or_else(|| panic!("Cannot take mul of {lty} and {rty}:\n  {left} * {right}"))
    }

    fn visit_div_expr(&mut self, left: &Expr, right: &Expr) -> Self::Output {
        let lty = self.visit_expr(left);
        let rty = self.visit_expr(right);
        lty.div(&rty)
            .unwrap_or_else(|| panic!("Cannot take div of {lty} and {rty}:\n  {left} / {right}"))
    }

    fn visit_rem_expr(&mut self, left: &Expr, right: &Expr) -> Self::Output {
        let lty = self.visit_expr(left);
        let rty = self.visit_expr(right);
        lty.rem(&rty)
            .unwrap_or_else(|| panic!("Cannot take rem of {lty} and {rty}:\n  {left} % {right}"))
    }

    fn visit_pow_expr(&mut self, left: &Expr, right: &Expr) -> Self::Output {
        let lty = self.visit_expr(left);
        let rty = self.visit_expr(right);
        lty.pow(&rty)
            .unwrap_or_else(|| panic!("Cannot take pow of {lty} and {rty}:\n  {left} ^ {right}"))
    }

    fn visit_pos_expr(&mut self, expr: &Expr) -> Self::Output {
        let ty = self.visit_expr(expr);
        ty.pos().unwrap_or_else(|| panic!("Cannot take the positive value of {ty}:\n  +{expr}"))
    }

    fn visit_neg_expr(&mut self, expr: &Expr) -> Self::Output {
        let ty = self.visit_expr(expr);
        ty.neg().unwrap_or_else(|| panic!("Cannot take the negative value of {ty}:\n  -{expr}"))
    }

    fn visit_not_expr(&mut self, expr: &Expr) -> Self::Output {
        let ty = self.visit_expr(expr);
        ty.not().unwrap_or_else(|| panic!("Cannot take inverse value of {ty}:\n  !{expr}"))
    }

    fn visit_call_expr(&mut self, functor: &Expr, args: &[Expr]) -> Self::Output {
        let args = args.iter().map(|arg| self.visit_expr(arg))
            .collect::<Vec<_>>();
        let ret = LookupFunction { stack: self.stack, args: args.as_slice() }
            .visit_expr(functor);
        ret.clone()
    }

    fn visit_index_expr(&mut self, expr: &Expr, indices: &[Expr]) -> Self::Output {
        let ty = self.visit_expr(expr);
        let indexes = indices.iter()
            .map(|expr| self.visit_expr(expr))
            .collect::<Vec<_>>();
        ty
            .index(indexes.as_slice())
            .unwrap_or_else(|| {
                panic!(
                    "Cannot index {ty} with {indexes:?}:\n  {expr}[{}]",
                    indices.iter().map(|index| format!("{}", index))
                        .collect::<Vec<_>>().join(", ")
                )
            })
    }

    fn visit_bool_expr(&mut self, _: bool) -> Self::Output {
        Ty::Bool
    }

    fn visit_int_expr(&mut self, _: u64) -> Self::Output {
        Ty::Int
    }

    fn visit_dec_expr(&mut self, _: f64) -> Self::Output {
        Ty::Dec
    }

    fn visit_unit_expr(&mut self) -> Self::Output {
        Ty::Unit
    }

    fn visit_mat_expr(&mut self, value: &Matrix) -> Self::Output {
        let mut out: Option<Ty> = None;
        for row in &value.0 {
            for expr in row {
                let ty = self.visit_expr(expr);
                out = match out {
                    Some(current) => current.best(&ty),
                    None => Some(ty),
                }
            }
        }
        let height = value.0.len();
        let width = value.0[0].len();
        Ty::Mat(Box::new(out.unwrap()), height as _, width as _)
    }

    fn visit_ident_expr(&mut self, value: &String) -> Self::Output {
        let (_, stack) = self.trace_stack.stack.last_mut().expect("Stack was empty");
        let ty = stack.iter()
            .find_map(|scope| scope.variables.get(value))
            .unwrap_or_else(|| {
                panic!("Unable to find variable {value} in scope!");
            })
            .unwrap_or_else(|| {
                panic!("Unable to use uninitialized variable!");
            });
        Ty::Ref(Box::new(ty))
    }

    fn visit_if_expr(&mut self, cond: &Expr, body: &Expr, elif: &[(Expr, Expr)], else_: Option<&Expr>) -> Self::Output {
        self.visit_expr(cond).filter(|ty| matches!(ty, Ty::Bool))
            .expect("If argument was not boolean");
        let mut ty = self.visit_expr(body);

        for (cond, branch) in elif {
            self.visit_expr(cond).filter(|ty| matches!(ty, Ty::Bool))
                .unwrap_or_else(|| panic!("else-if argument was not boolean: {cond}"));
            let body = self.visit_expr(branch);
            ty = ty.best(&body)
                .unwrap_or_else(|| {
                    panic!("if and else-if branch return values did not match:\n  `{body}`\n  and\n  `{branch}`")
                });
        }

        if let Some(other) = else_ {
            let body = self.visit_expr(other);
            ty = ty.best(&body)
                .expect("if and else branch return values did not match");
        }

        ty
    }

    fn visit_group_expr(&mut self, expr: &Expr) -> Self::Output {
        self.visit_expr(expr)
    }
}

pub struct LookupFunction<'a> {
    args: &'a [Ty],
    stack: &'a BasicStack,
}

impl<'a> ExprVisitor for LookupFunction<'a> {
    type Output = Ty;

    fn visit_default(&mut self) -> Self::Output {
        unreachable!()
    }

    fn visit_ident_expr(&mut self, value: &String) -> Self::Output {
        let (module, stack) = self.stack.stack.last().expect("Empty stack");

        fn filter(items: &Vec<(Vec<(String, Ty)>, Ty, Expr)>, args: &[Ty]) -> Option<Ty> {
            items.iter().find_map(|(params, ret, body)| {
                if params.len() != args.len() {
                    return None;
                }

                for ((_, param), arg) in params.iter().zip(args) {
                    if !arg.can_convert_into(param) {
                        return None;
                    }
                }

                Some(ret.clone())
            })
        }

        stack.iter()
            .find_map(|scope| {
                let functions = scope.functions.get(value)?;
                filter(functions, self.args)
            })
            .or_else(|| {
                let functions = self.stack.scopes.get(module)
                    .unwrap_or_else(|| panic!("Missing global scope: {module}"))
                    .functions.get(value)?;
                filter(functions, self.args)
            })
            .unwrap_or_else(|| {
                panic!(
                    "Unable to find function with name `{value}` and args `{}`",
                    self.args.iter().map(|arg| format!("{}", arg)).collect::<Vec<String>>()
                        .join(", ")
                )
            })
    }
}

pub struct SetVar<'a> {
    stack: &'a mut Stack,
}

impl<'a> ExprVisitor for SetVar<'a> {
    type Output = Ty;

    fn visit_default(&mut self) -> Self::Output {
        unreachable!()
    }

    fn visit_ident_expr(&mut self, value: &String) -> Self::Output {
        let (_, scopes) = self.stack.stack.last_mut().expect("set_var: empty stack");
        let ty = scopes.iter()
            .find_map(|scope| scope.variables.get(value))
            .cloned();
        let ty = ty.unwrap_or_else(|| {
            let scope = scopes.last_mut().unwrap();
            scope.variables.insert(value.clone(), None);
            scope.variables.get(value).unwrap().clone()
        });
        Ty::Ref(ty.clone().map(Box::new))
    }
}
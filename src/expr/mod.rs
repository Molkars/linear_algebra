use std::fmt::{Display, Formatter};
use std::ops::Deref;

pub mod convert;


#[derive(Debug, Clone)]
pub enum Expr {
    Block(Vec<Expr>),
    Set(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Rem(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Pos(Box<Expr>),
    Neg(Box<Expr>),
    Not(Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Index(Box<Expr>, Vec<Expr>),
    Bool(bool),
    Int(u64),
    Dec(f64),
    Unit,
    Mat(Matrix),
    Ident(String),
    If(Box<Expr>, Box<Expr>, Vec<(Expr, Expr)>, Option<Box<Expr>>),
    // cond (elseif)* else?
    Group(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Int,
    Dec,
    Num,
    Bool,
    Unit,
    Array(Box<Ty>, u64),
    // ty, h, w
    Mat(Box<Ty>, u64, u64),
    Ref(Box<Ty>),
    FnPtr(Vec<Ty>, Box<Ty>),
}

impl Ty {
    pub fn filter(self, f: impl FnOnce(&Self) -> bool) -> Option<Self> {
        if f(&self) {
            Some(self)
        } else {
            None
        }
    }

    pub fn as_fn_ptr(&self) -> Option<(&[Ty], &Ty)> {
        match self {
            Ty::FnPtr(args, ret) => Some((args.as_slice(), ret.as_ref())),
            _ => None,
        }
    }

    pub fn into_fn_ptr(self) -> (Vec<Ty>, Ty) {
        match self {
            Ty::FnPtr(args, ret) => (args, ret.deref().clone()),
            _ => panic!("into_fn_ptr called on non fn-ptr"),
        }
    }

    pub fn can_convert_into(&self, into: &Ty) -> bool {
        match (self, into) {
            (Ty::Ref(l), r) => l.as_ref().filter(|l| l.as_ref().eq(r)).is_some(),
            (Ty::Int | Ty::Dec, Ty::Num) => true,
            (l, r) => l == r,
        }
    }

    pub fn cmp(&self, other: &Ty) -> Option<Ty> {
        match (self, other) {
            (Ty::Ref(r), o) | (o, Ty::Ref(r)) if r.as_ref().unwrap().as_ref() == o => Some(Ty::Bool),
            (l, r) if l == r => Some(Ty::Bool),
            (l, r) => panic!("eq: cannot compare {} and {}", l, r),
        }
    }

    pub fn ord(&self, other: &Ty) -> Option<Ty> {
        match (self, other) {
            (Ty::Ref(l), Ty::Ref(r)) => l.as_ref().unwrap().cmp(r.as_ref().unwrap().as_ref()),
            (Ty::Ref(l), r) => l.as_ref().unwrap().as_ref().cmp(r),
            (l, Ty::Ref(r)) => l.cmp(r.as_ref().unwrap().as_ref()),
            (Ty::Int | Ty::Dec | Ty::Num, Ty::Int | Ty::Dec | Ty::Num) => Some(Ty::Bool),
            (Ty::Bool, Ty::Bool) => Some(Ty::Bool),
            (Ty::Mat(a, h1, w1), Ty::Mat(b, h2, w2)) => {
                if h1 != h2 || w1 != w2 {
                    None
                } else {
                    a.as_ref().ord(b.as_ref())
                }
            }
            (Ty::Array(a, l1), Ty::Array(b, l2)) => {
                if l1 != l2 {
                    None
                } else {
                    a.as_ref().ord(b.as_ref())
                }
            }
            _ => None,
        }
    }

    pub fn add(&self, other: &Ty) -> Option<Ty> {
        match (self, other) {
            (Ty::Ref(l), Ty::Ref(r)) => l.as_ref().unwrap().as_ref().add(r.as_ref().unwrap().as_ref()),
            (Ty::Ref(l), r) => l.as_ref().unwrap().as_ref().add(r),
            (l, Ty::Ref(r)) => l.add(r.as_ref().unwrap().as_ref()),
            (Ty::Num, Ty::Int | Ty::Dec | Ty::Num) => Some(Ty::Num),
            (Ty::Int, Ty::Int) => Some(Ty::Int),
            (Ty::Dec, Ty::Dec) => Some(Ty::Int),
            (Ty::Mat(l, lh, lw), Ty::Mat(r, rh, rw)) => {
                if lh != rh || lw != rw {
                    None
                } else {
                    l.as_ref().add(r.as_ref())
                        .map(|ty| Ty::Mat(Box::new(ty), *lh, *lw))
                }
            }
            _ => None,
        }
    }

    pub fn sub(&self, other: &Ty) -> Option<Ty> {
        self.add(other) // same for now
    }

    pub fn mul(&self, other: &Ty) -> Option<Ty> {
        match (self, other) {
            (Ty::Ref(l), Ty::Ref(r)) => l.as_ref().unwrap().as_ref().mul(r.as_ref().unwrap().as_ref()),
            (Ty::Ref(l), r) => l.as_ref().unwrap().as_ref().mul(r),
            (l, Ty::Ref(r)) => l.mul(r.as_ref().unwrap().as_ref()),
            (Ty::Int, Ty::Int) => Some(Ty::Int),
            (Ty::Dec, Ty::Dec) => Some(Ty::Dec),
            (Ty::Num, Ty::Int | Ty::Dec | Ty::Num) => Some(Ty::Num),
            (Ty::Mat(lt, lh, lw), Ty::Mat(rt, rh, rw)) => {
                if lw != rh { // Each row in l must be the same length as each col in r
                    None
                } else {
                    let mul = lt.as_ref().mul(rt.as_ref())?; // a_0*b_0
                    let sum = mul.add(&mul)?; // (a_0*b_0) + (a_1*b_0)
                    Some(if *lh == 1 && *rw == 1 { // [1, 0, 0] * [1; 0; 0] = [1*1 + 0*0 + 0*0] = [0] -unwrap-> 0
                        sum
                    } else {
                        Ty::Mat(Box::new(sum), *lh, *rw)
                    })
                }
            }
            _ => None,
        }
    }

    pub fn div(&self, other: &Ty) -> Option<Ty> {
        self.mul(other) // same for now
    }

    pub fn rem(&self, other: &Ty) -> Option<Ty> {
        match (self, other) {
            (Ty::Ref(l), Ty::Ref(r)) => l.as_ref().unwrap().as_ref().rem(r.as_ref().unwrap().as_ref()),
            (Ty::Ref(l), r) => l.as_ref().unwrap().as_ref().rem(r),
            (l, Ty::Ref(r)) => l.rem(r.as_ref().unwrap().as_ref()),
            (Ty::Int, Ty::Int) => Some(Ty::Int),
            (Ty::Dec, Ty::Dec) => Some(Ty::Dec),
            (Ty::Num, Ty::Int | Ty::Dec | Ty::Num) => Some(Ty::Num),
            _ => None,
        }
    }

    pub fn pow(&self, other: &Ty) -> Option<Ty> {
        match (self, other) {
            (Ty::Ref(l), Ty::Ref(r)) => l.as_ref().unwrap().as_ref().rem(r.as_ref().unwrap().as_ref()),
            (Ty::Ref(l), r) => l.as_ref().unwrap().as_ref().rem(r),
            (l, Ty::Ref(r)) => l.rem(r.as_ref().unwrap().as_ref()),
            (Ty::Int, Ty::Int) => Some(Ty::Int),
            (Ty::Dec, Ty::Dec) => Some(Ty::Dec),
            (Ty::Num, Ty::Int | Ty::Dec | Ty::Num) => Some(Ty::Num),
            _ => None,
        }
    }

    pub fn pos(&self) -> Option<Ty> {
        match self {
            Ty::Int | Ty::Dec | Ty::Num | Ty::Mat(_, _, _) => Some(self.clone()),
            _ => None,
        }
    }

    pub fn neg(&self) -> Option<Ty> {
        match self {
            Ty::Int | Ty::Dec | Ty::Num | Ty::Mat(_, _, _) => Some(self.clone()),
            _ => None,
        }
    }

    pub fn not(&self) -> Option<Ty> {
        match self {
            Ty::Bool => Some(Ty::Bool),
            _ => None,
        }
    }

    pub fn index(&self, indices: &[Ty]) -> Option<Ty> {
        match self {
            Ty::Array(arr, _) => {
                let mut iter = indices.iter();
                iter.next()?;
                if iter.next().is_some() {
                    None
                } else {
                    Some(arr.as_ref().clone())
                }
            }
            Ty::Mat(..) => {
                let mut _iter = indices.iter();
                todo!()
            }
            Ty::Ref(ty) => ty.as_ref().unwrap().index(indices),
            _ => None,
        }
    }

    /// Returns the most general type between [self] and [other].
    ///
    /// # Example
    /// best(int, num) -> num
    /// best(int, dec) -> dec
    pub fn best(&self, other: &Ty) -> Option<Ty> {
        match (self, other) {
            (Ty::Int, Ty::Dec) => Some(Ty::Dec),
            (Ty::Int | Ty::Dec | Ty::Num, Ty::Int | Ty::Dec | Ty::Num) => Some(Ty::Num),
            (Ty::Mat(t1, h1, w1), Ty::Mat(t2, h2, w2)) => {
                if h1 != h2 || w1 != w2 {
                    None
                } else {
                    let best = t1.best(t2.as_ref())?;
                    Some(Ty::Mat(Box::new(best), *h1, *w1))
                }
            }
            (Ty::Array(t1, l1), Ty::Array(t2, l2)) => {
                if l1 != l2 {
                    None
                } else {
                    t1.best(t2.as_ref()).map(|ty| Ty::Array(Box::new(ty), *l1))
                }
            }
            (l, r) if l == r => Some(l.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Decl {
    Func(String, Vec<(String, Ty)>, Ty, Expr),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct Matrix(pub Vec<Vec<Expr>>);

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Block(block) => {
                write!(f, "{{")?;
                if !block.is_empty() {
                    writeln!(f)?;
                    for expr in block {
                        writeln!(f, "  {}", expr)?;
                    }
                }
                write!(f, "}}")
            }
            Expr::Set(l, r) => write!(f, "{} = {}", l.as_ref(), r.as_ref()),
            Expr::Or(l, r) => write!(f, "{} or {}", l.as_ref(), r.as_ref()),
            Expr::And(l, r) => write!(f, "{} and {}", l.as_ref(), r.as_ref()),
            Expr::Eq(l, r) => write!(f, "{} == {}", l.as_ref(), r.as_ref()),
            Expr::Ne(l, r) => write!(f, "{} != {}", l.as_ref(), r.as_ref()),
            Expr::Le(l, r) => write!(f, "{} <= {}", l.as_ref(), r.as_ref()),
            Expr::Lt(l, r) => write!(f, "{} < {}", l.as_ref(), r.as_ref()),
            Expr::Ge(l, r) => write!(f, "{} >= {}", l.as_ref(), r.as_ref()),
            Expr::Gt(l, r) => write!(f, "{} > {}", l.as_ref(), r.as_ref()),
            Expr::Add(l, r) => write!(f, "{} + {}", l.as_ref(), r.as_ref()),
            Expr::Sub(l, r) => write!(f, "{} - {}", l.as_ref(), r.as_ref()),
            Expr::Mul(l, r) => write!(f, "{} * {}", l.as_ref(), r.as_ref()),
            Expr::Div(l, r) => write!(f, "{} / {}", l.as_ref(), r.as_ref()),
            Expr::Rem(l, r) => write!(f, "{} % {}", l.as_ref(), r.as_ref()),
            Expr::Pow(l, r) => write!(f, "{} ^ {}", l.as_ref(), r.as_ref()),
            Expr::Pos(e) => write!(f, "+{}", e.as_ref()),
            Expr::Neg(e) => write!(f, "-{}", e.as_ref()),
            Expr::Not(e) => write!(f, "!{}", e.as_ref()),
            Expr::Call(functor, args) => {
                write!(f, "{}(", functor.as_ref())?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expr::Index(expr, indices) => {
                write!(f, "{}[", expr.as_ref())?;
                for (i, indice) in indices.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", indice)?;
                }
                write!(f, "]")
            }
            Expr::Int(val) => write!(f, "{val}"),
            Expr::Dec(val) => write!(f, "{val}"),
            Expr::Bool(val) => write!(f, "{val}"),
            Expr::Unit => write!(f, "()"),
            Expr::Mat(val) => write!(f, "{val}"),
            Expr::Ident(val) => write!(f, "{val}"),
            Expr::If(cond, body, elif, else_) => {
                write!(f, "if {} {}", cond.as_ref(), body.as_ref())?;
                for (cond, body) in elif {
                    write!(f, " else if {} {}", cond, body)?;
                }
                if let Some(else_) = else_ {
                    write!(f, " else {}", else_.as_ref())
                } else {
                    Ok(())
                }
            }
            Expr::Group(expr) => write!(f, "({})", expr.as_ref()),
        }
    }
}


impl Display for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Int => write!(f, "int"),
            Ty::Dec => write!(f, "dec"),
            Ty::Num => write!(f, "num"),
            Ty::Bool => write!(f, "bool"),
            Ty::Unit => write!(f, "()"),
            Ty::Ref(ty) => write!(f, "&{}", ty.as_ref().unwrap()),
            Ty::Array(ty, len) => write!(f, "[{}; {}]", ty.as_ref(), len),
            Ty::Mat(ty, height, width) =>
                write!(f, "[{}; {height}x{width}]", ty.as_ref()),
            Ty::FnPtr(args, ret) => {
                write!(f, "fn*(")?;
                for (y, arg) in args.iter().enumerate() {
                    if y > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ") {}", ret.as_ref())
            }
        }
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Decl::Func(name, args, ret, body) => {
                write!(f, "{name}(")?;
                for (y, (name, ty)) in args.iter().enumerate() {
                    if y > 0 { write!(f, ", ")?; }
                    write!(f, "{name} {ty}")?;
                }
                write!(f, ") {ret} = {body}")
            }
            Decl::Expr(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug)]
pub enum MatrixConstructionError {
    MisSizedRow {
        /// The index of the row 1->n
        row: usize,
        /// The length of the row
        len: usize,
        /// The width of the matrix
        width: usize,
    },
    EmptyMatrix,
}

impl TryFrom<Vec<Vec<Expr>>> for Matrix {
    type Error = MatrixConstructionError;

    fn try_from(value: Vec<Vec<Expr>>) -> Result<Self, Self::Error> {
        let mut iter = value.iter().enumerate();
        let len = iter.next().ok_or_else(|| MatrixConstructionError::EmptyMatrix)?.1.len();
        for (y, row) in iter {
            if row.len() != len {
                return Err(MatrixConstructionError::MisSizedRow {
                    width: len,
                    len: row.len(),
                    row: y + 1,
                });
            }
        }
        Ok(Matrix(value))
    }
}

impl Display for Matrix {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[ ")?;
        for row in self.0.iter() {
            for (x, val) in row.iter().enumerate() {
                if x > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", val)?;
            }
            write!(f, "; ")?;
        }
        write!(f, "]")
    }
}


pub trait ExprVisitor {
    type Output;

    fn visit_expr(&mut self, expr: &Expr) -> Self::Output {
        match expr {
            Expr::Block(exprs) => self.visit_block_expr(exprs.as_slice()),
            Expr::Set(l, r) => self.visit_set_expr(l.as_ref(), r.as_ref()),
            Expr::Or(l, r) => self.visit_or_expr(l.as_ref(), r.as_ref()),
            Expr::And(l, r) => self.visit_and_expr(l.as_ref(), r.as_ref()),
            Expr::Eq(l, r) => self.visit_eq_expr(l.as_ref(), r.as_ref()),
            Expr::Ne(l, r) => self.visit_ne_expr(l.as_ref(), r.as_ref()),
            Expr::Le(l, r) => self.visit_le_expr(l.as_ref(), r.as_ref()),
            Expr::Lt(l, r) => self.visit_lt_expr(l.as_ref(), r.as_ref()),
            Expr::Ge(l, r) => self.visit_ge_expr(l.as_ref(), r.as_ref()),
            Expr::Gt(l, r) => self.visit_gt_expr(l.as_ref(), r.as_ref()),
            Expr::Add(l, r) => self.visit_add_expr(l.as_ref(), r.as_ref()),
            Expr::Sub(l, r) => self.visit_sub_expr(l.as_ref(), r.as_ref()),
            Expr::Mul(l, r) => self.visit_mul_expr(l.as_ref(), r.as_ref()),
            Expr::Div(l, r) => self.visit_div_expr(l.as_ref(), r.as_ref()),
            Expr::Rem(l, r) => self.visit_rem_expr(l.as_ref(), r.as_ref()),
            Expr::Pow(l, r) => self.visit_pow_expr(l.as_ref(), r.as_ref()),
            Expr::Pos(e) => self.visit_pos_expr(e.as_ref()),
            Expr::Neg(e) => self.visit_neg_expr(e.as_ref()),
            Expr::Not(e) => self.visit_not_expr(e.as_ref()),
            Expr::Call(f, a) => self.visit_call_expr(f.as_ref(), a.as_slice()),
            Expr::Index(e, i) => self.visit_index_expr(e.as_ref(), i.as_slice()),
            Expr::Bool(v) => self.visit_bool_expr(*v),
            Expr::Int(v) => self.visit_int_expr(*v),
            Expr::Dec(v) => self.visit_dec_expr(*v),
            Expr::Unit => self.visit_unit_expr(),
            Expr::Mat(v) => self.visit_mat_expr(v),
            Expr::Ident(v) => self.visit_ident_expr(v),
            Expr::If(c, b, ei, e) => {
                let e = e.as_ref().map(|v| v.as_ref());
                self.visit_if_expr(c.as_ref(), b.as_ref(), ei.as_slice(), e)
            }
            Expr::Group(e) => self.visit_group_expr(e),
        }
    }

    fn visit_default(&mut self) -> Self::Output;

    fn visit_block_expr(&mut self, _expressions: &[Expr]) -> Self::Output { self.visit_default() }
    fn visit_set_expr(&mut self, _left: &Expr, _right: &Expr) -> Self::Output { self.visit_default() }
    fn visit_or_expr(&mut self, _left: &Expr, _right: &Expr) -> Self::Output { self.visit_default() }
    fn visit_and_expr(&mut self, _left: &Expr, _right: &Expr) -> Self::Output { self.visit_default() }
    fn visit_eq_expr(&mut self, _left: &Expr, _right: &Expr) -> Self::Output { self.visit_default() }
    fn visit_ne_expr(&mut self, _left: &Expr, _right: &Expr) -> Self::Output { self.visit_default() }
    fn visit_le_expr(&mut self, _left: &Expr, _right: &Expr) -> Self::Output { self.visit_default() }
    fn visit_lt_expr(&mut self, _left: &Expr, _right: &Expr) -> Self::Output { self.visit_default() }
    fn visit_ge_expr(&mut self, _left: &Expr, _right: &Expr) -> Self::Output { self.visit_default() }
    fn visit_gt_expr(&mut self, _left: &Expr, _right: &Expr) -> Self::Output { self.visit_default() }
    fn visit_add_expr(&mut self, _left: &Expr, _right: &Expr) -> Self::Output { self.visit_default() }
    fn visit_sub_expr(&mut self, _left: &Expr, _right: &Expr) -> Self::Output { self.visit_default() }
    fn visit_mul_expr(&mut self, _left: &Expr, _right: &Expr) -> Self::Output { self.visit_default() }
    fn visit_div_expr(&mut self, _left: &Expr, _right: &Expr) -> Self::Output { self.visit_default() }
    fn visit_rem_expr(&mut self, _left: &Expr, _right: &Expr) -> Self::Output { self.visit_default() }
    fn visit_pow_expr(&mut self, _left: &Expr, _right: &Expr) -> Self::Output { self.visit_default() }
    fn visit_pos_expr(&mut self, _expr: &Expr) -> Self::Output { self.visit_default() }
    fn visit_neg_expr(&mut self, _expr: &Expr) -> Self::Output { self.visit_default() }
    fn visit_not_expr(&mut self, _expr: &Expr) -> Self::Output { self.visit_default() }
    fn visit_call_expr(&mut self, _functor: &Expr, _args: &[Expr]) -> Self::Output { self.visit_default() }
    fn visit_index_expr(&mut self, _expr: &Expr, _indices: &[Expr]) -> Self::Output { self.visit_default() }
    fn visit_bool_expr(&mut self, _value: bool) -> Self::Output { self.visit_default() }
    fn visit_int_expr(&mut self, _value: u64) -> Self::Output { self.visit_default() }
    fn visit_dec_expr(&mut self, _value: f64) -> Self::Output { self.visit_default() }
    fn visit_unit_expr(&mut self) -> Self::Output { self.visit_default() }
    fn visit_mat_expr(&mut self, _value: &Matrix) -> Self::Output { self.visit_default() }
    fn visit_ident_expr(&mut self, _value: &String) -> Self::Output { self.visit_default() }
    fn visit_if_expr(
        &mut self, _cond: &Expr, _body: &Expr, _elif: &[(Expr, Expr)], _else_: Option<&Expr>,
    ) -> Self::Output { self.visit_default() }
    fn visit_group_expr(&mut self, expr: &Expr) -> Self::Output { self.visit_expr(expr) }
}
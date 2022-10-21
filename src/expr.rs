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
    Func(String, Vec<(String, Ty)>, Ty, Box<Expr>),
    Int(isize),
    Uint(usize),
    Dec(f64),
    Mat(Matrix),
    Ty(Ty),
    Ident(String),
}

impl Expr {
    pub fn into_ty(self) -> Ty {
        match self {
            Self::Ty(ty) => ty,
            _ => panic!("into_ty called on a variant that was not Expr::Ty"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Ty {
    Int,
    Dec,
    Num,
    Array(Box<Ty>, usize),
    Mat(Box<Ty>, usize, usize),
}

#[derive(Debug, Clone)]
pub struct Matrix(Vec<Vec<Expr>>);

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
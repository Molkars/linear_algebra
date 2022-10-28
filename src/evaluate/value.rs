
pub enum Value {
    Integer(i64),
    Decimal(f64),
    Array(Vec<Value>, u32),
    Matrix(Vec<Value>, u32, u32),
}

pub enum Matrix {

}
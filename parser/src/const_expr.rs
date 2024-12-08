use std::ops::{Add, Sub, Mul, Div, Rem, Not, Neg, Shl, Shr, BitAnd, BitOr, BitXor};
use std::cmp::Ordering;
use crate::{ExprAST, NumberType, Type};

use super::{ParserError, Position};

#[derive(Debug, Clone)]
pub enum ConstExpr {
    Int(i64, Position),
    Unsigned(u64, Position),
    LongLong(i128, Position),
    ULongLong(u128, Position),
    Double(f64, Position),
}

impl ConstExpr {
    pub fn get_position(&self) -> &Position {
        match self {
            ConstExpr::Int(_, pos) => pos,
            ConstExpr::Unsigned(_, pos) => pos,
            ConstExpr::LongLong(_, pos) => pos,
            ConstExpr::ULongLong(_, pos) => pos,
            ConstExpr::Double(_, pos) => pos,
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            ConstExpr::Int(_, _) => Type::Number(NumberType::Int),
            ConstExpr::Unsigned(_, _) => Type::Number(NumberType::UnsignedInt),
            ConstExpr::LongLong(_, _) => Type::Number(NumberType::LongLong),
            ConstExpr::ULongLong(_, _) => Type::Number(NumberType::UnsignedLongLong),
            ConstExpr::Double(_, _) => Type::Number(NumberType::Double),
        }
    }

    pub fn to_usize(&self) -> Result<usize, ParserError> {
        match self {
            Self::Int(num, _pos) => Ok(*num as usize),
            Self::Unsigned(num, _pos) => Ok(*num as usize),
            Self::LongLong(num, _pos) => Ok(*num as usize),
            Self::ULongLong(num, _pos) => Ok(*num as usize),

            _ => Err(ParserError::cannot_convert_to_usize(self, self.get_position().clone())),
        }
    }

    pub fn as_i32_value(&self) -> i32 {
        match self {
            Self::Int(n, _) => *n as i32,
            Self::Unsigned(n, _) => *n as i32,
            Self::LongLong(n, _) => *n as i32,
            Self::ULongLong(n, _) => *n as i32,
            Self::Double(f, _) => *f as i32,
        }
    }

    pub fn as_u32_value(&self) -> u32 {
        match self {
            Self::Int(n, _) => *n as u32,
            Self::Unsigned(n, _) => *n as u32,
            Self::LongLong(n, _) => *n as u32,
            Self::ULongLong(n, _) => *n as u32,
            Self::Double(f, _) => *f as u32,
        }
    }

    pub fn as_i64_value(&self) -> i64 {
        match self {
            Self::Int(n, _) => *n,
            Self::Unsigned(n, _) => *n as i64,
            Self::LongLong(n, _) => *n as i64,
            Self::ULongLong(n, _) => *n as i64,
            Self::Double(f, _) => *f as i64,
        }
    }

    pub fn as_u64_value(&self) -> u64 {
        match self {
            Self::Int(n, _) => *n as u64,
            Self::Unsigned(n, _) => *n,
            Self::LongLong(n, _) => *n as u64,
            Self::ULongLong(n, _) => *n as u64,
            Self::Double(f, _) => *f as u64,
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            Self::Int(0, _) | Self::Unsigned(0, _) | Self::LongLong(0, _) | Self::ULongLong(0, _) => true,
            // Self::Double(num) => num == 0.0,
            _ => false,
        }
    }
}

macro_rules! op_expand {
    ($self:tt $op:tt $other:tt) => (
        match ($self, $other) {
            (Self::Int(n1, pos), Self::Int(n2, _pos2)) => ConstExpr::Int(n1 $op n2, pos),
            (Self::Int(n1, pos), Self::Unsigned(n2, _pos2)) => ConstExpr::Int(n1 $op n2 as i64, pos),
            (Self::Int(n1, pos), Self::LongLong(n2, _pos2)) => ConstExpr::LongLong(n1 as i128 $op n2, pos),
            (Self::Int(n1, pos), Self::ULongLong(n2, _pos2)) => ConstExpr::LongLong(n1 as i128 $op n2 as i128, pos),
            (Self::Int(n1, pos), Self::Double(n2, _pos2)) => ConstExpr::Double(n1 as f64 $op n2, pos),

            (Self::Unsigned(n1, pos), Self::Int(n2, _pos2)) => ConstExpr::Int(n1 as i64 $op n2, pos),
            (Self::Unsigned(n1, pos), Self::Unsigned(n2, _pos2)) => ConstExpr::Unsigned(n1 $op n2, pos),
            (Self::Unsigned(n1, pos), Self::LongLong(n2, _pos2)) => ConstExpr::LongLong(n1 as i128 $op n2, pos),
            (Self::Unsigned(n1, pos), Self::ULongLong(n2, _pos2)) => ConstExpr::ULongLong(n1 as u128 $op n2, pos),
            (Self::Unsigned(n1, pos), Self::Double(n2, _pos2)) => ConstExpr::Double(n1 as f64 $op n2, pos),

            (Self::LongLong(n1, pos), Self::Int(n2, _pos2)) => ConstExpr::LongLong(n1 $op n2 as i128, pos),
            (Self::LongLong(n1, pos), Self::Unsigned(n2, _pos2)) => ConstExpr::LongLong(n1 $op n2 as i128, pos),
            (Self::LongLong(n1, pos), Self::LongLong(n2, _pos2)) => ConstExpr::LongLong(n1 $op n2, pos),
            (Self::LongLong(n1, pos), Self::ULongLong(n2, _pos2)) => ConstExpr::LongLong(n1 $op n2 as i128, pos),
            (Self::LongLong(n1, pos), Self::Double(n2, _pos2)) => ConstExpr::Double(n1 as f64 $op n2, pos),

            (Self::ULongLong(n1, pos), Self::Int(n2, _pos2)) => ConstExpr::LongLong(n1 as i128 $op n2 as i128, pos),
            (Self::ULongLong(n1, pos), Self::Unsigned(n2, _pos2)) => ConstExpr::ULongLong(n1 $op n2 as u128, pos),
            (Self::ULongLong(n1, pos), Self::LongLong(n2, _pos2)) => ConstExpr::LongLong(n1 as i128 $op n2, pos),
            (Self::ULongLong(n1, pos), Self::ULongLong(n2, _pos2)) => ConstExpr::ULongLong(n1 $op n2, pos),
            (Self::ULongLong(n1, pos), Self::Double(n2, _pos2)) => ConstExpr::Double(n1 as f64 $op n2, pos),

            (Self::Double(n1, pos), Self::Int(n2, _pos2)) => ConstExpr::Double(n1 $op n2 as f64, pos),
            (Self::Double(n1, pos), Self::Unsigned(n2, _pos2)) => ConstExpr::Double(n1 $op n2 as f64, pos),
            (Self::Double(n1, pos), Self::LongLong(n2, _pos2)) => ConstExpr::Double(n1 $op n2 as f64, pos),
            (Self::Double(n1, pos), Self::ULongLong(n2, _pos2)) => ConstExpr::Double(n1 $op n2 as f64, pos),
            (Self::Double(n1, pos), Self::Double(n2, _pos2)) => ConstExpr::Double(n1 $op n2, pos),
        }
    )
}

macro_rules! op_expand_without_float {
    ($self:tt $op:tt $other:tt) => (
        match ($self, $other) {
            (Self::Int(n1, pos), Self::Int(n2, _pos2)) => Ok(ConstExpr::Int(n1 $op n2, pos)),
            (Self::Int(n1, pos), Self::Unsigned(n2, _pos2)) => Ok(ConstExpr::Int(n1 $op (n2 as i64), pos)),
            (Self::Int(n1, pos), Self::LongLong(n2, _pos2)) => Ok(ConstExpr::LongLong((n1 as i128) $op n2, pos)),
            (Self::Int(n1, pos), Self::ULongLong(n2, _pos2)) => Ok(ConstExpr::LongLong((n1 as i128) $op (n2 as i128), pos)),
            (Self::Int(_n1, pos), Self::Double(_n2, _pos)) => Err(ParserError::cannot_apply_operator_to_float(str_from_op!($op), pos.clone())),

            (Self::Unsigned(n1, pos), Self::Int(n2, _pos2)) => Ok(ConstExpr::Int((n1 as i64) $op n2, pos)),
            (Self::Unsigned(n1, pos), Self::Unsigned(n2, _pos2)) => Ok(ConstExpr::Unsigned(n1 $op n2, pos)),
            (Self::Unsigned(n1, pos), Self::LongLong(n2, _pos2)) => Ok(ConstExpr::LongLong((n1 as i128) $op n2, pos)),
            (Self::Unsigned(n1, pos), Self::ULongLong(n2, _pos2)) => Ok(ConstExpr::ULongLong((n1 as u128) $op n2, pos)),
            (Self::Unsigned(_n1, pos), Self::Double(_n2, _pos2)) => Err(ParserError::cannot_apply_operator_to_float(str_from_op!($op), pos.clone())),

            (Self::LongLong(n1, pos), Self::Int(n2, _pos2)) => Ok(ConstExpr::LongLong(n1 $op (n2 as i128), pos)),
            (Self::LongLong(n1, pos), Self::Unsigned(n2, _pos2)) => Ok(ConstExpr::LongLong(n1 $op (n2 as i128), pos)),
            (Self::LongLong(n1, pos), Self::LongLong(n2, _pos2)) => Ok(ConstExpr::LongLong(n1 $op n2, pos)),
            (Self::LongLong(n1, pos), Self::ULongLong(n2, _pos2)) => Ok(ConstExpr::LongLong(n1 $op (n2 as i128), pos)),
            (Self::LongLong(_n1, pos), Self::Double(_n2, _pos2)) => Err(ParserError::cannot_apply_operator_to_float(str_from_op!($op), pos.clone())),

            (Self::ULongLong(n1, pos), Self::Int(n2, _pos2)) => Ok(ConstExpr::LongLong((n1 as i128) $op (n2 as i128), pos)),
            (Self::ULongLong(n1, pos), Self::Unsigned(n2, _pos2)) => Ok(ConstExpr::ULongLong(n1 $op (n2 as u128), pos)),
            (Self::ULongLong(n1, pos), Self::LongLong(n2, _pos2)) => Ok(ConstExpr::LongLong((n1 as i128) $op n2, pos)),
            (Self::ULongLong(n1, pos), Self::ULongLong(n2, _pos2)) => Ok(ConstExpr::ULongLong(n1 $op n2, pos)),
            (Self::ULongLong(_n1, pos), Self::Double(_n2, _pos2)) => Err(ParserError::cannot_apply_operator_to_float(str_from_op!($op), pos.clone())),

            (Self::Double(_n1, pos), _) =>  Err(ParserError::cannot_apply_operator_to_float(str_from_op!($op), pos.clone())),
        }
    )
}

macro_rules! str_from_op {
    (<<) => ("<<");
    (>>) => (">>");
    (&) => ("'");
    (|) => ("|");
    (^) => ("^");
}

impl Add for ConstExpr {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match (self, other) {
            (Self::Int(n1, pos), Self::Int(n2, _pos2)) => ConstExpr::Int(n1 + n2, pos),
            (Self::Int(n1, pos), Self::Unsigned(n2, _pos2)) => ConstExpr::Int(n1 + n2 as i64, pos),
            (Self::Int(n1, pos), Self::LongLong(n2, _pos2)) => ConstExpr::LongLong(n1 as i128 + n2, pos),
            (Self::Int(n1, pos), Self::ULongLong(n2, _pos2)) => ConstExpr::LongLong(n1 as i128 + n2 as i128, pos),
            (Self::Int(n1, pos), Self::Double(n2, _pos2)) => ConstExpr::Double(n1 as f64 + n2, pos),

            (Self::Unsigned(n1, pos), Self::Int(n2, _pos2)) => ConstExpr::Int(n1 as i64 + n2, pos),
            (Self::Unsigned(n1, pos), Self::Unsigned(n2, _pos2)) => ConstExpr::Unsigned(n1 + n2, pos),
            (Self::Unsigned(n1, pos), Self::LongLong(n2, _pos2)) => ConstExpr::LongLong(n1 as i128 + n2, pos),
            (Self::Unsigned(n1, pos), Self::ULongLong(n2, _pos2)) => ConstExpr::ULongLong(n1 as u128 + n2, pos),
            (Self::Unsigned(n1, pos), Self::Double(n2, _pos2)) => ConstExpr::Double(n1 as f64 + n2, pos),

            (Self::LongLong(n1, pos), Self::Int(n2, _pos2)) => ConstExpr::LongLong(n1 + n2 as i128, pos),
            (Self::LongLong(n1, pos), Self::Unsigned(n2, _pos2)) => ConstExpr::LongLong(n1 + n2 as i128, pos),
            (Self::LongLong(n1, pos), Self::LongLong(n2, _pos2)) => ConstExpr::LongLong(n1 + n2, pos),
            (Self::LongLong(n1, pos), Self::ULongLong(n2, _pos2)) => ConstExpr::LongLong(n1 + n2 as i128, pos),
            (Self::LongLong(n1, pos), Self::Double(n2, _pos2)) => ConstExpr::Double(n1 as f64 + n2, pos),

            (Self::ULongLong(n1, pos), Self::Int(n2, _pos2)) => ConstExpr::LongLong(n1 as i128 + n2 as i128, pos),
            (Self::ULongLong(n1, pos), Self::Unsigned(n2, _pos2)) => ConstExpr::ULongLong(n1 + n2 as u128, pos),
            (Self::ULongLong(n1, pos), Self::LongLong(n2, _pos2)) => ConstExpr::LongLong(n1 as i128 + n2, pos),
            (Self::ULongLong(n1, pos), Self::ULongLong(n2, _pos2)) => ConstExpr::ULongLong(n1 + n2, pos),
            (Self::ULongLong(n1, pos), Self::Double(n2, _pos2)) => ConstExpr::Double(n1 as f64 + n2, pos),

            (Self::Double(n1, pos), Self::Int(n2, _pos2)) => ConstExpr::Double(n1 + n2 as f64, pos),
            (Self::Double(n1, pos), Self::Unsigned(n2, _pos2)) => ConstExpr::Double(n1 + n2 as f64, pos),
            (Self::Double(n1, pos), Self::LongLong(n2, _pos2)) => ConstExpr::Double(n1 + n2 as f64, pos),
            (Self::Double(n1, pos), Self::ULongLong(n2, _pos2)) => ConstExpr::Double(n1 + n2 as f64, pos),
            (Self::Double(n1, pos), Self::Double(n2, _pos2)) => ConstExpr::Double(n1 + n2, pos),
        }
    }
}

impl BitAnd for ConstExpr {
    type Output = Result<Self, ParserError>;

    // other is the "right-hand side" of the expression `a & b`
    fn bitand(self, other: Self) -> Self::Output {
        op_expand_without_float!(self & other)
    }
}

impl BitOr for ConstExpr {
    type Output = Result<Self, ParserError>;

    fn bitor(self, other: Self) -> Self::Output {
        op_expand_without_float!(self | other)
    }
}

impl BitXor for ConstExpr {
    type Output = Result<Self, ParserError>;

    fn bitxor(self, other: Self) -> Self::Output {
        op_expand_without_float!(self ^ other)
    }
}

impl Div for ConstExpr {
    type Output = Self;

    fn div(self, other: Self) -> Self::Output {
        if other.is_zero() {
            panic!("Cannot divide by zero-valued");
        }

        op_expand!(self / other)
    }
}

impl Mul for ConstExpr {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        op_expand!(self * other)
    }
}

impl Neg for ConstExpr {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            ConstExpr::Int(num, pos) => ConstExpr::Int(-num, pos),
            ConstExpr::Unsigned(num, pos) => ConstExpr::LongLong(-(num as i128), pos),
            ConstExpr::LongLong(num, pos) => ConstExpr::LongLong(-num, pos),
            ConstExpr::ULongLong(num, pos) => ConstExpr::LongLong(- (num as i128), pos),
            ConstExpr::Double(num, pos) => ConstExpr::Double(-num, pos),
        }

    }
}

impl Not for ConstExpr {
    type Output = Result<Self, ParserError>;

    fn not(self) -> Self::Output {
        match self {
            ConstExpr::Int(num, pos) => Ok(ConstExpr::Int(!num, pos)),
            ConstExpr::Unsigned(num, pos) => Ok(ConstExpr::Unsigned(!num, pos)),
            ConstExpr::LongLong(num, pos) => Ok(ConstExpr::LongLong(!num, pos)),
            ConstExpr::ULongLong(num, pos) => Ok(ConstExpr::ULongLong(!num, pos)),
            ConstExpr::Double(_num, pos) => Err(ParserError::cannot_not_of_float(pos.clone())),
        }
    }
}

impl Rem for ConstExpr {
    type Output = Self;

    fn rem(self, other: Self) -> Self::Output {
        op_expand!(self % other)
    }
}

impl Shl for ConstExpr {
    type Output = Result<Self, ParserError>;

    fn shl(self, other: Self) -> Self::Output {
        op_expand_without_float!(self << other)
    }
}

impl Shr for ConstExpr {
    type Output = Result<Self, ParserError>;

    fn shr(self, other: Self) -> Self::Output {
        op_expand_without_float!(self >> other)
    }
}

impl Sub for ConstExpr {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        op_expand!(self - other)
    }
}

impl PartialEq for ConstExpr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(n1, _pos), Self::Int(n2, _pos2)) => *n1 == *n2,
            (Self::Int(n1, _pos), Self::Unsigned(n2, _pos2)) => *n1 == *n2 as i64,
            (Self::Int(n1, _pos), Self::LongLong(n2, _pos2)) => *n1 as i128 == *n2,
            (Self::Int(n1, _pos), Self::ULongLong(n2, _pos2)) => *n1 as i128 == *n2 as i128,
            (Self::Int(n1, _pos), Self::Double(n2, _pos2)) => *n1 as f64 == *n2,

            (Self::Unsigned(n1, _pos), Self::Int(n2, _pos2)) => *n1 as i64 == *n2,
            (Self::Unsigned(n1, _pos), Self::Unsigned(n2, _pos2)) => *n1 == *n2,
            (Self::Unsigned(n1, _pos), Self::LongLong(n2, _pos2)) => *n1 as i128 == *n2,
            (Self::Unsigned(n1, _pos), Self::ULongLong(n2, _pos2)) => *n1 as u128 == *n2,
            (Self::Unsigned(n1, _pos), Self::Double(n2, _pos2)) => *n1 as f64 == *n2,

            (Self::LongLong(n1, _pos), Self::Int(n2, _pos2)) => *n1 == *n2 as i128,
            (Self::LongLong(n1, _pos), Self::Unsigned(n2, _pos2)) => *n1 == *n2 as i128,
            (Self::LongLong(n1, _pos), Self::LongLong(n2, _pos2)) => *n1 == *n2,
            (Self::LongLong(n1, _pos), Self::ULongLong(n2, _pos2)) => *n1 == *n2 as i128,
            (Self::LongLong(n1, _pos), Self::Double(n2, _pos2)) => *n1 as f64 == *n2,

            (Self::ULongLong(n1, _pos), Self::Int(n2, _pos2)) => *n1 as i128 == *n2 as i128,
            (Self::ULongLong(n1, _pos), Self::Unsigned(n2, _pos2)) => *n1 == *n2 as u128,
            (Self::ULongLong(n1, _pos), Self::LongLong(n2, _pos2)) => *n1 as i128 == *n2,
            (Self::ULongLong(n1, _pos), Self::ULongLong(n2, _pos2)) => *n1 == *n2,
            (Self::ULongLong(n1, _pos), Self::Double(n2, _pos2)) => *n1 as f64 == *n2,

            (Self::Double(n1, _pos), Self::Int(n2, _pos2)) => *n1 == *n2 as f64,
            (Self::Double(n1, _pos), Self::Unsigned(n2, _pos2)) => *n1 == *n2 as f64,
            (Self::Double(n1, _pos), Self::LongLong(n2, _pos2)) => *n1 == *n2 as f64,
            (Self::Double(n1, _pos), Self::ULongLong(n2, _pos2)) => *n1 == *n2 as f64,
            (Self::Double(n1, _pos), Self::Double(n2, _pos2)) => *n1 == *n2,
        }
    }
}
impl Eq for ConstExpr {}

impl PartialOrd for ConstExpr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Int(n1, _pos), Self::Int(n2, _pos2)) => n1.partial_cmp(n2),
            (Self::Int(n1, _pos), Self::Unsigned(n2, _pos2)) => n1.partial_cmp(&(*n2 as i64)),
            (Self::Int(n1, _pos), Self::LongLong(n2, _pos2)) => (*n1 as i128).partial_cmp(n2),
            (Self::Int(n1, _pos), Self::ULongLong(n2, _pos2)) => (*n1 as i128).partial_cmp(&(*n2 as i128)),
            (Self::Int(n1, _pos), Self::Double(n2, _pos2)) => (*n1 as f64).partial_cmp(n2),

            (Self::Unsigned(n1, _pos), Self::Int(n2, _pos2)) => (*n1 as i64).partial_cmp(n2),
            (Self::Unsigned(n1, _pos), Self::Unsigned(n2, _pos2)) => n1.partial_cmp(n2),
            (Self::Unsigned(n1, _pos), Self::LongLong(n2, _pos2)) => (*n1 as i128).partial_cmp(n2),
            (Self::Unsigned(n1, _pos), Self::ULongLong(n2, _pos2)) => (*n1 as u128).partial_cmp(n2),
            (Self::Unsigned(n1, _pos), Self::Double(n2, _pos2)) => (*n1 as f64).partial_cmp(n2),

            (Self::LongLong(n1, _pos), Self::Int(n2, _pos2)) => n1.partial_cmp(&(*n2 as i128)),
            (Self::LongLong(n1, _pos), Self::Unsigned(n2, _pos2)) => n1.partial_cmp(&(*n2 as i128)),
            (Self::LongLong(n1, _pos), Self::LongLong(n2, _pos2)) => n1.partial_cmp(n2),
            (Self::LongLong(n1, _pos), Self::ULongLong(n2, _pos2)) => n1.partial_cmp(&(*n2 as i128)),
            (Self::LongLong(n1, _pos), Self::Double(n2, _pos2)) => (*n1 as f64).partial_cmp(n2),

            (Self::ULongLong(n1, _pos), Self::Int(n2, _pos2)) => (*n1 as i128).partial_cmp(&(*n2 as i128)),
            (Self::ULongLong(n1, _pos), Self::Unsigned(n2, _pos2)) => n1.partial_cmp(&(*n2 as u128)),
            (Self::ULongLong(n1, _pos), Self::LongLong(n2, _pos2)) => (*n1 as i128).partial_cmp(n2),
            (Self::ULongLong(n1, _pos), Self::ULongLong(n2, _pos2)) => n1.partial_cmp(n2),
            (Self::ULongLong(n1, _pos), Self::Double(n2, _pos2)) => (*n1 as f64).partial_cmp(n2),

            (Self::Double(n1, _pos), Self::Int(n2, _pos2)) => n1.partial_cmp(&(*n2 as f64)),
            (Self::Double(n1, _pos), Self::Unsigned(n2, _pos2)) => n1.partial_cmp(&(*n2 as f64)),
            (Self::Double(n1, _pos), Self::LongLong(n2, _pos2)) => n1.partial_cmp(&(*n2 as f64)),
            (Self::Double(n1, _pos), Self::ULongLong(n2, _pos2)) => n1.partial_cmp(&(*n2 as f64)),
            (Self::Double(n1, _pos), Self::Double(n2, _pos2)) => n1.partial_cmp(n2),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn const_expr_add() {
        let dummy_pos = Position::new(1, 1);

        let i1 = ConstExpr::Int(1, dummy_pos.clone());
        let i2 = ConstExpr::Int(2, dummy_pos.clone());
        let u1 = ConstExpr::Unsigned(3, dummy_pos.clone());
        let u2 = ConstExpr::Unsigned(4, dummy_pos.clone());
        let l1 = ConstExpr::LongLong(5, dummy_pos.clone());
        let l2 = ConstExpr::LongLong(6, dummy_pos.clone());
        let ul1 = ConstExpr::ULongLong(7, dummy_pos.clone());
        let ul2 = ConstExpr::ULongLong(8, dummy_pos.clone());
        let d1 = ConstExpr::Double(9.9, dummy_pos.clone());
        let d2 = ConstExpr::Double(10.11, dummy_pos.clone());

        assert_eq!(i1.clone() + i2.clone(), ConstExpr::Int(3, dummy_pos.clone()));
        assert_eq!(i1.clone() + u1.clone(), ConstExpr::Int(4, dummy_pos.clone()));
        assert_eq!(u1.clone() + i1.clone(), ConstExpr::Int(4, dummy_pos.clone()));
        assert_eq!(u1.clone() + u2.clone(), ConstExpr::Unsigned(7, dummy_pos.clone()));
        assert_eq!(i1.clone() + l1.clone(), ConstExpr::LongLong(6, dummy_pos.clone()));
        assert_eq!(u1.clone() + l1.clone(), ConstExpr::LongLong(8, dummy_pos.clone()));
        assert_eq!(l1.clone() + l2.clone(), ConstExpr::LongLong(11, dummy_pos.clone()));
        assert_eq!(l1.clone() + i1.clone(), ConstExpr::LongLong(6, dummy_pos.clone()));
        assert_eq!(l1.clone() + u1.clone(), ConstExpr::LongLong(8, dummy_pos.clone()));
        assert_eq!(l1.clone() + ul1.clone(), ConstExpr::ULongLong(12, dummy_pos.clone()));
        assert_eq!(ul1.clone() + l1.clone(), ConstExpr::ULongLong(12, dummy_pos.clone()));
        assert_eq!(ul1.clone() + ul2.clone(), ConstExpr::ULongLong(15, dummy_pos.clone()));
        assert_eq!(d1.clone() + d2.clone(), ConstExpr::Double(9.9 + 10.11, dummy_pos.clone()));
        assert_eq!(d2.clone() + d1.clone(), ConstExpr::Double(9.9 + 10.11, dummy_pos.clone()));
    }

    #[test]
    fn const_expr_sub() {
        let dummy_pos = Position::new(1, 1);

        let i1 = ConstExpr::Int(1, dummy_pos.clone());
        let i2 = ConstExpr::Int(2, dummy_pos.clone());
        let u1 = ConstExpr::Unsigned(3, dummy_pos.clone());
        let u2 = ConstExpr::Unsigned(4, dummy_pos.clone());
        let l1 = ConstExpr::LongLong(5, dummy_pos.clone());
        let l2 = ConstExpr::LongLong(6, dummy_pos.clone());
        let ul1 = ConstExpr::ULongLong(7, dummy_pos.clone());
        let _ul2 = ConstExpr::ULongLong(8, dummy_pos.clone());
        let d1 = ConstExpr::Double(9.9, dummy_pos.clone());
        let d2 = ConstExpr::Double(10.11, dummy_pos.clone());

        assert_eq!(i1.clone() - i2.clone(), ConstExpr::Int(-1, dummy_pos.clone()));
        assert_eq!(i1.clone() - u1.clone(), ConstExpr::Int(-2, dummy_pos.clone()));
        assert_eq!(u1.clone() - i1.clone(), ConstExpr::Int(2, dummy_pos.clone()));
        assert_eq!(u2.clone() - u1.clone(), ConstExpr::Unsigned(1, dummy_pos.clone()));
        assert_eq!(i1.clone() - l1.clone(), ConstExpr::LongLong(-4, dummy_pos.clone()));
        assert_eq!(u1.clone() - l1.clone(), ConstExpr::LongLong(-2, dummy_pos.clone()));
        assert_eq!(l1.clone() - l2.clone(), ConstExpr::LongLong(-1, dummy_pos.clone()));
        assert_eq!(l1.clone() - i1.clone(), ConstExpr::LongLong(4, dummy_pos.clone()));
        assert_eq!(l1.clone() - u1.clone(), ConstExpr::LongLong(2, dummy_pos.clone()));
        // assert_eq!(l1.clone() - ul1.clone(), ConstExpr::ULongLong(-2));
        assert_eq!(ul1.clone() - l1.clone(), ConstExpr::ULongLong(2, dummy_pos.clone()));
        // assert_eq!(ul1.clone() - ul2.clone(), ConstExpr::ULongLong(1));
        assert_eq!(d1.clone() - d2.clone(), ConstExpr::Double(9.9 - 10.11, dummy_pos.clone()));
        assert_eq!(d2.clone() - d1.clone(), ConstExpr::Double(10.11 - 9.9, dummy_pos.clone()));
    }

    #[test]
    fn const_expr_shl() {
        let dummy_pos = Position::new(1, 1);

        let i1 = ConstExpr::Int(1, dummy_pos.clone());
        let i2 = ConstExpr::Int(2, dummy_pos.clone());
        let u1 = ConstExpr::Unsigned(3, dummy_pos.clone());
        let u2 = ConstExpr::Unsigned(4, dummy_pos.clone());
        let l1 = ConstExpr::LongLong(5, dummy_pos.clone());
        let l2 = ConstExpr::LongLong(6, dummy_pos.clone());
        let ul1 = ConstExpr::ULongLong(7, dummy_pos.clone());
        let ul2 = ConstExpr::ULongLong(8, dummy_pos.clone());
        let _d1 = ConstExpr::Double(9.9, dummy_pos.clone());
        let _d2 = ConstExpr::Double(10.11, dummy_pos.clone());

        assert_eq!(i1.clone() << i2.clone(), Ok(ConstExpr::Int(4, dummy_pos.clone())));
        assert_eq!(i1.clone() << u1.clone(), Ok(ConstExpr::Int(8, dummy_pos.clone())));
        assert_eq!(u1.clone() << i1.clone(), Ok(ConstExpr::Int(6, dummy_pos.clone())));
        assert_eq!(u1.clone() << u2.clone(), Ok(ConstExpr::Unsigned(48, dummy_pos.clone())));
        assert_eq!(i1.clone() << l1.clone(), Ok(ConstExpr::LongLong(32, dummy_pos.clone())));
        assert_eq!(u1.clone() << l1.clone(), Ok(ConstExpr::LongLong(96, dummy_pos.clone())));
        assert_eq!(l1.clone() << l2.clone(), Ok(ConstExpr::LongLong(320, dummy_pos.clone())));
        assert_eq!(l1.clone() << i1.clone(), Ok(ConstExpr::LongLong(10, dummy_pos.clone())));
        assert_eq!(l1.clone() << u1.clone(), Ok(ConstExpr::LongLong(40, dummy_pos.clone())));
        assert_eq!(l1.clone() << ul1.clone(), Ok(ConstExpr::ULongLong(640, dummy_pos.clone())));
        assert_eq!(ul1.clone() << l1.clone(), Ok(ConstExpr::ULongLong(224, dummy_pos.clone())));
        assert_eq!(ul1.clone() << ul2.clone(), Ok(ConstExpr::ULongLong(1792, dummy_pos.clone())));
        // assert_eq!(d1.clone() << d2.clone(), ConstExpr::Double(9.9 << 10.11));
        // assert_eq!(d2.clone() + d1.clone(), ConstExpr::Double(9.9 << 10.11));
    }

}
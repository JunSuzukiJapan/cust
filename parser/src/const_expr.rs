use std::ops::{Add, Sub, Mul, Div, Rem, Not, Neg, Shl, Shr, BitAnd, BitOr, BitXor};
use std::cmp::Ordering;
use super::ParserError;

#[derive(Debug, Clone)]
pub enum ConstExpr {
    Int(i64),
    Unsigned(u64),
    LongLong(i128),
    ULongLong(u128),
    Double(f64),
}

impl ConstExpr {
    pub fn to_usize(&self) -> Result<u64, ParserError> {
        match self {
            Self::Int(num) => Ok(*num as u64),


            _ => Err(ParserError::cannot_convert_to_usize(None, self)),
        }
    }

    pub fn as_i32_value(&self) -> i32 {
        match self {
            Self::Int(n) => *n as i32,
            Self::Unsigned(n) => *n as i32,
            Self::LongLong(n) => *n as i32,
            Self::ULongLong(n) => *n as i32,
            Self::Double(f) => *f as i32,
        }
    }

    pub fn as_u32_value(&self) -> u32 {
        match self {
            Self::Int(n) => *n as u32,
            Self::Unsigned(n) => *n as u32,
            Self::LongLong(n) => *n as u32,
            Self::ULongLong(n) => *n as u32,
            Self::Double(f) => *f as u32,
        }
    }

    pub fn as_i64_value(&self) -> i64 {
        match self {
            Self::Int(n) => *n,
            Self::Unsigned(n) => *n as i64,
            Self::LongLong(n) => *n as i64,
            Self::ULongLong(n) => *n as i64,
            Self::Double(f) => *f as i64,
        }
    }

    pub fn as_u64_value(&self) -> u64 {
        match self {
            Self::Int(n) => *n as u64,
            Self::Unsigned(n) => *n,
            Self::LongLong(n) => *n as u64,
            Self::ULongLong(n) => *n as u64,
            Self::Double(f) => *f as u64,
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            Self::Int(0) | Self::Unsigned(0) | Self::LongLong(0) | Self::ULongLong(0) => true,
            // Self::Double(num) => num == 0.0,
            _ => false,
        }
    }
}

macro_rules! op_expand {
    ($self:tt $op:tt $other:tt) => (
        match ($self, $other) {
            (Self::Int(n1), Self::Int(n2)) => ConstExpr::Int(n1 $op n2),
            (Self::Int(n1), Self::Unsigned(n2)) => ConstExpr::Int(n1 $op n2 as i64),
            (Self::Int(n1), Self::LongLong(n2)) => ConstExpr::LongLong(n1 as i128 $op n2),
            (Self::Int(n1), Self::ULongLong(n2)) => ConstExpr::LongLong(n1 as i128 $op n2 as i128),
            (Self::Int(n1), Self::Double(n2)) => ConstExpr::Double(n1 as f64 $op n2),

            (Self::Unsigned(n1), Self::Int(n2)) => ConstExpr::Int(n1 as i64 $op n2),
            (Self::Unsigned(n1), Self::Unsigned(n2)) => ConstExpr::Unsigned(n1 $op n2),
            (Self::Unsigned(n1), Self::LongLong(n2)) => ConstExpr::LongLong(n1 as i128 $op n2),
            (Self::Unsigned(n1), Self::ULongLong(n2)) => ConstExpr::ULongLong(n1 as u128 $op n2),
            (Self::Unsigned(n1), Self::Double(n2)) => ConstExpr::Double(n1 as f64 $op n2),

            (Self::LongLong(n1), Self::Int(n2)) => ConstExpr::LongLong(n1 $op n2 as i128),
            (Self::LongLong(n1), Self::Unsigned(n2)) => ConstExpr::LongLong(n1 $op n2 as i128),
            (Self::LongLong(n1), Self::LongLong(n2)) => ConstExpr::LongLong(n1 $op n2),
            (Self::LongLong(n1), Self::ULongLong(n2)) => ConstExpr::LongLong(n1 $op n2 as i128),
            (Self::LongLong(n1), Self::Double(n2)) => ConstExpr::Double(n1 as f64 $op n2),

            (Self::ULongLong(n1), Self::Int(n2)) => ConstExpr::LongLong(n1 as i128 $op n2 as i128),
            (Self::ULongLong(n1), Self::Unsigned(n2)) => ConstExpr::ULongLong(n1 $op n2 as u128),
            (Self::ULongLong(n1), Self::LongLong(n2)) => ConstExpr::LongLong(n1 as i128 $op n2),
            (Self::ULongLong(n1), Self::ULongLong(n2)) => ConstExpr::ULongLong(n1 $op n2),
            (Self::ULongLong(n1), Self::Double(n2)) => ConstExpr::Double(n1 as f64 $op n2),

            (Self::Double(n1), Self::Int(n2)) => ConstExpr::Double(n1 $op n2 as f64),
            (Self::Double(n1), Self::Unsigned(n2)) => ConstExpr::Double(n1 $op n2 as f64),
            (Self::Double(n1), Self::LongLong(n2)) => ConstExpr::Double(n1 $op n2 as f64),
            (Self::Double(n1), Self::ULongLong(n2)) => ConstExpr::Double(n1 $op n2 as f64),
            (Self::Double(n1), Self::Double(n2)) => ConstExpr::Double(n1 $op n2),
        }
    )
}

macro_rules! op_expand_without_float {
    ($self:tt $op:tt $other:tt) => (
        match ($self, $other) {
            (Self::Int(n1), Self::Int(n2)) => Ok(ConstExpr::Int(n1 $op n2)),
            (Self::Int(n1), Self::Unsigned(n2)) => Ok(ConstExpr::Int(n1 $op (n2 as i64))),
            (Self::Int(n1), Self::LongLong(n2)) => Ok(ConstExpr::LongLong((n1 as i128) $op n2)),
            (Self::Int(n1), Self::ULongLong(n2)) => Ok(ConstExpr::LongLong((n1 as i128) $op (n2 as i128))),
            (Self::Int(_n1), Self::Double(_n2)) => Err(ParserError::cannot_apply_operator_to_float(None, str_from_op!($op))),

            (Self::Unsigned(n1), Self::Int(n2)) => Ok(ConstExpr::Int((n1 as i64) $op n2)),
            (Self::Unsigned(n1), Self::Unsigned(n2)) => Ok(ConstExpr::Unsigned(n1 $op n2)),
            (Self::Unsigned(n1), Self::LongLong(n2)) => Ok(ConstExpr::LongLong((n1 as i128) $op n2)),
            (Self::Unsigned(n1), Self::ULongLong(n2)) => Ok(ConstExpr::ULongLong((n1 as u128) $op n2)),
            (Self::Unsigned(_n1), Self::Double(_n2)) => Err(ParserError::cannot_apply_operator_to_float(None, str_from_op!($op))),

            (Self::LongLong(n1), Self::Int(n2)) => Ok(ConstExpr::LongLong(n1 $op (n2 as i128))),
            (Self::LongLong(n1), Self::Unsigned(n2)) => Ok(ConstExpr::LongLong(n1 $op (n2 as i128))),
            (Self::LongLong(n1), Self::LongLong(n2)) => Ok(ConstExpr::LongLong(n1 $op n2)),
            (Self::LongLong(n1), Self::ULongLong(n2)) => Ok(ConstExpr::LongLong(n1 $op (n2 as i128))),
            (Self::LongLong(_n1), Self::Double(_n2)) => Err(ParserError::cannot_apply_operator_to_float(None, str_from_op!($op))),

            (Self::ULongLong(n1), Self::Int(n2)) => Ok(ConstExpr::LongLong((n1 as i128) $op (n2 as i128))),
            (Self::ULongLong(n1), Self::Unsigned(n2)) => Ok(ConstExpr::ULongLong(n1 $op (n2 as u128))),
            (Self::ULongLong(n1), Self::LongLong(n2)) => Ok(ConstExpr::LongLong((n1 as i128) $op n2)),
            (Self::ULongLong(n1), Self::ULongLong(n2)) => Ok(ConstExpr::ULongLong(n1 $op n2)),
            (Self::ULongLong(_n1), Self::Double(_n2)) => Err(ParserError::cannot_apply_operator_to_float(None, str_from_op!($op))),

            (Self::Double(_n1), _) =>  Err(ParserError::cannot_apply_operator_to_float(None, str_from_op!($op))),
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
            (Self::Int(n1), Self::Int(n2)) => ConstExpr::Int(n1 + n2),
            (Self::Int(n1), Self::Unsigned(n2)) => ConstExpr::Int(n1 + n2 as i64),
            (Self::Int(n1), Self::LongLong(n2)) => ConstExpr::LongLong(n1 as i128 + n2),
            (Self::Int(n1), Self::ULongLong(n2)) => ConstExpr::LongLong(n1 as i128 + n2 as i128),
            (Self::Int(n1), Self::Double(n2)) => ConstExpr::Double(n1 as f64 + n2),

            (Self::Unsigned(n1), Self::Int(n2)) => ConstExpr::Int(n1 as i64 + n2),
            (Self::Unsigned(n1), Self::Unsigned(n2)) => ConstExpr::Unsigned(n1 + n2),
            (Self::Unsigned(n1), Self::LongLong(n2)) => ConstExpr::LongLong(n1 as i128 + n2),
            (Self::Unsigned(n1), Self::ULongLong(n2)) => ConstExpr::ULongLong(n1 as u128 + n2),
            (Self::Unsigned(n1), Self::Double(n2)) => ConstExpr::Double(n1 as f64 + n2),

            (Self::LongLong(n1), Self::Int(n2)) => ConstExpr::LongLong(n1 + n2 as i128),
            (Self::LongLong(n1), Self::Unsigned(n2)) => ConstExpr::LongLong(n1 + n2 as i128),
            (Self::LongLong(n1), Self::LongLong(n2)) => ConstExpr::LongLong(n1 + n2),
            (Self::LongLong(n1), Self::ULongLong(n2)) => ConstExpr::LongLong(n1 + n2 as i128),
            (Self::LongLong(n1), Self::Double(n2)) => ConstExpr::Double(n1 as f64 + n2),

            (Self::ULongLong(n1), Self::Int(n2)) => ConstExpr::LongLong(n1 as i128 + n2 as i128),
            (Self::ULongLong(n1), Self::Unsigned(n2)) => ConstExpr::ULongLong(n1 + n2 as u128),
            (Self::ULongLong(n1), Self::LongLong(n2)) => ConstExpr::LongLong(n1 as i128 + n2),
            (Self::ULongLong(n1), Self::ULongLong(n2)) => ConstExpr::ULongLong(n1 + n2),
            (Self::ULongLong(n1), Self::Double(n2)) => ConstExpr::Double(n1 as f64 + n2),

            (Self::Double(n1), Self::Int(n2)) => ConstExpr::Double(n1 + n2 as f64),
            (Self::Double(n1), Self::Unsigned(n2)) => ConstExpr::Double(n1 + n2 as f64),
            (Self::Double(n1), Self::LongLong(n2)) => ConstExpr::Double(n1 + n2 as f64),
            (Self::Double(n1), Self::ULongLong(n2)) => ConstExpr::Double(n1 + n2 as f64),
            (Self::Double(n1), Self::Double(n2)) => ConstExpr::Double(n1 + n2),
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
            ConstExpr::Int(num) => ConstExpr::Int(-num),
            ConstExpr::Unsigned(num) => ConstExpr::LongLong(-(num as i128)),
            ConstExpr::LongLong(num) => ConstExpr::LongLong(-num),
            ConstExpr::ULongLong(num) => ConstExpr::LongLong(- (num as i128)),
            ConstExpr::Double(num) => ConstExpr::Double(-num),
        }

    }
}

impl Not for ConstExpr {
    type Output = Result<Self, ParserError>;

    fn not(self) -> Self::Output {
        match self {
            ConstExpr::Int(num) => Ok(ConstExpr::Int(!num)),
            ConstExpr::Unsigned(num) => Ok(ConstExpr::Unsigned(!num)),
            ConstExpr::LongLong(num) => Ok(ConstExpr::LongLong(!num)),
            ConstExpr::ULongLong(num) => Ok(ConstExpr::ULongLong(!num)),
            ConstExpr::Double(_num) => Err(ParserError::cannot_not_of_float(None)),
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
            (Self::Int(n1), Self::Int(n2)) => *n1 == *n2,
            (Self::Int(n1), Self::Unsigned(n2)) => *n1 == *n2 as i64,
            (Self::Int(n1), Self::LongLong(n2)) => *n1 as i128 == *n2,
            (Self::Int(n1), Self::ULongLong(n2)) => *n1 as i128 == *n2 as i128,
            (Self::Int(n1), Self::Double(n2)) => *n1 as f64 == *n2,

            (Self::Unsigned(n1), Self::Int(n2)) => *n1 as i64 == *n2,
            (Self::Unsigned(n1), Self::Unsigned(n2)) => *n1 == *n2,
            (Self::Unsigned(n1), Self::LongLong(n2)) => *n1 as i128 == *n2,
            (Self::Unsigned(n1), Self::ULongLong(n2)) => *n1 as u128 == *n2,
            (Self::Unsigned(n1), Self::Double(n2)) => *n1 as f64 == *n2,

            (Self::LongLong(n1), Self::Int(n2)) => *n1 == *n2 as i128,
            (Self::LongLong(n1), Self::Unsigned(n2)) => *n1 == *n2 as i128,
            (Self::LongLong(n1), Self::LongLong(n2)) => *n1 == *n2,
            (Self::LongLong(n1), Self::ULongLong(n2)) => *n1 == *n2 as i128,
            (Self::LongLong(n1), Self::Double(n2)) => *n1 as f64 == *n2,

            (Self::ULongLong(n1), Self::Int(n2)) => *n1 as i128 == *n2 as i128,
            (Self::ULongLong(n1), Self::Unsigned(n2)) => *n1 == *n2 as u128,
            (Self::ULongLong(n1), Self::LongLong(n2)) => *n1 as i128 == *n2,
            (Self::ULongLong(n1), Self::ULongLong(n2)) => *n1 == *n2,
            (Self::ULongLong(n1), Self::Double(n2)) => *n1 as f64 == *n2,

            (Self::Double(n1), Self::Int(n2)) => *n1 == *n2 as f64,
            (Self::Double(n1), Self::Unsigned(n2)) => *n1 == *n2 as f64,
            (Self::Double(n1), Self::LongLong(n2)) => *n1 == *n2 as f64,
            (Self::Double(n1), Self::ULongLong(n2)) => *n1 == *n2 as f64,
            (Self::Double(n1), Self::Double(n2)) => *n1 == *n2,
        }
    }
}
impl Eq for ConstExpr {}

impl PartialOrd for ConstExpr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Int(n1), Self::Int(n2)) => n1.partial_cmp(n2),
            (Self::Int(n1), Self::Unsigned(n2)) => n1.partial_cmp(&(*n2 as i64)),
            (Self::Int(n1), Self::LongLong(n2)) => (*n1 as i128).partial_cmp(n2),
            (Self::Int(n1), Self::ULongLong(n2)) => (*n1 as i128).partial_cmp(&(*n2 as i128)),
            (Self::Int(n1), Self::Double(n2)) => (*n1 as f64).partial_cmp(n2),

            (Self::Unsigned(n1), Self::Int(n2)) => (*n1 as i64).partial_cmp(n2),
            (Self::Unsigned(n1), Self::Unsigned(n2)) => n1.partial_cmp(n2),
            (Self::Unsigned(n1), Self::LongLong(n2)) => (*n1 as i128).partial_cmp(n2),
            (Self::Unsigned(n1), Self::ULongLong(n2)) => (*n1 as u128).partial_cmp(n2),
            (Self::Unsigned(n1), Self::Double(n2)) => (*n1 as f64).partial_cmp(n2),

            (Self::LongLong(n1), Self::Int(n2)) => n1.partial_cmp(&(*n2 as i128)),
            (Self::LongLong(n1), Self::Unsigned(n2)) => n1.partial_cmp(&(*n2 as i128)),
            (Self::LongLong(n1), Self::LongLong(n2)) => n1.partial_cmp(n2),
            (Self::LongLong(n1), Self::ULongLong(n2)) => n1.partial_cmp(&(*n2 as i128)),
            (Self::LongLong(n1), Self::Double(n2)) => (*n1 as f64).partial_cmp(n2),

            (Self::ULongLong(n1), Self::Int(n2)) => (*n1 as i128).partial_cmp(&(*n2 as i128)),
            (Self::ULongLong(n1), Self::Unsigned(n2)) => n1.partial_cmp(&(*n2 as u128)),
            (Self::ULongLong(n1), Self::LongLong(n2)) => (*n1 as i128).partial_cmp(n2),
            (Self::ULongLong(n1), Self::ULongLong(n2)) => n1.partial_cmp(n2),
            (Self::ULongLong(n1), Self::Double(n2)) => (*n1 as f64).partial_cmp(n2),

            (Self::Double(n1), Self::Int(n2)) => n1.partial_cmp(&(*n2 as f64)),
            (Self::Double(n1), Self::Unsigned(n2)) => n1.partial_cmp(&(*n2 as f64)),
            (Self::Double(n1), Self::LongLong(n2)) => n1.partial_cmp(&(*n2 as f64)),
            (Self::Double(n1), Self::ULongLong(n2)) => n1.partial_cmp(&(*n2 as f64)),
            (Self::Double(n1), Self::Double(n2)) => n1.partial_cmp(n2),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn const_expr_add() {
        let i1 = ConstExpr::Int(1);
        let i2 = ConstExpr::Int(2);
        let u1 = ConstExpr::Unsigned(3);
        let u2 = ConstExpr::Unsigned(4);
        let l1 = ConstExpr::LongLong(5);
        let l2 = ConstExpr::LongLong(6);
        let ul1 = ConstExpr::ULongLong(7);
        let ul2 = ConstExpr::ULongLong(8);
        let d1 = ConstExpr::Double(9.9);
        let d2 = ConstExpr::Double(10.11);

        assert_eq!(i1.clone() + i2.clone(), ConstExpr::Int(3));
        assert_eq!(i1.clone() + u1.clone(), ConstExpr::Int(4));
        assert_eq!(u1.clone() + i1.clone(), ConstExpr::Int(4));
        assert_eq!(u1.clone() + u2.clone(), ConstExpr::Unsigned(7));
        assert_eq!(i1.clone() + l1.clone(), ConstExpr::LongLong(6));
        assert_eq!(u1.clone() + l1.clone(), ConstExpr::LongLong(8));
        assert_eq!(l1.clone() + l2.clone(), ConstExpr::LongLong(11));
        assert_eq!(l1.clone() + i1.clone(), ConstExpr::LongLong(6));
        assert_eq!(l1.clone() + u1.clone(), ConstExpr::LongLong(8));
        assert_eq!(l1.clone() + ul1.clone(), ConstExpr::ULongLong(12));
        assert_eq!(ul1.clone() + l1.clone(), ConstExpr::ULongLong(12));
        assert_eq!(ul1.clone() + ul2.clone(), ConstExpr::ULongLong(15));
        assert_eq!(d1.clone() + d2.clone(), ConstExpr::Double(9.9 + 10.11));
        assert_eq!(d2.clone() + d1.clone(), ConstExpr::Double(9.9 + 10.11));
    }

    #[test]
    fn const_expr_sub() {
        let i1 = ConstExpr::Int(1);
        let i2 = ConstExpr::Int(2);
        let u1 = ConstExpr::Unsigned(3);
        let u2 = ConstExpr::Unsigned(4);
        let l1 = ConstExpr::LongLong(5);
        let l2 = ConstExpr::LongLong(6);
        let ul1 = ConstExpr::ULongLong(7);
        let _ul2 = ConstExpr::ULongLong(8);
        let d1 = ConstExpr::Double(9.9);
        let d2 = ConstExpr::Double(10.11);

        assert_eq!(i1.clone() - i2.clone(), ConstExpr::Int(-1));
        assert_eq!(i1.clone() - u1.clone(), ConstExpr::Int(-2));
        assert_eq!(u1.clone() - i1.clone(), ConstExpr::Int(2));
        assert_eq!(u2.clone() - u1.clone(), ConstExpr::Unsigned(1));
        assert_eq!(i1.clone() - l1.clone(), ConstExpr::LongLong(-4));
        assert_eq!(u1.clone() - l1.clone(), ConstExpr::LongLong(-2));
        assert_eq!(l1.clone() - l2.clone(), ConstExpr::LongLong(-1));
        assert_eq!(l1.clone() - i1.clone(), ConstExpr::LongLong(4));
        assert_eq!(l1.clone() - u1.clone(), ConstExpr::LongLong(2));
        // assert_eq!(l1.clone() - ul1.clone(), ConstExpr::ULongLong(-2));
        assert_eq!(ul1.clone() - l1.clone(), ConstExpr::ULongLong(2));
        // assert_eq!(ul1.clone() - ul2.clone(), ConstExpr::ULongLong(1));
        assert_eq!(d1.clone() - d2.clone(), ConstExpr::Double(9.9 - 10.11));
        assert_eq!(d2.clone() - d1.clone(), ConstExpr::Double(10.11 - 9.9));
    }

    #[test]
    fn const_expr_shl() {
        let i1 = ConstExpr::Int(1);
        let i2 = ConstExpr::Int(2);
        let u1 = ConstExpr::Unsigned(3);
        let u2 = ConstExpr::Unsigned(4);
        let l1 = ConstExpr::LongLong(5);
        let l2 = ConstExpr::LongLong(6);
        let ul1 = ConstExpr::ULongLong(7);
        let ul2 = ConstExpr::ULongLong(8);
        let _d1 = ConstExpr::Double(9.9);
        let _d2 = ConstExpr::Double(10.11);

        assert_eq!(i1.clone() << i2.clone(), Ok(ConstExpr::Int(4)));
        assert_eq!(i1.clone() << u1.clone(), Ok(ConstExpr::Int(8)));
        assert_eq!(u1.clone() << i1.clone(), Ok(ConstExpr::Int(6)));
        assert_eq!(u1.clone() << u2.clone(), Ok(ConstExpr::Unsigned(48)));
        assert_eq!(i1.clone() << l1.clone(), Ok(ConstExpr::LongLong(32)));
        assert_eq!(u1.clone() << l1.clone(), Ok(ConstExpr::LongLong(96)));
        assert_eq!(l1.clone() << l2.clone(), Ok(ConstExpr::LongLong(320)));
        assert_eq!(l1.clone() << i1.clone(), Ok(ConstExpr::LongLong(10)));
        assert_eq!(l1.clone() << u1.clone(), Ok(ConstExpr::LongLong(40)));
        assert_eq!(l1.clone() << ul1.clone(), Ok(ConstExpr::ULongLong(640)));
        assert_eq!(ul1.clone() << l1.clone(), Ok(ConstExpr::ULongLong(224)));
        assert_eq!(ul1.clone() << ul2.clone(), Ok(ConstExpr::ULongLong(1792)));
        // assert_eq!(d1.clone() << d2.clone(), ConstExpr::Double(9.9 << 10.11));
        // assert_eq!(d2.clone() + d1.clone(), ConstExpr::Double(9.9 << 10.11));
    }

}
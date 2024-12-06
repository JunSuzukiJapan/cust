use std::rc::Rc;

use crate::{ExprAST, Type, Position};

#[derive(Debug, Clone, PartialEq)]
pub enum Initializer {
    Simple(ExprAST, Position),
    Array(Vec<Box<Initializer>>, Rc<Type>, Position),
    Struct(Vec<Box<Initializer>>, Rc<Type>, Position),
}

impl Initializer {
    pub fn get_position(&self) -> &Position {
        match self {
            Self::Array(_, _type, pos) => pos,
            Self::Struct(_, _type,  pos) => pos,
            Self::Simple(_, pos) => pos,
        }
    }
}
use std::rc::Rc;

use crate::{ExprAST, ConstExpr, Type, Position};

#[derive(Debug, Clone, PartialEq)]
pub enum Initializer {
    Simple(ExprAST, Position),
    Array(Vec<Box<ConstInitializer>>, Rc<Type>, Position),
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

    pub fn try_to_const_initializer(&self) -> Option<ConstInitializer> {
        match self {
            Self::Array(vec, typ, pos) => {
                Some(ConstInitializer::Array(vec.clone(), Rc::clone(typ), pos.clone()))
            },
            Self::Simple(expr, pos) => {
                let const_expr = ConstExpr::try_from_expr(expr)?;
                Some(ConstInitializer::Simple(const_expr, pos.clone()))
            },
            Self::Struct(exprs, typ, pos) => {
                let mut list: Vec<Box<ConstInitializer>> = Vec::new();
                for expr in exprs {
                    let e = expr.try_to_const_initializer()?;
                    list.push(Box::new(e));
                }
                Some(ConstInitializer::Struct(list, Rc::clone(typ), pos.clone()))
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstInitializer {
    Simple(ConstExpr, Position),
    Array(Vec<Box<ConstInitializer>>, Rc<Type>, Position),
    Struct(Vec<Box<ConstInitializer>>, Rc<Type>, Position),
}

impl ConstInitializer {
    pub fn try_from_initializer(init: &Initializer) -> Option<Self> {
        match init {
            Initializer::Simple(expr, pos) => {
                if let Some(const_expr) = ConstExpr::try_from_expr(expr) {
                    Some(ConstInitializer::Simple(const_expr, pos.clone()))
                }else{
                    None
                }
            },
            Initializer::Array(vec, typ, pos) => {
                Some(ConstInitializer::Array(vec.clone(), Rc::clone(typ), pos.clone()))
            },
            Initializer::Struct(vec, typ, pos) => {
                let mut list = Vec::new();

                for init in vec {
                    if let Some(e) = Self::try_from_initializer(init) {
                        list.push(Box::new(e));
                    }else{
                        return None;
                    }
                }

                Some(ConstInitializer::Struct(list, Rc::clone(typ), pos.clone()))
            },
        }
    }

    pub fn get_position(&self) -> &Position {
        match self {
            Self::Array(_, _type, pos) => pos,
            Self::Struct(_, _type,  pos) => pos,
            Self::Simple(_, pos) => pos,
        }
    }
}
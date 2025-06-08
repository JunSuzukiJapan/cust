use std::{rc::Rc};

use crate::{ConstExpr, Defines, ExprAST, Position, Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Initializer {
    Simple(ExprAST, Position),
    Array(Vec<Box<ArrayInitializer>>, Rc<Type>, Position),
    Struct(Vec<Box<Initializer>>, Rc<Type>, Position),
    Tuple(Vec<Box<Initializer>>, Rc<Type>, Position),
}

impl Initializer {
    pub fn get_position(&self) -> &Position {
        match self {
            Self::Simple(_, pos) => pos,
            Self::Array(_, _type, pos) => pos,
            Self::Struct(_, _type,  pos) => pos,
            Self::Tuple(_, _type, pos) => pos,
        }
    }

    pub fn try_to_const_initializer(&self, defs: &Defines) -> Option<ConstInitializer> {
        match self {
            Self::Array(vec, typ, pos) => {
                Some(ConstInitializer::Array(vec.clone(), Rc::clone(typ), pos.clone()))
            },
            Self::Simple(expr, pos) => {
                // let const_expr = ConstExpr::try_from_expr(expr)?;
                if let Ok(const_expr) = expr.to_const(defs, pos) {
                    Some(ConstInitializer::Simple(const_expr, pos.clone()))
                }else{
                    None
                }
            },
            Self::Struct(exprs, typ, pos) => {
                let mut list: Vec<Box<ConstInitializer>> = Vec::new();
                for expr in exprs {
                    let e = expr.try_to_const_initializer(defs)?;
                    list.push(Box::new(e));
                }
                Some(ConstInitializer::Struct(list, Rc::clone(typ), pos.clone()))
            },
            Self::Tuple(exprs, typ, pos) => {
                let mut list: Vec<Box<ConstInitializer>> = Vec::new();
                for expr in exprs {
                    let e = expr.try_to_const_initializer(defs)?;
                    list.push(Box::new(e));
                }
                Some(ConstInitializer::Tuple(list, Rc::clone(typ), pos.clone()))
            },
        }
    }

    pub fn try_to_array_initializer(&self, defs: &Defines) -> Option<ArrayInitializer> {
        match self {
            Self::Array(vec, typ, pos) => {
                Some(ArrayInitializer::Array(vec.clone(), Rc::clone(typ), pos.clone()))
            },
            Self::Simple(expr, pos) => {
                if let Ok(const_expr) = expr.to_const(defs, pos) {
                    let init = ConstInitializer::Simple(const_expr, pos.clone());
                    Some(ArrayInitializer::Const(init, pos.clone()))
                }else{
                    None
                }
            },
            Self::Struct(exprs, typ, pos) => {
                let mut list: Vec<Box<ConstInitializer>> = Vec::new();
                for expr in exprs {
                    let e = expr.try_to_const_initializer(defs)?;
                    list.push(Box::new(e));
                }
                let init = ConstInitializer::Struct(list, Rc::clone(typ), pos.clone());
                Some(ArrayInitializer::Const(init, pos.clone()))
            },
            Self::Tuple(exprs, typ, pos) => {
                let mut list: Vec<Box<ConstInitializer>> = Vec::new();
                for expr in exprs {
                    let e = expr.try_to_const_initializer(defs)?;
                    list.push(Box::new(e));
                }
                let init = ConstInitializer::Tuple(list, Rc::clone(typ), pos.clone());
                Some(ArrayInitializer::Const(init, pos.clone()))
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstInitializer {
    Simple(ConstExpr, Position),
    Array(Vec<Box<ArrayInitializer>>, Rc<Type>, Position),
    Struct(Vec<Box<ConstInitializer>>, Rc<Type>, Position),
    Tuple(Vec<Box<ConstInitializer>>, Rc<Type>, Position),
}

impl ConstInitializer {
    pub fn try_from_initializer(init: &Initializer, defs: &Defines) -> Option<Self> {
        match init {
            Initializer::Simple(expr, pos) => {
                if let Ok(const_expr) = expr.to_const(defs, pos) {
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
                    if let Some(e) = Self::try_from_initializer(init, defs) {
                        list.push(Box::new(e));
                    }else{
                        return None;
                    }
                }

                Some(ConstInitializer::Struct(list, Rc::clone(typ), pos.clone()))
            },
            Initializer::Tuple(vec, typ, pos) => {
                let mut list = Vec::new();

                for init in vec {
                    if let Some(e) = Self::try_from_initializer(init, defs) {
                        list.push(Box::new(e));
                    }else{
                        return None;
                    }
                }

                Some(ConstInitializer::Tuple(list, Rc::clone(typ), pos.clone()))
            },
        }
    }

    pub fn get_position(&self) -> &Position {
        match self {
            Self::Simple(_, pos) => pos,
            Self::Array(_, _type, pos) => pos,
            Self::Struct(_, _type,  pos) => pos,
            Self::Tuple(_, _type, pos) => pos,
        }
    }

    pub fn get_type(&self) -> Rc<Type> {
        match self {
            Self::Simple(expr, _pos) => Rc::new(expr.get_type()),
            Self::Array(_, typ, _pos) => Rc::clone(typ),
            Self::Struct(_, typ, _pos) => Rc::clone(typ),
            Self::Tuple(_, typ, _pos) => Rc::clone(typ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrayInitializer {
    Const(ConstInitializer, Position),
    Array(Vec<Box<ArrayInitializer>>, Rc<Type>, Position),
}

impl ArrayInitializer {
    pub fn try_from_initializer(init: &Initializer, defs: &Defines) -> Option<Self> {
        match init {
            Initializer::Simple(expr, pos) => {
                if let Ok(const_expr) = expr.to_const(defs, pos) {
                    let init = ConstInitializer::Simple(const_expr, pos.clone());
                    Some(ArrayInitializer::Const(init, pos.clone()))
                }else{
                    None
                }
            },
            Initializer::Array(vec, typ, pos) => {
                Some(ArrayInitializer::Array(vec.clone(), Rc::clone(typ), pos.clone()))
            },
            Initializer::Struct(vec, typ, pos) => {
                let mut list: Vec<Box<ConstInitializer>> = Vec::new();

                for init in vec {
                    if let Some(e) = ConstInitializer::try_from_initializer(init, defs) {
                        list.push(Box::new(e));
                    }else{
                        return None;
                    }
                }

                let init = ConstInitializer::Struct(list, Rc::clone(typ), pos.clone());
                Some(ArrayInitializer::Const(init, pos.clone()))
            },
            Initializer::Tuple(vec, typ, pos) => {
                let mut list: Vec<Box<ConstInitializer>> = Vec::new();

                for init in vec {
                    if let Some(e) = ConstInitializer::try_from_initializer(init, defs) {
                        list.push(Box::new(e));
                    }else{
                        return None;
                    }
                }

                let init = ConstInitializer::Tuple(list, Rc::clone(typ), pos.clone());
                Some(ArrayInitializer::Const(init, pos.clone()))
            },
        }
    }

    pub fn get_const(&self) -> Option<&ConstInitializer> {
        match self {
            Self::Const(value, _pos) => Some(value),
            _ => None,
        }
    }

    pub fn get_array_list(&self) -> Option<&Vec<Box<ArrayInitializer>>> {
        match self {
            Self::Array(list, _, _pos) => Some(list),
            _ => None,
        }
    }

    pub fn get_position(&self) -> &Position {
        match self {
            Self::Const(_, pos) => pos,
            Self::Array(_, _, pos) => pos,
        }
    }
}
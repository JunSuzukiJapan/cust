use std::collections::HashMap;
use std::rc::Rc;

use tokenizer::Position;
use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Var(String, Option<String>, Position),
    Str(String, Option<String>, Position),
    Char(char, Option<String>, Position),
    CharRange(char, char, Option<String>, Position),
    Number(i128, Option<String>, Position),
    NumberRange(i128, i128, Option<String>, Position),
    Enum(EnumPattern, Option<String>, Position),
    Tuple(Vec<Vec<Box<Pattern>>>, Option<String>, Position),
    Struct(StructPattern, Option<String>, Position),
    OrList(Vec<Box<Pattern>>, Option<String>, Position),
}

impl Pattern {
    pub fn get_position(&self) -> Option<&Position> {
        match self {
            Pattern::Var(_, _, pos) => Some(pos),
            Pattern::Str(_, _, pos) => Some(pos),
            Pattern::Char(_, _, pos) => Some(pos),
            Pattern::CharRange(_, _, _, pos) => Some(pos),
            Pattern::Number(_, _, pos) => Some(pos),
            Pattern::NumberRange(_, _, _, pos) => Some(pos),
            Pattern::Enum(_, _, pos) => Some(pos),
            Pattern::Tuple(_, _, pos) => Some(pos),
            Pattern::Struct(_, _, pos) => Some(pos),
            Pattern::OrList(_, _, pos) => Some(pos),
        }
    }

    pub fn get_at_name(&self) -> &Option<String> {
        match self {
            Pattern::Var(_, name, _) => name,
            Pattern::Str(_, name, _) => name,
            Pattern::Char(_, name, _) => name,
            Pattern::CharRange(_, _, name, _) => name,
            Pattern::Number(_, name, _) => name,
            Pattern::NumberRange(_, _, name, _) => name,
            Pattern::Enum(_, name, _) => name,
            Pattern::Tuple(_, name, _) => name,
            Pattern::Struct(_, name, _) => name,
            Pattern::OrList(_, name, _) => name,
        }
    }

    pub fn set_at_name(&mut self, name: Option<String>) {
        match self {
            Pattern::Var(_, at_name, _) => *at_name = name,
            Pattern::Str(_, at_name, _) => *at_name = name,
            Pattern::Char(_, at_name, _) => *at_name = name,
            Pattern::CharRange(_, _, at_name, _) => *at_name = name,
            Pattern::Number(_, at_name, _) => *at_name = name,
            Pattern::NumberRange(_, _, at_name, _) => *at_name = name,
            Pattern::Enum(_, at_name, _) => *at_name = name,
            Pattern::Tuple(_, at_name, _) => *at_name = name,
            Pattern::Struct(_, at_name, _) => *at_name = name,
            Pattern::OrList(_, at_name, _) => *at_name = name,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumPattern {
    // Name::SubName
    Simple(Rc<Type>, String, String),
    // Name::SubName(pattern1 @ pat_name, pattern2, ...)
    Tuple(Rc<Type>, String, String, Vec<Vec<Box<Pattern>>>),
    // Name::SubName { field1: struct_pattern1, field2: struct_pattern2, ... }
    Struct(Rc<Type>, String, String, StructPattern),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructPattern {
    pub name: String,
    pub keys: Vec<String>,
    // pub map: HashMap<String, Option<(Vec<(Box<Pattern>, Position)>, Option<String>)>>,
    pub map: HashMap<String, Option<Vec<Box<Pattern>>>>,
    pub has_optional: bool,
}

impl StructPattern {
    pub fn get_name(&self) -> &String {
        &self.name
    }

    pub fn get_keys(&self) -> &Vec<String> {
        &self.keys
    }

    pub fn get_map(&self) -> &HashMap<String, Option<Vec<Box<Pattern>>>> {
        &self.map
    }

    pub fn get_has_optional(&self) -> bool {
        self.has_optional
    }
}

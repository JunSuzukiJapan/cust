use std::collections::HashMap;
use std::rc::Rc;

use tokenizer::Position;
use crate::types::Type;

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Var(String),
    Tuple(Vec<(Vec<(Box<Pattern>, Position)>, Option<String>)>),
    Str(String),
    Char(char),
    CharRange(char, char),
    Number(i128),
    NumberRange(i128, i128),
    Enum(EnumPattern),
    Struct(StructPattern),
}

#[derive(Debug, PartialEq, Clone)]
pub enum EnumPattern {
    // Name::SubName
    Simple(Rc<Type>, String, String),
    // Name::SubName(pattern1 @ pat_name, pattern2, ...)
    Tuple(Rc<Type>, String, String, Vec<(Vec<(Box<Pattern>, Position)>, Option<String>)>),
    // Name::SubName { field1: struct_pattern1, field2: struct_pattern2, ... }
    Struct(Rc<Type>, String, String, StructPattern),
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructPattern {
    pub name: String,
    pub keys: Vec<String>,
    pub map: HashMap<String, Option<(Vec<(Box<Pattern>, Position)>, Option<String>)>>,
    pub has_optional: bool,
}

impl StructPattern {
    pub fn get_name(&self) -> &String {
        &self.name
    }

    pub fn get_keys(&self) -> &Vec<String> {
        &self.keys
    }

    pub fn get_map(&self) -> &HashMap<String, Option<(Vec<(Box<Pattern>, Position)>, Option<String>)>> {
        &self.map
    }

    pub fn get_has_optional(&self) -> bool {
        self.has_optional
    }
}
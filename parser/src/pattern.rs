use std::collections::HashMap;
use tokenizer::Position;

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
    Simple(String, String),
    Tuple(String, String, Vec<(Vec<(Box<Pattern>, Position)>, Option<String>)>),
    Struct(String, String, StructPattern),
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructPattern {
    pub name: String,
    pub map: HashMap<String, Option<Vec<(Box<Pattern>, Position)>>>,
    pub has_optional: bool,
}
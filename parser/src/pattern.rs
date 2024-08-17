#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Var(String),
    Tuple(Vec<(Vec<Box<Pattern>>, Option<String>)>),
    Str(String),
    Char(char),
    CharRange(char, char),
    Number(i128),
    NumberRange(i128, i128),
    Enum(EnumPattern),
    // Struct(StructDefinition),
}

#[derive(Debug, PartialEq, Clone)]
pub enum EnumPattern {
    Simple(String, String),
    Tuple(String, String, Vec<(Vec<Box<Pattern>>, Option<String>)>),
    Struct(String, String),
}
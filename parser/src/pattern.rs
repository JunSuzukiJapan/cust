#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Var(String),
    Tuple(Vec<(Vec<Box<Pattern>>, Option<String>)>),
    // Struct(StructDefinition),
    // Enum(EnumDefinition),
    Str(String),
    Char(char),
    CharRange(char, char),
    Number(i128),
    NumberRange(i128, i128),
}
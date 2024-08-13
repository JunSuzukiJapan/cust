#[derive(Debug, PartialEq)]
pub enum Pattern {
    Var(String),
    Tuple(Vec<Box<Pattern>>),
    // Struct(StructDefinition),
    // Enum(EnumDefinition),
    Str(String),
    Char(char),
    CharRange(char, char),
    Number(i128),
    NumberRange(i128, i128),
}
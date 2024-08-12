#[derive(Debug, PartialEq)]
pub enum Pattern {
    Var(String),
    Tuple(Vec<Box<Pattern>>),
    // Struct(StructDefinition),
    // Enum(EnumDefinition),
    Str(String),
    Char(u32),
    CharRange(u32, u32),
    Number(i128),
    NumberRange(i128, i128),
}
#[derive(Debug)]
pub enum Pattern {
    Var(String),
    Tuple(Vec<Box<Pattern>>),
    // Struct(StructDefinition),
    // Enum(EnumDefinition),
    Str(String),
    Char(u32),
    Number(i128),
    NumberRange(i128, i128),
    // CharRange(Char, Char),
}
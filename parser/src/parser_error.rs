pub enum ParserError {
    NotPointer,
}

impl ParserError {
    pub fn not_pointer() -> ParserError {
        ParserError::NotPointer
    }
}
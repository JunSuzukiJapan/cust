use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct Location {
    pub line: u64,
    pub column: u64,
}

impl Location {
    pub fn new() -> Self {
        Location {
            line: 1,
            column: 1,
        }
    }

    pub fn inc(&mut self) {
        self.column += 1;
    }

    pub fn inc_line(&mut self) {
        self.line += 1;
    }

    pub fn get_line(&self) -> u64 {
        self.line
    }

    pub fn get_column(&self) -> u64 {
        self.column
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(line: {}, column: {})", self.line, self.column)
    }
}
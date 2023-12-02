use super::Location;

pub enum TokenizerError {
    IllegalEndOfInput(Option<Location>),
    SyntaxError(Option<Location>, &'static str, u32, u32),
    IllegalCharWhileParsingDigit(Option<Location>),
    IllegalCharWhileParsingChar(Option<Location>),
    IllegalCharWhileParsingCharAfter(Option<Location>, char),
    IllegalEndOfInputWhileParsingChar(Option<Location>),
    IllegalEndOfInputWhileParsingCharAfter(Option<Location>, char),
    IllegalCharWhileParsingString(Option<Location>),
    IllegalEndOfInputWhileParsingString(Option<Location>),


}
#![allow(dead_code)]

use std::fmt;
use super::Location;

#[derive(Debug, Clone)]
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

impl TokenizerError {
    pub fn get_location(&self) -> &Option<Location> {
        match self {
            Self::IllegalEndOfInput(opt_loc) => opt_loc,
            Self::SyntaxError(opt_loc, _filename, _line, _column) => opt_loc,
            Self::IllegalCharWhileParsingDigit(opt_loc) => opt_loc,
            Self::IllegalCharWhileParsingChar(opt_loc) => opt_loc,
            Self::IllegalCharWhileParsingCharAfter(opt_loc, _char) => opt_loc,
            Self::IllegalEndOfInputWhileParsingChar(opt_loc) => opt_loc,
            Self::IllegalEndOfInputWhileParsingCharAfter(opt_loc, _char) => opt_loc,
            Self::IllegalCharWhileParsingString(opt_loc) => opt_loc,
            Self::IllegalEndOfInputWhileParsingString(opt_loc) => opt_loc,
        }
    }
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::IllegalEndOfInput(_opt_loc) => write!(f, "illegal end of input."),
            Self::SyntaxError(_opt_loc, _filename, _line, _column) => write!(f, "syntax error."),
            Self::IllegalCharWhileParsingDigit(_opt_loc) => write!(f, "illegal char while parsing digit."),
            Self::IllegalCharWhileParsingChar(_opt_loc) => write!(f, "illegal char while parsing char."),
            Self::IllegalCharWhileParsingCharAfter(_opt_loc, char) => write!(f, "illegal char while parsing char after '{}'", char),
            Self::IllegalEndOfInputWhileParsingChar(_opt_loc) => write!(f, "illegal end of input while parsing char."),
            Self::IllegalEndOfInputWhileParsingCharAfter(_opt_loc, char) => write!(f, "illegal end of input while parsing char after '{}'", char),
            Self::IllegalCharWhileParsingString(_opt_loc) => write!(f, "illegal char while parsing string."),
            Self::IllegalEndOfInputWhileParsingString(_opt_loc) => write!(f, "illegal end of input while parsing string."),
        }
    }
}
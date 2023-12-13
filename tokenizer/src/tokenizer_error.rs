#![allow(dead_code)]

use std::fmt;
use super::Position;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenizerError {
    IllegalEndOfInput(Option<Position>),
    SyntaxError(Option<Position>),
    IllegalCharWhileParsingDigit(Option<Position>),
    IllegalCharWhileParsingChar(Option<Position>),
    IllegalCharWhileParsingCharAfter(Option<Position>, char),
    IllegalEndOfInputWhileParsingChar(Option<Position>),
    IllegalEndOfInputWhileParsingCharAfter(Option<Position>, char),
    IllegalCharWhileParsingString(Option<Position>),
    IllegalEndOfInputWhileParsingString(Option<Position>),
}

impl TokenizerError {
    pub fn illegal_end_of_input(opt_loc: Option<Position>) -> TokenizerError {
        TokenizerError::IllegalEndOfInput(opt_loc)
    }

    pub fn syntax_error(opt_loc: Option<Position>) -> TokenizerError {
        TokenizerError::SyntaxError(opt_loc)
    }

    pub fn get_location(&self) -> &Option<Position> {
        match self {
            Self::IllegalEndOfInput(opt_loc) => opt_loc,
            Self::SyntaxError(opt_loc) => opt_loc,
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
            Self::SyntaxError(_opt_loc) => write!(f, "syntax error."),
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
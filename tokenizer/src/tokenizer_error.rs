#![allow(dead_code)]

use std::fmt;
use super::Position;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenizerError {
    IllegalEndOfInput(Position),
    SyntaxError(Position),
    IllegalCharWhileParsingDigit(Position),
    IllegalCharWhileParsingChar(Position),
    IllegalCharWhileParsingCharAfter(Position, char),
    IllegalEndOfInputWhileParsingChar(Position),
    IllegalEndOfInputWhileParsingCharAfter(Position, char),
    IllegalCharWhileParsingString(Position),
    IllegalEndOfInputWhileParsingString(Position),
    NotOct(char, Position),
    NotHex(char, Position),
}

impl TokenizerError {
    pub fn illegal_end_of_input(pos: Position) -> TokenizerError {
        TokenizerError::IllegalEndOfInput(pos)
    }

    pub fn syntax_error(pos: Position) -> TokenizerError {
        TokenizerError::SyntaxError(pos)
    }

    pub fn get_location(&self) -> &Position {
        match self {
            Self::IllegalEndOfInput(pos) => pos,
            Self::SyntaxError(pos) => pos,
            Self::IllegalCharWhileParsingDigit(pos) => pos,
            Self::IllegalCharWhileParsingChar(pos) => pos,
            Self::IllegalCharWhileParsingCharAfter(pos, _char) => pos,
            Self::IllegalEndOfInputWhileParsingChar(pos) => pos,
            Self::IllegalEndOfInputWhileParsingCharAfter(pos, _char) => pos,
            Self::IllegalCharWhileParsingString(pos) => pos,
            Self::IllegalEndOfInputWhileParsingString(pos) => pos,
            Self::NotOct(_, pos) => pos,
            Self::NotHex(_, pos) => pos,
        }
    }
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::IllegalEndOfInput(_pos) => write!(f, "illegal end of input."),
            Self::SyntaxError(_pos) => write!(f, "syntax error."),
            Self::IllegalCharWhileParsingDigit(_pos) => write!(f, "illegal char while parsing digit."),
            Self::IllegalCharWhileParsingChar(_pos) => write!(f, "illegal char while parsing char."),
            Self::IllegalCharWhileParsingCharAfter(_pos, char) => write!(f, "illegal char while parsing char after '{}'", char),
            Self::IllegalEndOfInputWhileParsingChar(_pos) => write!(f, "illegal end of input while parsing char."),
            Self::IllegalEndOfInputWhileParsingCharAfter(_pos, char) => write!(f, "illegal end of input while parsing char after '{}'", char),
            Self::IllegalCharWhileParsingString(_pos) => write!(f, "illegal char while parsing string."),
            Self::IllegalEndOfInputWhileParsingString(_pos) => write!(f, "illegal end of input while parsing string."),
            Self::NotOct(ch, _pos) => write!(f, "{ch} is not octal number"),
            Self::NotHex(ch, _pos) => write!(f, "{ch} is not hex number"),
        }
    }
}
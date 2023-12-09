use tokenizer::TokenType;

use crate::Location;

#[derive(Debug, Clone)]
pub enum ParserError {
    NotPointer,
    SyntaxError{opt_loc: Option<Location>, filename: &'static str, line: u32, column: u32},
    IllegalEndOfInput(Option<Location>),
    WithoutExpectedToken{opt_loc: Option<Location>, expected: TokenType, real: TokenType},
    NoSuchAOperator{opt_loc: Option<Location>, token_type: TokenType},
    NeedExpr(Option<Location>),
}

impl ParserError {
    pub fn not_pointer() -> ParserError {
        ParserError::NotPointer
    }

    pub fn syntax_error(opt_loc: Option<Location>, filename: &'static str, line: u32, column: u32) -> ParserError {
        ParserError::SyntaxError { opt_loc, filename, line, column }
    }

    pub fn illegal_end_of_input(opt_loc: Option<Location>) -> ParserError {
        ParserError::IllegalEndOfInput(opt_loc)
    }

    pub fn without_expected_token(opt_loc: Option<Location>, expected: TokenType, real: TokenType) -> ParserError {
        ParserError::WithoutExpectedToken{opt_loc, expected, real}
    }

    pub fn no_such_a_operator(opt_loc: Option<Location>, token_type: TokenType) -> ParserError {
        ParserError::NoSuchAOperator { opt_loc, token_type }
    }

    pub fn need_expr(opt_loc: Option<Location>) -> ParserError {
        ParserError::NeedExpr(opt_loc)
    }
}
use super::token_type::TokenType;
use crate::Location;

pub struct Token {
    token_type: TokenType,
    location: Location,
}

impl Token {
    pub fn new(token_type: TokenType, location: Location) -> Token {
        Token { token_type, location }
    }
}
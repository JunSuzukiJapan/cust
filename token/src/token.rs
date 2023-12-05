#![allow(dead_code)]

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

    #[inline]
    pub fn get_type(&self) -> &TokenType {
        &self.token_type
    }

    #[inline]
    pub fn get_location(&self) -> &Location {
        &self.location
    }
}
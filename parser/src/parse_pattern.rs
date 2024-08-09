use crate::{ExprAST, Pattern};
use super::{Position, Token};
use super::ParserError;
use super::parse::Parser;
use super::defines::Defines;

use std::slice::Iter;
use std::iter::Peekable;


impl Parser {
    pub fn parse_pattern(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(Pattern, Position), ParserError> {
        let (tok, pos) = iter.peek().unwrap();
        iter.next();

        match &*tok {
            Token::CharLiteral(ch) => {
                let pat = Pattern::Char(*ch);

                let (tok2, pos2) = iter.peek().unwrap();
                if *tok2 == Token::RangeEqual {
                    iter.next();

                    let (tok3, pos3) = iter.peek().unwrap();
                    match tok3 {
                        Token::CharLiteral(ch2) => {
                            iter.next();

                            let pat2 = Pattern::CharRange(*ch, *ch2);
                            return Ok((pat2, pos.clone()));
                        },
                        _ => return Err(ParserError::syntax_error(pos3.clone())),
                    }
                }

                Ok((pat, pos.clone()))
            },
            Token::IntLiteral(num) => {
                let pat = Pattern::Number(*num);

                let (tok2, pos2) = iter.peek().unwrap();
                if *tok2 == Token::RangeEqual {
                    iter.next();

                    let (tok3, pos3) = iter.peek().unwrap();
                    match tok3 {
                        Token::IntLiteral(num2) => {
                            iter.next();

                            let pat2 = Pattern::NumberRange(*num, *num2);
                            return Ok((pat2, pos.clone()));
                        },
                        _ => return Err(ParserError::syntax_error(pos3.clone())),
                    }
                }

                Ok((pat, pos.clone()))
            },
            Token::StringLiteral(s) => {
                let pat = Pattern::Str(s.to_string());
                Ok((pat, pos.clone()))
            },
            // TODO: enumeration-constant




            Token::EndOfInput => Err(ParserError::illegal_end_of_input(pos.clone())),
            _ => Err(ParserError::not_pattern(pos.clone())),
        }

    }
}
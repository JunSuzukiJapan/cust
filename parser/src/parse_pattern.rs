use crate::{ExprAST, Pattern};
use super::{Position, Token};
use super::ParserError;
use super::parse::Parser;
use super::defines::Defines;

use std::slice::Iter;
use std::iter::Peekable;


impl Parser {
    pub fn parse_pattern(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Vec<(Pattern, Position)>, ParserError> {
        let mut v = Vec::new();

        loop {
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
                                v.push((pat2, pos.clone()));
                            },
                            _ => return Err(ParserError::syntax_error(pos3.clone())),
                        }
                    }

                    v.push((pat, pos.clone()));
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
                                v.push((pat2, pos.clone()));
                            },
                            _ => return Err(ParserError::syntax_error(pos3.clone())),
                        }
                    }

                    v.push((pat, pos.clone()))
                },
                Token::StringLiteral(s) => {
                    let pat = Pattern::Str(s.to_string());
                    v.push((pat, pos.clone()))
                },
                Token::ParenLeft => {
                    // parse tuple pattern


                    unimplemented!()
                },
                Token::Symbol(name) => {
                    let pat = Pattern::Var(name.to_string());

                    let (tok2, pos2) = iter.peek().unwrap();
                    match tok2 {
                        Token::WColon => {  // parse Enum pattern
                            iter.next();




                            unimplemented!()
                        },
                        Token::BraceLeft => {  // parse struct pattern
                            iter.next();




                            unimplemented!()
                        },
                        _ => (),  // do nothing
                    }

                    v.push((pat, pos.clone()))
                },
                Token::EndOfInput => return Err(ParserError::illegal_end_of_input(pos.clone())),
                _ => return Err(ParserError::not_pattern(pos.clone())),
            }


            // '|' check
            unimplemented!()

        }

        Ok(v)
    }
}
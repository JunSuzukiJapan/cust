#![allow(dead_code)]

use super::{Token, TokenType};
use super::{AST, ExprAST};
use super::Defines;
use super::ParserError;
use std::iter::Peekable;
use std::slice::Iter;

pub struct Parser;

impl Parser {
    pub fn new() -> Self {
        Parser
    }

    fn parse(self, token_list: Vec<Token>) -> Result<Vec<AST>, ParserError> {
        let mut iter = token_list.iter().peekable();
        let mut defs = Defines::new();

        self.parse_translation_unit(&mut iter, &mut defs)
    }

    fn parse_translation_unit(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines) -> Result<Vec<AST>, ParserError> {
        let mut declarations = Vec::new();
        // let mut labels = Vec::new();

        // while let Some(_) = iter.peek() {
        //     if let Some(mut decl) = self.parse_external_declaration(iter, defs, &mut Some(&mut labels))? {
        //         match decl {
        //             AST::DefVar {specifiers, declarations: declaration} => {
        //                 decl = AST::GlobalDefVar{specifiers, declaration};
        //             },
        //             _ => {
        //             },
        //         }
        //         declarations.push(decl);
        //     }
        // }

        Ok(declarations)
    }

    fn parse_constant(&self, iter: &mut Peekable<Iter<Token>>, _defs: &mut Defines) -> Result<Option<ExprAST>, ParserError> {
        if let Some(tok) = iter.peek() {

            match &*tok.get_type() {
                TokenType::CharLiteral(ch) => {
                    iter.next();
                    Ok(Some(ExprAST::Char(*ch)))
                },
                // TokenType::ShortLiteral(num) => {
                //     iter.next();
                //     Ok(Some(AST::Short(*num)))
                // },
                TokenType::IntLiteral(num) => {
                    iter.next();
                    Ok(Some(ExprAST::Int(*num)))
                },
                // TokenType::LongLiteral(num) => {
                //     iter.next();
                //     Ok(Some(ExprAST::Long(*num)))
                // },
                // TokenType::LongLongLiteral(num) => {
                //     iter.next();
                //     Ok(Some(ExprAST::LongLong(*num)))
                // },
                // TokenType::UCharLiteral(ch) => {
                //     iter.next();
                //     Ok(Some(ExprAST::UChar(*ch)))
                // },
                // TokenType::UShortLiteral(num) => {
                //     iter.next();
                //     Ok(Some(ExprAST::UShort(*num)))
                // },
                // TokenType::UIntLiteral(num) => {
                //     iter.next();
                //     Ok(Some(ExprAST::UInt(*num)))
                // },
                // TokenType::ULongLiteral(num) => {
                //     iter.next();
                //     Ok(Some(ExprAST::ULong(*num)))
                // },
                // TokenType::ULongLongLiteral(num) => {
                //     iter.next();
                //     Ok(Some(ExprAST::ULongLong(*num)))
                // },
                // TokenType::FloatLiteral(num) => {
                //     iter.next();
                //     Ok(Some(ExprAST::Float(*num)))
                // },
                TokenType::DoubleLiteral(num) => {
                    iter.next();
                    Ok(Some(ExprAST::Double(*num)))
                },
                TokenType::StringLiteral(s) => {
                    iter.next();
                    Ok(Some(ExprAST::StringLiteral(s.clone())))
                },
                // TODO: enumeration-constant


                _ => Ok(None),
            }
        }else{
            Ok(None)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::Tokenizer;

    fn parse_constant_from_str(src: &str) -> Result<Option<ExprAST>, ParserError> {
        let tokenizer = Tokenizer::new();
        let token_list = tokenizer.tokenize(src).unwrap();
        let mut iter = token_list.iter().peekable();
        let parser = Parser::new();
        let mut defs = Defines::new();
        parser.parse_constant(&mut iter, &mut defs)
    }

    #[test]
    fn parse_constant_int() {
        let src = "123";
        let ast = parse_constant_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::Int(123)
        );
    }
}
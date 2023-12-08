#![allow(dead_code)]

use super::{Token, TokenType};
use super::{AST, ExprAST, BinOp};
use super::Defines;
use super::ParserError;
use crate::Location;
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










    pub fn parse_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_assignment_expression(iter, defs, labels)? {
            if let Some(tok) = iter.peek() {
                let token_type = tok.get_type();
                match token_type {
                    TokenType::Comma => {
                        if let Some(code) = self.parse_expression_sub(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }

            Ok(Some(ast))
        }else{  // None
            Ok(None)
        }
    }

    fn parse_expression_sub<'a>(&'a self, iter: &mut Peekable<Iter<Token>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some(tok) = iter.peek() {
                let typ = tok.get_type();
                match typ {
                    TokenType::Comma => {
                        iter.next(); // skip ','

                        if let Some(right) = self.parse_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::Comma, Box::new(left), Box::new(right)));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::Comma, Box::new(ast.clone()), Box::new(right)));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_primary_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {

        if let Some(tok) = iter.peek() {
            match &*tok.get_type() {
                TokenType::Symbol(name) => {
                    iter.next();  // skip symbol
                    Ok(Some(ExprAST::Symbol(name.clone())))
                },
                TokenType::_Self => {
                    iter.next();  // skip 'Self'
                    Ok(Some(ExprAST::_Self))
                },
                TokenType::_self => {
                    iter.next();  // skip 'self'
                    Ok(Some(ExprAST::_self))
                },
                TokenType::ParenLeft => {
                    iter.next(); // skip '('
                    let result = self.parse_expression(iter, defs, labels)?;
                    self.parse_expected_token(iter, TokenType::ParenRight)?;

                    Ok(result)
                },
                _ => {
                    self.parse_constant(iter, defs)
                },
            }
        }else{
            Ok(None)
        }
    }

    fn parse_expected_token(&self, iter: &mut Peekable<Iter<Token>>, expected: TokenType) -> Result<Location, ParserError> {
        if let Some(tok) = iter.next() {
            let typ = tok.get_type();
            if *typ == expected {
                Ok(tok.get_location().clone())
            }else{
                Err(ParserError::without_expected_token(Some(tok.get_location().clone()), expected, typ.clone()))
            }
        }else{
            Err(ParserError::illegal_end_of_input(None))
        }
    }

    fn parse_constant(&self, iter: &mut Peekable<Iter<Token>>, _defs: &mut Defines) -> Result<Option<ExprAST>, ParserError> {
        if let Some(tok) = iter.peek() {

            match &*tok.get_type() {
                TokenType::CharLiteral(ch) => {
                    iter.next();
                    Ok(Some(ExprAST::Char(*ch)))
                },
                TokenType::IntLiteral(num) => {
                    iter.next();
                    Ok(Some(ExprAST::Int(*num)))
                },
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
    fn parse_constant() {
        let src = "'a'";
        let ast = parse_constant_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::Char('a' as u32)
        );

        let src = "123";
        let ast = parse_constant_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::Int(123)
        );

        let src = "1.2";
        let ast = parse_constant_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::Double(1.2)
        );

        let src = "\"Hello world!\"";
        let ast = parse_constant_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::StringLiteral("Hello world!".into())
        );
    }
}
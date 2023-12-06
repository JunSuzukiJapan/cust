#![allow(dead_code)]

use super::Token;
use super::AST;
use super::Defines;
use super::ParserError;
use std::iter::Peekable;
use std::slice::Iter;

pub struct Parser;

impl Parser {
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
}
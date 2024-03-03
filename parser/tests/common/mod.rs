#![allow(unused)]
#![allow(non_camel_case_types)]

extern crate tokenizer;
extern crate parser;

pub use tokenizer::*;
pub use parser::*;

pub fn parse_expression_from_str(src: &str) -> Result<Option<ExprAST>, ParserError> {
    let token_list = Tokenizer::tokenize(src).unwrap();
    let mut iter = token_list.iter().peekable();
    let parser = Parser::new();
    let mut defs = Defines::new();
    let mut labels = Vec::new();
    parser.parse_expression(&mut iter, &mut defs, &mut Some(&mut labels))
}

pub fn parse_external_declaration_from_str(src: &str) -> Result<Option<AST>, ParserError> {
    let token_list = Tokenizer::tokenize(src).unwrap();
    let mut iter = token_list.iter().peekable();
    let parser = Parser::new();
    let mut defs = Defines::new();
    let mut labels = Vec::new();
    parser.parse_external_declaration(&mut iter, &mut defs, &mut Some(&mut labels))
}

pub fn parse_translation_unit_from_str(src: &str) -> Result<Vec<AST>, ParserError> {
    let token_list = Tokenizer::tokenize(src).unwrap();
    let mut iter = token_list.iter().peekable();
    let parser = Parser::new();
    let mut defs = Defines::new();
    parser.parse_translation_unit(&mut iter, &mut defs)
}
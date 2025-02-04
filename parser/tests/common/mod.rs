#![allow(unused)]
#![allow(non_camel_case_types)]

extern crate tokenizer;
extern crate parser;

pub use tokenizer::*;
pub use parser::*;

pub fn parse_from_str(input: &str) -> Result<Vec<ToplevelAST>, ParserError> {
    let token_list = Tokenizer::tokenize(input)?;
    Parser::parse(token_list)
}

pub fn parse_expression_from_str(src: &str) -> Result<Option<ExprAST>, ParserError> {
    let token_list = Tokenizer::tokenize(src).unwrap();
    let mut iter = token_list.iter().peekable();
    let parser = Parser::new();
    let mut defs = Defines::new();
    parser.parse_expression(&mut iter, &mut defs)
}

pub fn parse_external_declaration_from_str(src: &str) -> Result<Option<ToplevelAST>, ParserError> {
    let token_list = Tokenizer::tokenize(src).unwrap();
    let mut iter = token_list.iter().peekable();
    let parser = Parser::new();
    let mut defs = Defines::new();
    parser.parse_external_declaration(&mut iter, &mut defs)
}

pub fn parse_translation_unit_from_str(src: &str) -> Result<Vec<ToplevelAST>, ParserError> {
    let token_list = Tokenizer::tokenize(src).unwrap();
    let mut iter = token_list.iter().peekable();
    let parser = Parser::new();
    let mut defs = Defines::new();
    parser.parse_translation_unit(&mut iter, &mut defs)
}


pub fn parse_pattern_from_str(src: &str) -> Result<(Vec<(Box<Pattern>, Position)>, Option<String>), ParserError> {
    let token_list = Tokenizer::tokenize(src).unwrap();
    let mut iter = token_list.iter().peekable();
    let parser = Parser::new();
    let mut defs = Defines::new();
    let mut labels = Vec::new();
    parser.parse_pattern(&mut iter, &mut defs, &mut Some(&mut labels))
}

pub fn parse_stmt_from_str(src: &str) -> Result<Option<AST>, ParserError> {
    let token_list = Tokenizer::tokenize(src).unwrap();
    let mut iter = token_list.iter().peekable();
    let parser = Parser::new();
    let mut defs = Defines::new();
    let mut labels = Vec::new();
    parser.parse_statement(&mut iter, &mut defs, &mut Some(&mut labels))
}
mod common;

mod tests {
    use super::common::*;

    fn parse_pattern_from_str(src: &str) -> Result<(Vec<(Box<Pattern>, Position)>, Option<String>), ParserError> {
        let token_list = Tokenizer::tokenize(src).unwrap();
        let mut iter = token_list.iter().peekable();
        let parser = Parser::new();
        let mut defs = Defines::new();
        let mut labels = Vec::new();
        parser.parse_pattern(&mut iter, &mut defs, &mut Some(&mut labels))
    }

    fn parse_stmt_from_str(src: &str) -> Result<Option<AST>, ParserError> {
        let token_list = Tokenizer::tokenize(src).unwrap();
        let mut iter = token_list.iter().peekable();
        let parser = Parser::new();
        let mut defs = Defines::new();
        let mut labels = Vec::new();
        parser.parse_statement(&mut iter, &mut defs, &mut Some(&mut labels))
    }

    #[test]
    fn parse_if_let() {
        let src = "
            if let (Some(value) = variable) {
                1;
            }else{
                2;
            }
        ";
    
        // parse
        let asts = parse_stmt_from_str(src).unwrap();

        unimplemented!()
    }
}
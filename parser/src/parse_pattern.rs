use crate::{pattern, ExprAST, Pattern};
use super::{Position, Token};
use super::ParserError;
use super::parse::Parser;
use super::defines::Defines;

use std::f32::consts::E;
use std::slice::Iter;
use std::iter::Peekable;


impl Parser {
    pub fn parse_pattern(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(Vec<(Pattern, Position)>, Option<String>), ParserError> {
        let mut v = Vec::new();
        let mut name: Option<String> = None;

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
                    }else{
                        v.push((pat, pos.clone()));
                    }
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
                    }else{
                        v.push((pat, pos.clone()))
                    }
                },
                Token::StringLiteral(s) => {
                    let pat = Pattern::Str(s.to_string());
                    v.push((pat, pos.clone()))
                },
                Token::ParenLeft => {  // parse tuple pattern
                    let mut patterns_lists = Vec::new();

                    let (tok2, _pos2) = iter.peek().unwrap();

                    if *tok2 == Token::ParenRight {  // when "()"
                        iter.next();  // skip ')'
                    }else{

                        loop {
                            let pat = self.parse_pattern(iter, defs, labels)?;
                            patterns_lists.push(pat);

                            let (tok3, pos3) = iter.peek().unwrap();
                            if *tok3 == Token::Comma {
                                iter.next();  // skip ','
                            }else if *tok3 == Token::ParenRight {  // when ')'
                                iter.next();  // skip ')'
                                break;
                            }else{
                                return Err(ParserError::syntax_error(pos3.clone()));
                            }
                        }
                    }

                    let mut pat_list: Vec<(Vec<Box<Pattern>>, Option<String>)> = Vec::new();
                    for (patterns, name) in &patterns_lists {
                        let mut v = Vec::new();
                        for (pat, _pos) in patterns {
                            v.push(Box::new(pat.clone()));
                        }
                        pat_list.push((v, name.clone()));
                    }

                    let p = Pattern::Tuple(pat_list);
                    v.push((p, pos.clone()));
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
                Token::EndOfInput => {
                    return Err(ParserError::illegal_end_of_input(pos.clone()));
                },
                _ => return Err(ParserError::not_pattern(pos.clone())),
            }

            // '|' check
            let (tok2, pos2) = iter.peek().unwrap();
            if *tok2 == Token::BitOr {
                iter.next();  // skip '|'

            }else if *tok2 == Token::At {
                iter.next();  // skip '@'

                let (tok3, pos3) = iter.peek().unwrap();
                if tok3.is_symbol() {
                    iter.next();

                    let str = tok3.get_symbol_name().unwrap();
                    name = Some(str.to_string());

                    break;

                }else{
                    return Err(ParserError::syntax_error(pos3.clone()));
                }

            }else{
                break;
            }
        }

        Ok((v, name))
    }
}

#[cfg(test)]
mod tests {
    use tokenizer::*;
    use super::*;

    fn parse_pattern_from_str(src: &str) -> Result<(Vec<(Pattern, Position)>, Option<String>), ParserError> {
        let token_list = Tokenizer::tokenize(src).unwrap();
        let mut iter = token_list.iter().peekable();
        let parser = Parser::new();
        let mut defs = Defines::new();
        let mut labels = Vec::new();
        parser.parse_pattern(&mut iter, &mut defs, &mut Some(&mut labels))
    }

    #[test]
    fn parse_num_pattern() {
        let src = "123";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let (pat, _pos) = pat_vec.first().unwrap();
        assert_eq!(*pat, Pattern::Number(123));
    }

    #[test]
    fn parse_num_range_pattern() {
        let src = "1 ..= 5";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let (pat, _pos) = pat_vec.first().unwrap();
        assert_eq!(*pat, Pattern::NumberRange(1, 5));
    }

    #[test]
    fn parse_num_at_pattern() {
        let src = "123 @ x";
        let (pat_vec, name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);
        assert_eq!(name.unwrap(), "x");

        let (pat, _pos) = pat_vec.first().unwrap();
        assert_eq!(*pat, Pattern::Number(123));
    }

    #[test]
    fn parse_char_pattern() {
        let src = "'a'";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let (pat, _pos) = pat_vec.first().unwrap();
        assert_eq!(*pat, Pattern::Char('a'));
    }

    #[test]
    fn parse_char_range_pattern() {
        let src = "'a' ..= 'e'";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();
println!("pat_vec: {pat_vec:?}");
        assert_eq!(pat_vec.len(), 1);

        let (pat, _pos) = &pat_vec[0];
        assert_eq!(*pat, Pattern::CharRange('a', 'e'));
    }

    #[test]
    fn parse_char_at_pattern() {
        let src = "'a' | 'b' | 'c' @ ch";
        let (pat_vec, name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 3);
        assert_eq!(name.unwrap(), "ch");

        let (pat, _pos) = &pat_vec[0];
        assert_eq!(*pat, Pattern::Char('a'));
        let (pat, _pos) = &pat_vec[1];
        assert_eq!(*pat, Pattern::Char('b'));
        let (pat, _pos) = &pat_vec[2];
        assert_eq!(*pat, Pattern::Char('c'));
    }

    #[test]
    fn parse_str_pattern() {
        let src = "\"hello\"";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let (pat, _pos) = pat_vec.first().unwrap();
        assert_eq!(*pat, Pattern::Str("hello".to_string()));
    }

    #[test]
    fn parse_var_pattern() {
        let src = "some_var";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let (pat, _pos) = pat_vec.first().unwrap();
        assert_eq!(*pat, Pattern::Var("some_var".to_string()));
    }

    #[test]
    fn parse_char_or_pattern() {
        let src = "'a' | 'b'";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 2);

        let (pat, _pos) = &pat_vec[0];
        assert_eq!(*pat, Pattern::Char('a'));

        let (pat2, _pos2) = &pat_vec[1];
        assert_eq!(*pat2, Pattern::Char('b'));
    }

    #[test]
    fn parse_tuple_pattern() {
        let src = "(1, 'a', \"Hello\")";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        if let (Pattern::Tuple(patterns_list), _pos) = &pat_vec[0] {
            assert_eq!(patterns_list.len(), 3);

            let (patterns1, name1) = &patterns_list[0];
            let (patterns2, name2) = &patterns_list[1];
            let (patterns3, name3) = &patterns_list[2];

            assert_eq!(patterns1.len(), 1);
            assert_eq!(patterns2.len(), 1);
            assert_eq!(patterns3.len(), 1);

            assert_eq!(*name1, None);
            assert_eq!(*name2, None);
            assert_eq!(*name2, None);

            assert_eq!(*patterns1[0], Pattern::Number(1));
            assert_eq!(*patterns2[0], Pattern::Char('a'));
            assert_eq!(*patterns3[0], Pattern::Str("Hello".to_string()));

        } else {
            panic!();
        }

    }

    #[test]
    fn parse_tuple_at_pattern() {
        let src = "(1 | 2 @ x, ('a' | 'b' @ y, 'c' | 'd' @ z) @ inner) @ outer";
        let (pat_vec, outer_name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);
        assert_eq!(outer_name.unwrap(), "outer");

        if let (Pattern::Tuple(patterns_list), _pos) = &pat_vec[0] {
            assert_eq!(patterns_list.len(), 2);

            let (patterns1, name1) = &patterns_list[0];
            let (patterns2, name2) = &patterns_list[1];

            assert_eq!(patterns1.len(), 2);
            assert_eq!(patterns2.len(), 1);

            assert_eq!(*patterns1[0], Pattern::Number(1));
            assert_eq!(*patterns1[1], Pattern::Number(2));
            assert_eq!(name1.as_ref().unwrap(), "x");
            assert_eq!(name2.as_ref().unwrap(), "inner");

            if let Pattern::Tuple(patterns_list2) = &*patterns2[0] {
                assert_eq!(patterns_list2.len(), 2);

                let (patterns1, name1) = &patterns_list2[0];
                let (patterns2, name2) = &patterns_list2[1];

                assert_eq!(patterns1.len(), 2);
                assert_eq!(patterns2.len(), 2);

                assert_eq!(*patterns1[0], Pattern::Char('a'));
                assert_eq!(*patterns1[1], Pattern::Char('b'));
                assert_eq!(name1.as_ref().unwrap(), "y");

                assert_eq!(*patterns2[0], Pattern::Char('c'));
                assert_eq!(*patterns2[1], Pattern::Char('d'));
                assert_eq!(name2.as_ref().unwrap(), "z");

            }else{
                panic!();
            }

        } else {
            panic!();
        }

    }

}
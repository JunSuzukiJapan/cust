use crate::pattern::{EnumPattern, StructPattern};
use crate::Pattern;
use super::{Position, Token};
use super::ParserError;
use super::parse::Parser;
use super::defines::Defines;

use std::collections::HashMap;
use std::slice::Iter;
use std::iter::Peekable;


impl Parser {
    pub fn parse_pattern(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(Vec<(Box<Pattern>, Position)>, Option<String>), ParserError> {
        let mut v: Vec<(Box<Pattern>, Position)> = Vec::new();
        let mut name: Option<String> = None;

        loop {
            let (tok, pos) = iter.next().unwrap();

            match &*tok {
                Token::CharLiteral(ch) => {
                    let pat = Pattern::Char(*ch);

                    let (tok2, _pos2) = iter.peek().unwrap();
                    if *tok2 == Token::RangeEqual {
                        iter.next();

                        let (tok3, pos3) = iter.peek().unwrap();
                        match tok3 {
                            Token::CharLiteral(ch2) => {
                                iter.next();

                                let pat2 = Pattern::CharRange(*ch, *ch2);
                                v.push((Box::new(pat2), pos.clone()));
                            },
                            _ => return Err(ParserError::syntax_error(pos3.clone())),
                        }
                    }else{
                        v.push((Box::new(pat), pos.clone()));
                    }
                },
                Token::IntLiteral(num) => {
                    let pat = Pattern::Number(*num);

                    let (tok2, _pos2) = iter.peek().unwrap();
                    if *tok2 == Token::RangeEqual {
                        iter.next();

                        let (tok3, pos3) = iter.peek().unwrap();
                        match tok3 {
                            Token::IntLiteral(num2) => {
                                iter.next();

                                let pat2 = Pattern::NumberRange(*num, *num2);
                                v.push((Box::new(pat2), pos.clone()));
                            },
                            _ => return Err(ParserError::syntax_error(pos3.clone())),
                        }
                    }else{
                        v.push((Box::new(pat), pos.clone()))
                    }
                },
                Token::StringLiteral(s) => {
                    let pat = Pattern::Str(s.to_string());
                    v.push((Box::new(pat), pos.clone()))
                },
                Token::ParenLeft => {  // parse tuple pattern
                    let pat = self.parse_tuple_pattern(iter, defs, labels)?;
                    v.push((Box::new(pat), pos.clone()));
                },
                Token::Symbol(name) => {
                    let (tok2, _pos2) = iter.peek().unwrap();
                    match tok2 {
                        Token::WColon => {  // parse Enum pattern
                            iter.next();  // skip '::'

                            let (tok3, pos3) = iter.next().unwrap();
                            if ! tok3.is_symbol() {
                                return Err(ParserError::syntax_error(pos3.clone()));
                            }
                            let sub_name = tok3.get_symbol_name().unwrap();

                            let pat = self.parse_enum_pattern(name, sub_name, iter, defs, labels)?;
                            v.push((Box::new(pat), pos.clone()));
                        },
                        Token::BraceLeft => {  // parse struct pattern
                            iter.next();  // skip '{'

                            let struct_pat = self.parse_struct_pattern(name, iter, defs, labels)?;
                            let pat = Pattern::Struct(struct_pat);
                            v.push((Box::new(pat), pos.clone()));
                        },
                        Token::ParenLeft => {
                            iter.next();  // skip '('

                            let pat = self.parse_tuple_pattern(iter, defs, labels)?;
                            if let Pattern::Tuple(list) = pat {
                                let enum_pat = EnumPattern::Tuple("".into(), name.to_string(), list);
                                let pat = Pattern::Enum(enum_pat);
                                v.push((Box::new(pat), pos.clone()))

                            }else{
                                panic!()
                            }
                        },
                        _ => {
                            let pat = Pattern::Var(name.to_string());
                            v.push((Box::new(pat), pos.clone()))
                        },
                    }
                },
                Token::EndOfInput => {
                    return Err(ParserError::illegal_end_of_input(pos.clone()));
                },
                _ => return Err(ParserError::not_pattern(pos.clone())),
            }

            // '|' check
            let (tok2, _pos2) = iter.peek().unwrap();
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

    fn parse_tuple_pattern(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Pattern, ParserError> {
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

        let pat = Pattern::Tuple(patterns_lists);
        Ok(pat)
    }

    fn parse_enum_pattern(&self, name: &str, sub_name: &str, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Pattern, ParserError> {
        let (tok, _pos) = iter.peek().unwrap();
        
        match tok {
            Token::ParenLeft => {  // Tuple pattern
                iter.next();  // skip '('

                if let Pattern::Tuple(list) = self.parse_tuple_pattern(iter, defs, labels)? {
                    let enum_pat = EnumPattern::Tuple(name.to_string(), sub_name.to_string(), list);
                    Ok(Pattern::Enum(enum_pat))

                }else{
                    panic!("system error");  // never reach
                }
            },
            Token::BraceLeft => {  // Struct pattern
                iter.next();  // skip '{'
                let struct_pat = self.parse_struct_pattern(&format!("{name}::{sub_name}"), iter, defs, labels)?;
                let enum_pat = EnumPattern::Struct(name.to_string(), sub_name.to_string(), struct_pat);
                Ok(Pattern::Enum(enum_pat))
            },
            _ => {  // pattern 'Name::SubName'
                let enum_pat = EnumPattern::Simple(name.to_string(), sub_name.to_string());
                Ok(Pattern::Enum(enum_pat))
            },
        }
    }

    fn parse_struct_pattern(&self, name: &str, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<StructPattern, ParserError> {
        let mut map: HashMap<String, Option<Vec<(Box<Pattern>, Position)>>> = HashMap::new();
        let mut has_optional = false;

        loop {
            let (tok, pos) = iter.peek().unwrap();

            match tok {
                Token::BraceRight => {
                    iter.next();  // skip '}'
                    break;
                },
                Token::Symbol(name) => {
                    iter.next();  // skip Symbol

                    let (tok2, _pos2) = iter.peek().unwrap();
                    match tok2 {
                        Token::Comma => {  // ','
                            iter.next();  // skip ','
                            map.insert(name.to_string(), None);
                        },
                        Token::Colon => {  // ':'
                            iter.next();  // skip ';'

                            let (pat, _name) = self.parse_pattern(iter, defs, labels)?;
                            map.insert(name.to_string(), Some(pat));

                            let (tok3, _pos3) = iter.peek().unwrap();
                            if *tok3 == Token::Comma {
                                iter.next();  // skip ','
                            }
                        },
                        _ => ()  // do nothing
                    }
                },
                Token::TripleDot => {
                    iter.next();  // skip '...'

                    has_optional = true;
                    break;
                },
                _ => {
                    return  Err(ParserError::syntax_error(pos.clone()));
                }
            }
        }

        let struct_pat = StructPattern {
            name: name.to_string(),
            map,
            has_optional,
        };

        Ok(struct_pat)
    }
}

#[cfg(test)]
mod tests {
    use crate::pattern;

    use super::*;
    use pattern::StructPattern;
    use tokenizer::*;

    fn parse_pattern_from_str(src: &str) -> Result<(Vec<(Box<Pattern>, Position)>, Option<String>), ParserError> {
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

        let (pat, _pos) = &pat_vec[0];
        assert_eq!(**pat, Pattern::Number(123));
    }

    #[test]
    fn parse_num_range_pattern() {
        let src = "1 ..= 5";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let (pat, _pos) = &pat_vec[0];
        assert_eq!(**pat, Pattern::NumberRange(1, 5));
    }

    #[test]
    fn parse_num_at_pattern() {
        let src = "123 @ x";
        let (pat_vec, name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);
        assert_eq!(name.unwrap(), "x");

        let (pat, _pos) = &pat_vec[0];
        assert_eq!(**pat, Pattern::Number(123));
    }

    #[test]
    fn parse_char_pattern() {
        let src = "'a'";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let (pat, _pos) = &pat_vec[0];
        assert_eq!(**pat, Pattern::Char('a'));
    }

    #[test]
    fn parse_char_range_pattern() {
        let src = "'a' ..= 'e'";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let (pat, _pos) = &pat_vec[0];
        assert_eq!(**pat, Pattern::CharRange('a', 'e'));
    }

    #[test]
    fn parse_char_at_pattern() {
        let src = "'a' | 'b' | 'c' @ ch";
        let (pat_vec, name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 3);
        assert_eq!(name.unwrap(), "ch");

        let (pat, _pos) = &pat_vec[0];
        assert_eq!(**pat, Pattern::Char('a'));
        let (pat, _pos) = &pat_vec[1];
        assert_eq!(**pat, Pattern::Char('b'));
        let (pat, _pos) = &pat_vec[2];
        assert_eq!(**pat, Pattern::Char('c'));
    }

    #[test]
    fn parse_str_pattern() {
        let src = "\"hello\"";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let (pat, _pos) = &pat_vec[0];
        assert_eq!(**pat, Pattern::Str("hello".to_string()));
    }

    #[test]
    fn parse_var_pattern() {
        let src = "some_var";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let (pat, _pos) = &pat_vec[0];
        assert_eq!(**pat, Pattern::Var("some_var".to_string()));
    }

    #[test]
    fn parse_char_or_pattern() {
        let src = "'a' | 'b'";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 2);

        let (pat, _pos) = &pat_vec[0];
        assert_eq!(**pat, Pattern::Char('a'));

        let (pat2, _pos2) = &pat_vec[1];
        assert_eq!(**pat2, Pattern::Char('b'));
    }

    #[test]
    fn parse_tuple_pattern() {
        let src = "(1, 'a', \"Hello\")";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let (pat, _pos) = &pat_vec[0];
        if let Pattern::Tuple(patterns_list) = &**pat {
            assert_eq!(patterns_list.len(), 3);

            let (patterns1, name1) = &patterns_list[0];
            let (patterns2, name2) = &patterns_list[1];
            let (patterns3, name3) = &patterns_list[2];

            assert_eq!(patterns1.len(), 1);
            assert_eq!(patterns2.len(), 1);
            assert_eq!(patterns3.len(), 1);

            assert_eq!(*name1, None);
            assert_eq!(*name2, None);
            assert_eq!(*name3, None);

            let (pat1, _pos1) = &patterns1[0];
            assert_eq!(**pat1, Pattern::Number(1));
            let (pat2, _pos2) = &patterns2[0];
            assert_eq!(**pat2, Pattern::Char('a'));
            let (pat3, _pos3) = &patterns3[0];
            assert_eq!(**pat3, Pattern::Str("Hello".to_string()));

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

        let (pat, _pos) = &pat_vec[0];
        if let Pattern::Tuple(patterns_list) = &**pat {
            assert_eq!(patterns_list.len(), 2);

            let (patterns1, name1) = &patterns_list[0];
            let (patterns2, name2) = &patterns_list[1];

            assert_eq!(patterns1.len(), 2);
            assert_eq!(patterns2.len(), 1);

            let (pat1, _pos1) = &patterns1[0];
            assert_eq!(**pat1, Pattern::Number(1));
            let (pat1_2, _pos1_2) = &patterns1[1];
            assert_eq!(**pat1_2, Pattern::Number(2));
            assert_eq!(name1.as_ref().unwrap(), "x");
            assert_eq!(name2.as_ref().unwrap(), "inner");

            let (pat2, _pos2) = &patterns2[0];
            if let Pattern::Tuple(patterns_list2) = &**pat2 {
                assert_eq!(patterns_list2.len(), 2);

                let (patterns1, name1) = &patterns_list2[0];
                let (patterns2, name2) = &patterns_list2[1];

                assert_eq!(patterns1.len(), 2);
                assert_eq!(patterns2.len(), 2);

                let (pat, _pos) = &patterns1[0];
                assert_eq!(**pat, Pattern::Char('a'));
                let (pat, _pos) = &patterns1[1];
                assert_eq!(**pat, Pattern::Char('b'));
                assert_eq!(name1.as_ref().unwrap(), "y");

                let (pat, _pos) = &patterns2[0];
                assert_eq!(**pat, Pattern::Char('c'));
                let (pat, _pos) = &patterns2[1];
                assert_eq!(**pat, Pattern::Char('d'));
                assert_eq!(name2.as_ref().unwrap(), "z");

            }else{
                panic!();
            }

        } else {
            panic!();
        }
    }

    #[test]
    fn parse_enum_simple_pattern() {
        let src = "EnumName::SubName";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let (pat, _pos) = &pat_vec[0];
        if let Pattern::Enum(enum_pat) = &**pat {
            match enum_pat {
                EnumPattern::Simple(name, sub_name) => {
                    assert_eq!(name, "EnumName");
                    assert_eq!(sub_name, "SubName");
                },
                _ => panic!()
            }


        }else{
            panic!()
        }
    }

    #[test]
    fn parse_enum_tuple_pattern() {
        let src = "EnumName::SubName(1, 'a', \"Hello\")";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let (pat, _pos) = &pat_vec[0];
        if let Pattern::Enum(enum_pat) = &**pat {
            match enum_pat {
                EnumPattern::Tuple(name, sub_name, patterns_list) => {
                    assert_eq!(name, "EnumName");
                    assert_eq!(sub_name, "SubName");

                    assert_eq!(pat_vec.len(), 1);

                    assert_eq!(patterns_list.len(), 3);

                    let (patterns1, name1) = &patterns_list[0];
                    let (patterns2, name2) = &patterns_list[1];
                    let (patterns3, name3) = &patterns_list[2];

                    assert_eq!(patterns1.len(), 1);
                    assert_eq!(patterns2.len(), 1);
                    assert_eq!(patterns3.len(), 1);

                    assert_eq!(*name1, None);
                    assert_eq!(*name2, None);
                    assert_eq!(*name3, None);

                    let (pat, _pos) = &patterns1[0];
                    assert_eq!(**pat, Pattern::Number(1));
                    let (pat, _pos) = &patterns2[0];
                    assert_eq!(**pat, Pattern::Char('a'));
                    let (pat, _pos) = &patterns3[0];
                    assert_eq!(**pat, Pattern::Str("Hello".to_string()));
            },
                _ => panic!()
            }

        }else{
            panic!()
        }
    }

    #[test]
    fn parse_struct_pattern() {
        let src = "Foo {x, y: 0, ...} ";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let (pat, _pos) = &pat_vec[0];
        if let Pattern::Struct(strct_pat) = &**pat {
            let StructPattern {name, map, has_optional} = strct_pat;
            assert_eq!(name, "Foo");
            assert_eq!(*has_optional, true);

            assert_eq!(map.len(), 2);
            assert_eq!(map["x"], None);

            if let Some(vec) = &map["y"] {
                assert_eq!(vec.len(), 1);
                assert_eq!(*vec[0].0, Pattern::Number(0));

            }else{
                panic!()
            }

        }else{
            panic!()
        }
    }

    #[test]
    fn parse_enum_struct_pattern() {
        let src = "Some::Foo {x, y: 0, z: 'a' | 'b'} ";
        let (pat_vec, _name) = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let (pat, _pos) = &pat_vec[0];
        if let Pattern::Enum(enum_pat) = &**pat {
            match enum_pat {
                EnumPattern::Struct(name, sub_name, struct_pat) => {
                    assert_eq!(name, "Some");
                    assert_eq!(sub_name, "Foo");

                    let StructPattern {name, map, has_optional} = struct_pat;
                    assert_eq!(name, "Some::Foo");
                    assert_eq!(*has_optional, false);

                    assert_eq!(map.len(), 3);
                    assert_eq!(map["x"], None);

                    if let Some(vec) = &map["y"] {
                        assert_eq!(vec.len(), 1);
                        assert_eq!(*vec[0].0, Pattern::Number(0));

                    }else{
                        panic!()
                    }

                    if let Some(vec) = &map["z"] {
                        assert_eq!(vec.len(), 2);
                        assert_eq!(*vec[0].0, Pattern::Char('a'));
                        assert_eq!(*vec[1].0, Pattern::Char('b'));

                    }else{
                        panic!()
                    }
                },
                _ => panic!(),
            }
        }
    }
}
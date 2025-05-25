use crate::pattern::{EnumPattern, StructPattern};
use crate::Pattern;
use super::{Position, Token};
use super::ParserError;
use super::parse::Parser;
use super::defines::Defines;

use std::clone;
use std::collections::HashMap;
use std::slice::Iter;
use std::iter::Peekable;
use std::rc::Rc;


impl Parser {
    pub fn parse_pattern_list(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Vec<Box<Pattern>>, ParserError> {
        let mut v: Vec<Box<Pattern>> = Vec::new();

        loop {
            // let (tok, pos) = iter.next().unwrap();

            // match &*tok {
            //     Token::CharLiteral(ch) => {
            //         let pat = Pattern::Char(*ch, None, pos.clone());

            //         let (tok2, _pos2) = iter.peek().unwrap();
            //         if *tok2 == Token::RangeEqual {
            //             iter.next();

            //             let (tok3, pos3) = iter.peek().unwrap();
            //             match tok3 {
            //                 Token::CharLiteral(ch2) => {
            //                     iter.next();

            //                     if ch > ch2 {
            //                         return Err(ParserError::left_greater_than_right_in_char_range(*ch, *ch2, pos3.clone()));
            //                     }
            //                     let pat2 = Pattern::CharRange(*ch, *ch2, None, pos.clone());
            //                     v.push(Box::new(pat2));
            //                 },
            //                 _ => return Err(ParserError::syntax_error(file!(), line!(), column!(), pos3.clone())),
            //             }
            //         }else{
            //             v.push(Box::new(pat));
            //         }
            //     },
            //     Token::IntLiteral(num) => {
            //         let pat = Pattern::Number(*num, None, pos.clone());

            //         let (tok2, _pos2) = iter.peek().unwrap();
            //         if *tok2 == Token::RangeEqual {
            //             iter.next();

            //             let (tok3, pos3) = iter.peek().unwrap();
            //             match tok3 {
            //                 Token::IntLiteral(num2) => {
            //                     iter.next();

            //                     let pat2 = Pattern::NumberRange(*num, *num2, None, pos3.clone());
            //                     v.push(Box::new(pat2));
            //                 },
            //                 _ => return Err(ParserError::syntax_error(file!(), line!(), column!(), pos3.clone())),
            //             }
            //         }else{
            //             v.push(Box::new(pat))
            //         }
            //     },
            //     Token::StringLiteral(s) => {
            //         let pat = Pattern::Str(s.to_string(), None, pos.clone());
            //         v.push(Box::new(pat))
            //     },
            //     Token::TupleStart => {
            //         let pat = self.parse_tuple_pattern(iter, defs, labels)?;
            //         v.push(Box::new(pat));
            //     },
            //     Token::Symbol(name) => {
            //         let (tok2, pos2) = iter.peek().unwrap();
            //         match tok2 {
            //             Token::WColon => {  // parse Enum pattern
            //                 iter.next();  // skip '::'

            //                 let (tok3, pos3) = iter.next().unwrap();
            //                 if ! tok3.is_symbol() {
            //                     return Err(ParserError::syntax_error(file!(), line!(), column!(), pos3.clone()));
            //                 }
            //                 let sub_name = tok3.get_symbol_name().unwrap();
            //                 let pat = self.parse_enum_pattern(name, sub_name, iter, defs, labels)?;
            //                 v.push(Box::new(pat));
            //             },
            //             Token::BraceLeft => {  // parse struct pattern
            //                 iter.next();  // skip '{'

            //                 let struct_pat = self.parse_struct_pattern(name, iter, defs, labels)?;
            //                 let pat = Pattern::Struct(struct_pat, None, pos2.clone());
            //                 v.push(Box::new(pat));
            //             },
            //             Token::ParenLeft => {
            //                 iter.next();  // skip '('

            //                 let pat = self.parse_tuple_pattern(iter, defs, labels)?;
            //                 if let Pattern::Tuple(list, opt_at_name, pos3) = pat {
            //                     let enum_type = defs.get_type(name).ok_or(ParserError::no_type_defined(Some(name.into()), pos.clone()))?;
            //                     let enum_type = Rc::clone(enum_type);

            //                     let enum_pat = EnumPattern::Tuple(enum_type, "".into(), name.to_string(), list);
            //                     let pat = Pattern::Enum(enum_pat, opt_at_name, pos3.clone());
            //                     v.push(Box::new(pat))

            //                 }else{
            //                     panic!()
            //                 }
            //             },
            //             Token::At => {
            //                 iter.next();  // skip '@'

            //                 let pat_list = self.parse_pattern_list(iter, defs, labels)?;



            //                 // v.push(Box::new(pat_list));
            //                 unimplemented!()
            //             },
            //             _ => {
            //                 let pat = Pattern::Var(name.to_string(), None, pos.clone());
            //                 v.push(Box::new(pat))
            //             },
            //         }
            //     },
            //     Token::EndOfInput => {
            //         return Err(ParserError::illegal_end_of_input(pos.clone()));
            //     },
            //     _ => return Err(ParserError::not_pattern(pos.clone())),
            // }

            let pat = self.parse_pattern(iter, defs, labels)?;
            v.push(Box::new(pat));

            // '|' check
            let (tok2, _pos2) = iter.peek().unwrap();
            if *tok2 == Token::BitOr {
                iter.next();  // skip '|'
            }else{
                break;
            }
        }

        Ok(v)
    }

    fn parse_pattern(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Pattern, ParserError> {
        let (tok, pos) = iter.next().unwrap();

        match &*tok {
            Token::ParenLeft => {  // '('
                let pat_list = self.parse_pattern_list(iter, defs, labels)?;
                let pat = Pattern::OrList(pat_list, None, pos.clone());

                let (tok2, pos2) = iter.next().unwrap();
                if tok2 != &Token::ParenRight {
                    return Err(ParserError::syntax_error(file!(), line!(), column!(), pos2.clone()));
                }

                return Ok(pat);
            },
            Token::CharLiteral(ch) => {
                let pat = Pattern::Char(*ch, None, pos.clone());

                let (tok2, _pos2) = iter.peek().unwrap();
                if *tok2 == Token::RangeEqual {
                    iter.next();

                    let (tok3, pos3) = iter.peek().unwrap();
                    match tok3 {
                        Token::CharLiteral(ch2) => {
                            iter.next();

                            if ch > ch2 {
                                return Err(ParserError::left_greater_than_right_in_char_range(*ch, *ch2, pos3.clone()));
                            }
                            let pat2 = Pattern::CharRange(*ch, *ch2, None, pos.clone());
                            // v.push(Box::new(pat2));
                            return Ok(pat2);
                        },
                        _ => return Err(ParserError::syntax_error(file!(), line!(), column!(), pos3.clone())),
                    }
                }else{
                    // v.push(Box::new(pat));
                    return Ok(pat);
                }
            },
            Token::IntLiteral(num) => {
                let pat = Pattern::Number(*num, None, pos.clone());

                let (tok2, _pos2) = iter.peek().unwrap();
                if *tok2 == Token::RangeEqual {
                    iter.next();

                    let (tok3, pos3) = iter.peek().unwrap();
                    match tok3 {
                        Token::IntLiteral(num2) => {
                            iter.next();

                            let pat2 = Pattern::NumberRange(*num, *num2, None, pos3.clone());
                            // v.push(Box::new(pat2));
                            return Ok(pat2);
                        },
                        _ => return Err(ParserError::syntax_error(file!(), line!(), column!(), pos3.clone())),
                    }
                }else{
                    // v.push(Box::new(pat))
                    return Ok(pat);
                }
            },
            Token::StringLiteral(s) => {
                let pat = Pattern::Str(s.to_string(), None, pos.clone());
                // v.push(Box::new(pat))
                return Ok(pat);
            },
            Token::TupleStart => {
                let pat = self.parse_tuple_pattern(iter, defs, labels)?;
                // v.push(Box::new(pat));
                return Ok(pat);
            },
            Token::Symbol(name) => {
                let (tok2, pos2) = iter.peek().unwrap();
                match tok2 {
                    Token::WColon => {  // parse Enum pattern
                        iter.next();  // skip '::'

                        let (tok3, pos3) = iter.next().unwrap();
                        if ! tok3.is_symbol() {
                            return Err(ParserError::syntax_error(file!(), line!(), column!(), pos3.clone()));
                        }
                        let sub_name = tok3.get_symbol_name().unwrap();
                        let pat = self.parse_enum_pattern(name, sub_name, iter, defs, labels)?;
                        // v.push(Box::new(pat));
                        return Ok(pat);
                    },
                    Token::BraceLeft => {  // parse struct pattern
                        iter.next();  // skip '{'

                        let struct_pat = self.parse_struct_pattern(name, iter, defs, labels)?;
                        let pat = Pattern::Struct(struct_pat, None, pos2.clone());
                        // v.push(Box::new(pat));
                        return Ok(pat);
                    },
                    Token::ParenLeft => {
                        iter.next();  // skip '('

                        let pat = self.parse_tuple_pattern(iter, defs, labels)?;
                        if let Pattern::Tuple(list, opt_at_name, pos3) = pat {
                            let enum_type = defs.get_type(name).ok_or(ParserError::no_type_defined(Some(name.into()), pos.clone()))?;
                            let enum_type = Rc::clone(enum_type);

                            let enum_pat = EnumPattern::Tuple(enum_type, "".into(), name.to_string(), list);
                            let pat = Pattern::Enum(enum_pat, opt_at_name, pos3.clone());
                            // v.push(Box::new(pat))
                            return Ok(pat);

                        }else{
                            panic!()
                        }
                    },
                    Token::At => {
                        iter.next();  // skip '@'

                        let mut pat = self.parse_pattern(iter, defs, labels)?;
                        pat.set_at_name(Some(name.to_string()));

                        // v.push(Box::new(pat_list));
                        return Ok(pat);
                    },
                    _ => {
                        let pat = Pattern::Var(name.to_string(), None, pos.clone());
                        // v.push(Box::new(pat))
                        return Ok(pat);
                    },
                }
            },
            Token::EndOfInput => {
                return Err(ParserError::illegal_end_of_input(pos.clone()));
            },
            _ => {
                return Err(ParserError::not_pattern(pos.clone()));
            }
        }
    }

    fn parse_tuple_pattern(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Pattern, ParserError> {
        let mut patterns_lists = Vec::new();

        let (tok, pos) = iter.peek().unwrap();

        if *tok == Token::ParenRight {  // when "()"
            iter.next();  // skip ')'
        }else{

            loop {
                let pat = self.parse_pattern_list(iter, defs, labels)?;
                patterns_lists.push(pat);

                let (tok3, pos3) = iter.peek().unwrap();
                if *tok3 == Token::Comma {
                    iter.next();  // skip ','
                }else if *tok3 == Token::ParenRight {  // when ')'
                    iter.next();  // skip ')'
                    break;
                }else{
                    return Err(ParserError::syntax_error(file!(), line!(), column!(), pos3.clone()));
                }
            }
        }

        let pat = Pattern::Tuple(patterns_lists, None, pos.clone());
        Ok(pat)
    }

    fn parse_enum_pattern(&self, name: &str, sub_name: &str, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Pattern, ParserError> {
        let (tok, pos) = iter.peek().unwrap();

        let enum_type = defs.get_type(name).ok_or(ParserError::no_type_defined(Some(name.into()), pos.clone()))?;
        let enum_type = Rc::clone(enum_type);
        
        match tok {
            Token::ParenLeft => {  // Tuple pattern
                iter.next();  // skip '('

                if let Pattern::Tuple(list, _opt_name, _pos2) = self.parse_tuple_pattern(iter, defs, labels)? {
                    let enum_pat = EnumPattern::Tuple(enum_type, name.to_string(), sub_name.to_string(), list);
                    Ok(Pattern::Enum(enum_pat, None, pos.clone()))

                }else{
                    panic!("system error");  // never reach
                }
            },
            Token::BraceLeft => {  // Struct pattern
                iter.next();  // skip '{'
                let struct_pat = self.parse_struct_pattern(&format!("{name}::{sub_name}"), iter, defs, labels)?;
                let enum_pat = EnumPattern::Struct(enum_type, name.to_string(), sub_name.to_string(), struct_pat);
                Ok(Pattern::Enum(enum_pat, None, pos.clone()))
            },
            _ => {  // pattern 'Name::SubName'
                let enum_pat = EnumPattern::Simple(enum_type, name.to_string(), sub_name.to_string());
                Ok(Pattern::Enum(enum_pat, None, pos.clone()))
            },
        }
    }

    fn parse_struct_pattern(&self, name: &str, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<StructPattern, ParserError> {
        let mut map: HashMap<String, Option<Vec<Box<Pattern>>>> = HashMap::new();
        let mut list = Vec::new();
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
                            list.push(name.to_string());
                        },
                        Token::Colon => {  // ':'
                            iter.next();  // skip ';'

                            let pat = self.parse_pattern_list(iter, defs, labels)?;
                            map.insert(name.to_string(), Some(pat));
                            list.push(name.to_string());

                            let (tok3, _pos3) = iter.peek().unwrap();
                            if *tok3 == Token::Comma {
                                iter.next();  // skip ','
                            }
                        },
                        _ => {
                            map.insert(name.to_string(), None);
                            list.push(name.to_string());
                        }
                    }
                },
                Token::TripleDot => {
                    iter.next();  // skip '...'

                    has_optional = true;
                    break;
                },
                _ => {
                    return  Err(ParserError::syntax_error(file!(), line!(), column!(), pos.clone()));
                }
            }
        }

        let struct_pat = StructPattern {
            name: name.to_string(),
            keys: list,
            map,
            has_optional,
        };

        Ok(struct_pat)
    }
}

#[cfg(test)]
mod tests {
    use std::char;

    use crate::{pattern, EnumDefinition};

    use super::*;
    use pattern::StructPattern;
    use tokenizer::*;

    fn parse_pattern_from_str(src: &str) -> Result<Vec<Box<Pattern>>, ParserError> {
        let token_list = Tokenizer::tokenize(src).unwrap();
        let mut iter = token_list.iter().peekable();
        let parser = Parser::new();
        let mut defs = Defines::new();
        let mut labels = Vec::new();
        parser.parse_pattern_list(&mut iter, &mut defs, &mut Some(&mut labels))
    }

    fn parse_pattern_from_str_with_defs(src: &str, defs: &mut Defines) -> Result<Vec<Box<Pattern>>, ParserError> {
        let token_list = Tokenizer::tokenize(src).unwrap();
        let mut iter = token_list.iter().peekable();
        let parser = Parser::new();
        let mut labels = Vec::new();
        parser.parse_pattern_list(&mut iter, defs, &mut Some(&mut labels))
    }

    #[test]
    fn parse_num_pattern() {
        let src = "123";
        let pat_vec = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let pat = &pat_vec[0];
        if let Pattern::Number(num, _name, pos) = &**pat {
            assert_eq!(*num, 123);
            assert_eq!(pos.line, 1);
            assert_eq!(pos.column, 1);
        } else {
            panic!("Expected a number pattern");
        }
    }

    #[test]
    fn parse_num_range_pattern() {
        let src = "1 ..= 5";
        let pat_vec = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let pat = &pat_vec[0];
        if let Pattern::NumberRange(start, end, _name, pos) = &**pat {
            assert_eq!(*start, 1);
            assert_eq!(*end, 5);
            assert_eq!(pos.line, 1);
            assert_eq!(pos.column, 1);
        } else {
            panic!("Expected a number range pattern");
        }
    }

    #[test]
    fn parse_num_at_pattern() {
        let src = "x @ 123";
        let pat_vec = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let pat = &pat_vec[0];
        if let Pattern::Number(num, name, pos) = &**pat {
            assert_eq!(*num, 123);
            assert_eq!(*name, Some("x".into()));
            assert_eq!(pos.line, 1);
            assert_eq!(pos.column, 1);
        } else {
            panic!("Expected a variable pattern");
        }
    }

    #[test]
    fn parse_char_pattern() {
        let src = "'a'";
        let pat_vec = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let pat = &pat_vec[0];
        if let Pattern::Char(ch, _name, pos) = &**pat {
            assert_eq!(*ch, 'a');
            assert_eq!(pos.line, 1);
            assert_eq!(pos.column, 1);
        } else {
            panic!("Expected a char pattern");
        }
    }

    #[test]
    fn parse_char_range_pattern() {
        let src = "'a' ..= 'e'";
        let pat_vec = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let pat = &pat_vec[0];
        if let Pattern::CharRange(start, end, _name, pos) = &**pat {
            assert_eq!(*start, 'a');
            assert_eq!(*end, 'e');
            assert_eq!(pos.line, 1);
            assert_eq!(pos.column, 1);
        } else {
            panic!("Expected a char range pattern");
        }
    }

    #[test]
    fn parse_char_at_pattern() {
        let src = "ch @ 'a'";
        let pat_vec = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let pat = &pat_vec[0];
        if let Pattern::Char(ch, name, pos) = &**pat {
            assert_eq!(*ch, 'a');
            assert_eq!(name, &Some("ch".into()));
            assert_eq!(pos.line, 1);
            assert_eq!(pos.column, 1);
        } else {
            panic!("Expected a char pattern");
        }
    }

    #[test]
    fn parse_char_list_at_pattern() {
        let src = "ch @ ('a' | 'b' | 'c')";
        let pat_vec = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let pat = &pat_vec[0];
        if let Pattern::OrList(pat_list, name, pos) = &**pat {
            assert_eq!(name, &Some("ch".into()));
            assert_eq!(pos.line, 1);
            assert_eq!(pos.column, 1);
            assert_eq!(pat_list.len(), 3);

            if let Pattern::Char(ch, _name, _pos) = &*pat_list[0] {
                assert_eq!(*ch, 'a');
            } else {
                panic!("Expected a char pattern");
            }

            if let Pattern::Char(ch, _name, _pos) = &*pat_list[1] {
                assert_eq!(*ch, 'b');
            } else {
                panic!("Expected a char pattern");
            }

            if let Pattern::Char(ch, _name, _pos) = &*pat_list[2] {
                assert_eq!(*ch, 'c');
            } else {
                panic!("Expected a char pattern");
            }
        } else {
            panic!("Expected an OrList pattern");
        }
    }

    #[test]
    fn parse_str_pattern() {
        let src = "\"hello\"";
        let pat_vec = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let pat = &pat_vec[0];
        if let Pattern::Str(s, _name, pos) = &**pat {
            assert_eq!(s, "hello");
            assert_eq!(pos.line, 1);
            assert_eq!(pos.column, 1);
        } else {
            panic!("Expected a string pattern");
        }
    }

    #[test]
    fn parse_var_pattern() {
        let src = "some_var";
        let pat_vec = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let pat = &pat_vec[0];
        if let Pattern::Var(name, _opt_at_name, pos) = &**pat {
            assert_eq!(name, "some_var");
            assert_eq!(pos.line, 1);
            assert_eq!(pos.column, 1);
        } else {
            panic!("Expected a variable pattern");
        }
    }

    #[test]
    fn parse_char_or_pattern() {
        let src = "'a' | 'b'";
        let pat_vec = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 2);

        let pat = &pat_vec[0];
        if let Pattern::Char(ch, _name, pos) = &**pat {
            assert_eq!(*ch, 'a');
            assert_eq!(pos.line, 1);
            assert_eq!(pos.column, 1);
        } else {
            panic!("Expected a char pattern");
        }

        let pat2 = &pat_vec[1];
        if let Pattern::Char(ch, _name, pos) = &**pat2 {
            assert_eq!(*ch, 'b');
            assert_eq!(pos.line, 1);
            assert_eq!(pos.column, 5);
        } else {
            panic!("Expected a char pattern");
        }
    }

    #[test]
    fn parse_tuple_pattern() {
        let src = "$(1, 'a', \"Hello\")";
        let pat_vec= parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let pat = &pat_vec[0];
        if let Pattern::Tuple(patterns_list, _name, _pos) = &**pat {
            assert_eq!(patterns_list.len(), 3);

            let patterns1 = &patterns_list[0];
            let patterns2 = &patterns_list[1];
            let patterns3 = &patterns_list[2];

            assert_eq!(patterns1.len(), 1);
            assert_eq!(patterns2.len(), 1);
            assert_eq!(patterns3.len(), 1);

            let pat1 = &patterns1[0];
            if let Pattern::Number(num, _name, _pos) = &**pat1 {
                assert_eq!(*num, 1);
            } else {
                panic!("Expected a number pattern");
            }

            let pat2 = &patterns2[0];
            if let Pattern::Char(ch, _name, _pos) = &**pat2 {
                assert_eq!(*ch, 'a');
            } else {
                panic!("Expected a char pattern");
            }

            let pat3 = &patterns3[0];
            if let Pattern::Str(s, _name, _pos) = &**pat3 {
                assert_eq!(s, "Hello");
            } else {
                panic!("Expected a string pattern");
            }

        } else {
            panic!();
        }
    }

    #[test]
    fn parse_tuple_at_pattern() {
        let src = "outer @ $(x @ (1 | 2), inner @ $(y @ ('a' | 'b'), z @ ('c' | 'd'))";
        let pat_vec = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        // outer @ $(x @ (1 | 2), inner @ $(y @ ('a' | 'b'), z @ ('c' | 'd')))
        let pat = &pat_vec[0];
        if let Pattern::Tuple(patterns_list, name, _pos) = &**pat {
            assert_eq!(patterns_list.len(), 2);
            assert_eq!(name.as_ref().unwrap(), "outer");

            let patterns1 = &patterns_list[0];
            let patterns2 = &patterns_list[1];

            assert_eq!(patterns1.len(), 2);
            assert_eq!(patterns2.len(), 1);

            // x @ (1 | 2)
            let pat1 = &patterns1[0];
            if let Pattern::OrList(lst, list_name, _pos) = &**pat1 {
                assert_eq!(lst.len(), 2);
                assert_eq!(list_name.as_ref().unwrap(), "x");

                let pat1_1 = &lst[0];
                if let Pattern::Number(num, _name, _pos) = &**pat1_1 {
                    assert_eq!(*num, 1);
                } else {
                    panic!("Expected a number pattern");
                }

                let pat1_2 = &lst[1];
                if let Pattern::Number(num, _name, _pos) = &**pat1_2 {
                    assert_eq!(*num, 2);
                } else {
                    panic!("Expected a number pattern");
                }
            } else {
                panic!("Expected an OrList pattern");
            }

            // inner @ $(y @ ('a' | 'b'), z @ ('c' | 'd'))
            let pat2 = &patterns2[0];
            if let Pattern::Tuple(patterns_list2, name2, _pos) = &**pat2 {
                assert_eq!(patterns_list2.len(), 2);
                assert_eq!(name2.as_ref().unwrap(), "inner");

                // y @ ('a' | 'b')
                let item1 = &patterns_list2[0];
                assert_eq!(item1.len(), 1);
                let patterns1 = &item1[0];
                if let Pattern::OrList(lst, list_name, _pos) = &**patterns1 {
                    assert_eq!(lst.len(), 2);
                    assert_eq!(list_name.as_ref().unwrap(), "y");

                    let pat1_1 = &lst[0];
                    if let Pattern::Char(ch, _name, _pos) = &**pat1_1 {
                        assert_eq!(*ch, 'a');
                    } else {
                        panic!("Expected a char pattern");
                    }

                    let pat1_2 = &lst[1];
                    if let Pattern::Char(ch, _name, _pos) = &**pat1_2 {
                        assert_eq!(*ch, 'b');
                    } else {
                        panic!("Expected a char pattern");
                    }
                } else {
                    panic!("Expected an OrList pattern");
                }

                // z @ ('c' | 'd')
                let item2 = &patterns_list2[1];
                assert_eq!(item2.len(), 1);
                let patterns2 = &item2[0];
                if let Pattern::OrList(lst, list_name, _pos) = &**patterns2 {
                    assert_eq!(lst.len(), 2);
                    assert_eq!(list_name.as_ref().unwrap(), "z");

                    let pat2_1 = &lst[0];
                    if let Pattern::Char(ch, _name, _pos) = &**pat2_1 {
                        assert_eq!(*ch, 'c');
                    } else {
                        panic!("Expected a char pattern");
                    }

                    let pat2_2 = &lst[1];
                    if let Pattern::Char(ch, _name, _pos) = &**pat2_2 {
                        assert_eq!(*ch, 'd');
                    } else {
                        panic!("Expected a char pattern");
                    }
                } else {
                    panic!("Expected an OrList pattern");
                }
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
        let mut defs = Defines::new();
        let enum_def = EnumDefinition::new_tagged("EnumName".to_string(), Vec::new());
        defs.set_enum("EnumName", enum_def, None, &Position::new(0, 0)).unwrap();
        let pat_vec = parse_pattern_from_str_with_defs(src, &mut defs).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let pat = &pat_vec[0];
        if let Pattern::Enum(enum_pat, _name, _pos) = &**pat {
            match enum_pat {
                EnumPattern::Simple(_typ, name, sub_name) => {
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
        let mut defs = Defines::new();
        let enum_def = EnumDefinition::new_tagged("EnumName".to_string(), Vec::new());
        defs.set_enum("EnumName", enum_def, None, &Position::new(0, 0)).unwrap();
        let pat_vec = parse_pattern_from_str_with_defs(src, &mut defs).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let pat = &pat_vec[0];
        if let Pattern::Enum(enum_pat, _name, _pos) = &**pat {
            match enum_pat {
                EnumPattern::Tuple(_typ, name, sub_name, patterns_list) => {
                    assert_eq!(name, "EnumName");
                    assert_eq!(sub_name, "SubName");
                    assert_eq!(patterns_list.len(), 3);

                    let patterns1 = &patterns_list[0];
                    let patterns2 = &patterns_list[1];
                    let patterns3 = &patterns_list[2];

                    assert_eq!(patterns1.len(), 1);
                    assert_eq!(patterns2.len(), 1);
                    assert_eq!(patterns3.len(), 1);

                    let pat = &patterns1[0];
                    if let Pattern::Number(num, _name, _pos) = &**pat {
                        assert_eq!(*num, 1);
                    } else {
                        panic!("Expected a number pattern");
                    }

                    let pat = &patterns2[0];
                    if let Pattern::Char(ch, _name, _pos) = &**pat {
                        assert_eq!(*ch, 'a');
                    } else {
                        panic!("Expected a char pattern");
                    }

                    let pat = &patterns3[0];
                    if let Pattern::Str(s, _name, _pos) = &**pat {
                        assert_eq!(s, "Hello");
                    } else {
                        panic!("Expected a string pattern");
                    }
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
        let pat_vec = parse_pattern_from_str(src).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let pat = &pat_vec[0];
        if let Pattern::Struct(strct_pat, _name, _pos) = &**pat {
            let StructPattern {name, keys: list, map, has_optional} = strct_pat;
            assert_eq!(name, "Foo");
            assert_eq!(*has_optional, true);

            assert_eq!(list.len(), 2);
            assert_eq!(list[0], "x");
            assert_eq!(list[1], "y");

            assert_eq!(map.len(), 2);
            assert_eq!(map["x"], None);

            if let Some(vec) = &map["y"] {
                assert_eq!(vec.len(), 1);
                assert_eq!(*vec[0], Pattern::Number(0, None, Position::new(1, 6)));

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
        let mut defs = Defines::new();
        let enum_def = EnumDefinition::new_tagged("Some".to_string(), Vec::new());
        defs.set_enum("Some", enum_def, None, &Position::new(0, 0)).unwrap();
        let pat_vec = parse_pattern_from_str_with_defs(src, &mut defs).unwrap();

        assert_eq!(pat_vec.len(), 1);

        let pat = &pat_vec[0];
        if let Pattern::Enum(enum_pat, _name, _pos) = &**pat {
            match enum_pat {
                EnumPattern::Struct(_typ, name, sub_name, struct_pat) => {
                    assert_eq!(name, "Some");
                    assert_eq!(sub_name, "Foo");

                    let StructPattern {name, keys: list, map, has_optional} = struct_pat;
                    assert_eq!(name, "Some::Foo");
                    assert_eq!(*has_optional, false);

                    assert_eq!(list.len(), 3);
                    assert_eq!(list[0], "x");
                    assert_eq!(list[1], "y");
                    assert_eq!(list[2], "z");

                    assert_eq!(map.len(), 3);
                    assert_eq!(map["x"], None);

                    if let Some(vec) = &map["y"] {
                        assert_eq!(vec.len(), 1);
                        assert_eq!(*vec[0], Pattern::Number(0, None, Position::new(1, 6)));

                    }else{
                        panic!()
                    }

                    if let Some(vec) = &map["z"] {
                        assert_eq!(vec.len(), 2);
                        assert_eq!(*vec[0], Pattern::Char('a', None, Position::new(1, 16)));
                        assert_eq!(*vec[1], Pattern::Char('b', None, Position::new(1, 20)));

                    }else{
                        panic!()
                    }
                },
                _ => panic!(),
            }
        }
    }
}
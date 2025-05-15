mod common;

mod tests {
    use super::common::*;
    use std::rc::Rc;

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
        let mut defs = Defines::new();
        let list = vec![
            Enumerator::new_tuple("Some", vec![Rc::new(Type::TypeVariable("T".to_string()))]),
            Enumerator::new("None", 1),
        ];
        let enum_def = EnumDefinition::new_tagged("Option".to_string(), list);
        defs.set_enum("Some", enum_def, None, &Position::new(0, 0)).unwrap();
        let ast = parse_stmt_from_str_with_defs(src, &mut defs).unwrap().unwrap();

        if let AST::IfLet { pattern_list, pattern_name, expr, then, else_, pos: _ } = ast {
            //
            // check pattern_list
            //
            assert_eq!(pattern_list.len(), 1);
            assert_eq!(pattern_name, None);
            let (pat, _pos) = &pattern_list[0];
            if let Pattern::Enum(enum_pat) = &**pat {
                match enum_pat {
                    EnumPattern::Tuple(_typ, name, sub_name, patterns_list) => {
                        assert_eq!(name, "");
                        assert_eq!(sub_name, "Some");

                        let (patterns1, name1) = &patterns_list[0];

                        assert_eq!(patterns1.len(), 1);
                        assert_eq!(*name1, None);

                        let (pat, _pos) = &patterns1[0];
                        assert_eq!(**pat, Pattern::Var("value".to_string()));
                    },
                    _ => {
                        panic!()
                    }
                }
            }else{
                panic!()
            }

            //
            // check expr
            //
            if let ExprAST::Symbol(name, _pos) = *expr {
                assert_eq!(name, "variable");
            }else{
                panic!()
            }

            //
            // check then
            //
            if let AST::Block(blk, _) = *then {
                let list = blk.body;
                assert_eq!(list.len(), 1);

                if let AST::Expr(expr, _pos) = &list[0] {
                    if let ExprAST::Int(num, _) = **expr {
                        assert_eq!(num, 1);
                    }else{
                        panic!()
                    }

                }else{
                    panic!()
                }
            }else{
                panic!()
            }

            //
            // check else_
            //
            if let AST::Block(blk, _) = *else_.unwrap() {
                let list = blk.body;
                assert_eq!(list.len(), 1);

                if let AST::Expr(expr, _pos) = &list[0] {
                    if let ExprAST::Int(num, _) = **expr {
                        assert_eq!(num, 2);
                    }else{
                        panic!()
                    }

                }else{
                    panic!()
                }
            }else{
                panic!()
            }

        }else{
            panic!()
        }
    }

    #[test]
    fn parse_do_match() {
        let src = "
            do match (variable) {
                Some(value) => {
                    1;
                },
                None => {
                    2;
                },
                _ => {
                    3;
                }
            }
        ";
    
        // parse
        let mut defs = Defines::new();
        let list = vec![
            Enumerator::new_tuple("Some", vec![Rc::new(Type::TypeVariable("T".to_string()))]),
            Enumerator::new("None", 1),
        ];
        let enum_def = EnumDefinition::new_tagged("Option".to_string(), list);
        defs.set_enum("Some", enum_def, None, &Position::new(0, 0)).unwrap();
        let ast = parse_stmt_from_str_with_defs(src, &mut defs).unwrap().unwrap();

        if let AST::Match { expr, pattern_list_list, pos: _ } = ast {
            //
            // check expr
            //
            if let ExprAST::Symbol(name, _pos) = *expr {
                assert_eq!(name, "variable");
            }else{
                panic!()
            }

            //
            // check pattern_list_list
            //
            assert_eq!(pattern_list_list.len(), 3);

            //
            // check 1st pattern block
            //
            let ((pattern_list, _pattern_name), ast) = &pattern_list_list[0];

            // check pattern
            assert_eq!(pattern_list.len(), 1);
            let (pat, _pos) = &pattern_list[0];
            if let Pattern::Enum(enum_pat) = &**pat {
                match enum_pat {
                    EnumPattern::Tuple(_typ, name, sub_name, patterns_list) => {
                        assert_eq!(name, "");
                        assert_eq!(sub_name, "Some");

                        let (patterns1, name1) = &patterns_list[0];

                        assert_eq!(patterns1.len(), 1);
                        assert_eq!(*name1, None);

                        let (pat, _pos) = &patterns1[0];
                        assert_eq!(**pat, Pattern::Var("value".to_string()));
                    },
                    _ => {
                        panic!()
                    }
                }
            }else{
                panic!()
            }

            // check statement
            if let AST::Block(blk, _) = &**ast {
                let list = &blk.body;
                assert_eq!(list.len(), 1);

                if let AST::Expr(expr, _pos) = &list[0] {
                    if let ExprAST::Int(num, _) = **expr {
                        assert_eq!(num, 1);
                    }else{
                        panic!()
                    }

                }else{
                    panic!()
                }
            }else{
                panic!()
            }


            //
            // check 2nd pattern block
            //
            let ((pattern_list, _pattern_name), ast) = &pattern_list_list[1];

            // check pattern
            assert_eq!(pattern_list.len(), 1);
            let (pat, _pos) = &pattern_list[0];
            if let Pattern::Var(name) = &**pat {
                assert_eq!(name, "None");
            }else{
                panic!()
            }

            // check statement
            if let AST::Block(blk, _) = &**ast {
                let list = &blk.body;
                assert_eq!(list.len(), 1);

                if let AST::Expr(expr, _pos) = &list[0] {
                    if let ExprAST::Int(num, _) = **expr {
                        assert_eq!(num, 2);
                    }else{
                        panic!()
                    }

                }else{
                    panic!()
                }
            }else{
                panic!()
            }

            //
            // check 3rd pattern block
            //
            let ((pattern_list, _pattern_name), ast) = &pattern_list_list[2];

            // check pattern
            assert_eq!(pattern_list.len(), 1);
            let (pat, _pos) = &pattern_list[0];
            if let Pattern::Var(name) = &**pat {
                assert_eq!(name, "_");
            }else{
                panic!()
            }

            // check statement
            if let AST::Block(blk, _) = &**ast {
                let list = &blk.body;
                assert_eq!(list.len(), 1);

                if let AST::Expr(expr, _pos) = &list[0] {
                    if let ExprAST::Int(num, _) = **expr {
                        assert_eq!(num, 3);
                    }else{
                        panic!()
                    }

                }else{
                    panic!()
                }
            }else{
                panic!()
            }

        }else{
            panic!()
        }
    }
}
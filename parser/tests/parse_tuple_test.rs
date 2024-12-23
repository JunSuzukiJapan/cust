mod common;

mod tests {
    use super::common::*;
    use std::rc::Rc;

    #[test]
    fn parse_tuple() {
        let src = "$(1, 2, 3)";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        if let ExprAST::TupleLiteral(list, _pos1) = ast {
            assert_eq!(list.len(), 3);

            let ast = &*list[0];
            assert_eq!(
                *ast,
                ExprAST::Int(1, Position::new(1, 3))
            );

            let ast = &*list[1];
            assert_eq!(
                *ast,
                ExprAST::Int(2, Position::new(1, 6))
            );

            let ast = &*list[2];
            assert_eq!(
                *ast,
                ExprAST::Int(3, Position::new(1, 9))
            );

        }else{
            panic!()
        }
    }

    #[test]
    fn parse_tuple2() {
        let src = "$(1, 2, 3 + 4)";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        if let ExprAST::TupleLiteral(list, _pos1) = ast {
            assert_eq!(list.len(), 3);

            let ast = &*list[0];
            assert_eq!(
                *ast,
                ExprAST::Int(1, Position::new(1, 3))
            );

            let ast = &*list[1];
            assert_eq!(
                *ast,
                ExprAST::Int(2, Position::new(1, 6))
            );

            let ast = &*list[2];
            assert_eq!(
                *ast,
                ExprAST::BinExpr(
                    BinOp::Add,
                    Box::new(ExprAST::Int(3, Position::new(1, 9))),
                    Box::new(ExprAST::Int(4, Position::new(1, 13))),
                    Position::new(1, 11)
                )
            );

        }else{
            panic!()
        }
    }


    #[test]
    fn parse_tuple_type() {
        let src = "$<int, short int, char*>";

        let token_list = Tokenizer::tokenize(src).unwrap();
        let mut iter = token_list.iter().peekable();
        let parser = Parser::new();
        let mut defs = Defines::new();
        let mut labels = Vec::new();

        let (sq, type_or_variadic, _pos) = parser.parse_type_specifier_qualifier(&mut iter, &mut defs, &mut Some(&mut labels)).unwrap();
        let typ = type_or_variadic.get_type().unwrap();

        let ptr = Rc::as_ptr(&typ);
        if let Type::Tuple(list) = unsafe {&*ptr} {
            assert_eq!(list.len(), 3);

            let ptr1 = Rc::as_ptr(&list[0]);
            let t1 = unsafe {&*ptr1};
            assert_eq!(*t1, Type::Number(NumberType::Int));

            let ptr2 = Rc::as_ptr(&list[1]);
            let t2 = unsafe {&*ptr2};
            assert_eq!(*t2, Type::Number(NumberType::Short));

            let ptr3 = Rc::as_ptr(&list[2]);
            let t3 = unsafe {&*ptr3};
            assert_eq!(*t3, Type::Pointer(Pointer { is_const: false, is_volatile: false, next_pointer: None }, Box::new(Rc::new(Type::Number(NumberType::Char)))));

        }else{
            panic!();
        }

        assert_eq!(sq,  SpecifierQualifier {
            auto: false,
            register: false,
            static_: false,
            extern_: false,
            typedef: false,
            const_: false,
            volatile: false,
        });
    }

    #[test]
    fn parse_tuple_index() {
        let src = "$(1, 3.0, \"hello\").0";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        if let ExprAST::TupleMemberAccess(ast2, index, _pos) = ast {
            assert_eq!(index, 0);

            if let ExprAST::TupleLiteral(list, _pos1) = *ast2 {

                assert_eq!(list.len(), 3);

                let ast = &*list[0];
                assert_eq!(
                    *ast,
                    ExprAST::Int(1, Position::new(1, 3))
                );

                let ast = &*list[1];
                assert_eq!(
                    *ast,
                    ExprAST::Double(3.0, Position::new(1, 6))
                );

                let ast = &*list[2];
                assert_eq!(
                    *ast,
                    ExprAST::StringLiteral("hello".into(), Position::new(1, 11))
                );

            }else{
                panic!()
            }

        }else{
            panic!()
        }
    }

    #[test]
    fn parse_tuple_index2() {
        let src = "$(1, 3.0, $(2, 3, 4)).2 .1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        if let ExprAST::TupleMemberAccess(ast2, index, _pos) = ast {
            assert_eq!(index, 1);

            if let ExprAST::TupleMemberAccess(ast3, index, _pos) = *ast2 {
                assert_eq!(index, 2);

                if let ExprAST::TupleLiteral(list, _pos1) = *ast3 {

                    assert_eq!(list.len(), 3);

                    let ast = &*list[0];
                    assert_eq!(
                        *ast,
                        ExprAST::Int(1, Position::new(1, 3))
                    );

                    let ast = &*list[1];
                    assert_eq!(
                        *ast,
                        ExprAST::Double(3.0, Position::new(1, 6))
                    );

                    if let ExprAST::TupleLiteral(list, _pos1) = &*list[2] {
                        assert_eq!(list.len(), 3);

                        let ast = &*list[0];
                        assert_eq!(
                            *ast,
                            ExprAST::Int(2, Position::new(1, 13))
                        );

                        let ast = &*list[1];
                        assert_eq!(
                            *ast,
                            ExprAST::Int(3, Position::new(1, 16))
                        );

                        let ast = &*list[2];
                        assert_eq!(
                            *ast,
                            ExprAST::Int(4, Position::new(1, 19))
                        );

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
    fn parse_global_define_tuple() {
        let src = "$<int, int> tpl = $(1, 2);";

        // parse
        let ast = parse_external_declaration_from_str(src).unwrap().unwrap();

        if let ToplevelAST::GlobalDefVar { specifiers, declaration, pos } = ast {






        }else{
            panic!()
        }
    }


    #[test]
    fn parse_tuple_pointer_index() {
        let src = "ptr->2";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        if let ExprAST::TuplePointerAccess(ast2, index, _pos) = ast {
            assert_eq!(index, 2);

            if let ExprAST::Symbol(name, _pos) = &*ast2 {
                assert_eq!(name, "ptr");
            }else{
                panic!()
            }

        }else{
            panic!()
        }
    }

}
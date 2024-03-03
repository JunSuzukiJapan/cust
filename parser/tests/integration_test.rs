mod common;

mod tests {
    use super::common::*;

    #[test]
    fn parse_constant() {
        let src = "123";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::Int(123, Position::new(1, 1))
        );
    }

    #[test]
    fn parse_primary_expression() {
        let src = "(1, 2, 3)";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::BinExpr(
                BinOp::Comma,
                Box::new(ExprAST::Int(1, Position::new(1, 2))),
                Box::new(ExprAST::BinExpr(
                    BinOp::Comma,
                    Box::new(ExprAST::Int(2, Position::new(1, 5))),
                    Box::new(ExprAST::Int(3, Position::new(1, 8))),
                    Position::new(1, 6)
                )),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_add() {
        let src = "10 + 11";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::BinExpr(
                BinOp::Add,
                Box::new(ExprAST::Int(10, Position::new(1, 1))),
                Box::new(ExprAST::Int(11, Position::new(1, 6))),
                Position::new(1, 4)
            )
        );
    }

    #[test]
    fn parse_sub() {
        let src = "10 - 11";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::BinExpr(
                BinOp::Sub,
                Box::new(ExprAST::Int(10, Position::new(1, 1))),
                Box::new(ExprAST::Int(11, Position::new(1, 6))),
                Position::new(1, 4)
            )
        );
    }

    #[test]
    fn parse_mul() {
        let src = "10 * 11";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::BinExpr(
                BinOp::Mul,
                Box::new(ExprAST::Int(10, Position::new(1, 1))),
                Box::new(ExprAST::Int(11, Position::new(1, 6))),
                Position::new(1, 4)
            )
        );
    }

    #[test]
    fn parse_div() {
        let src = "10 / 11";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::BinExpr(
                BinOp::Div,
                Box::new(ExprAST::Int(10, Position::new(1, 1))),
                Box::new(ExprAST::Int(11, Position::new(1, 6))),
                Position::new(1, 4)
            )
        );
    }

    #[test]
    fn parse_mod() {
        let src = "10 % 11";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::BinExpr(
                BinOp::Mod,
                Box::new(ExprAST::Int(10, Position::new(1, 1))),
                Box::new(ExprAST::Int(11, Position::new(1, 6))),
                Position::new(1, 4)
            )
        );
    }

    #[test]
    fn parse_complex_expr() {
        let src = "10 + 11 * 12 - 13";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::BinExpr(
                BinOp::Sub,
                Box::new(
                    ExprAST::BinExpr(
                        BinOp::Add,
                        Box::new(ExprAST::Int(10, Position::new(1, 1))),
                        Box::new(
                            ExprAST::BinExpr(
                                BinOp::Mul,
                                Box::new(ExprAST::Int(11, Position::new(1, 6))),
                                Box::new(ExprAST::Int(12, Position::new(1, 11))),
                                Position::new(1, 9)
                            )
                        ),
                        Position::new(1, 4)
                    )
                ),
                Box::new(ExprAST::Int(13, Position::new(1, 16))),
                Position::new(1, 14)
            )
        );
    }

    #[test]
    fn parse_unary_plus() {
        let src = "+5";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::Int(5, Position::new(1, 2))
        );
    }

    #[test]
    fn parse_unary_minus() {
        let src = "-5";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::UnaryMinus(Box::new(ExprAST::Int(5, Position::new(1, 2))), Position::new(1, 1))
        );
    }

    #[test]
    fn parse_unary_tilda() {
        let src = "~5";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::UnaryTilda(Box::new(ExprAST::Int(5, Position::new(1, 2))), Position::new(1, 1))
        );
    }

    #[test]
    fn parse_unary_not() {
        let src = "! 0";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::Not(Box::new(ExprAST::Int(0, Position::new(1, 3))), Position::new(1, 1))
        );
    }

    #[test]
    fn parse_unary_paren() {
        let src = "-(1 + 2 * 3)";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::UnaryMinus(
                Box::new(
                    ExprAST::BinExpr(
                        BinOp::Add,
                        Box::new(ExprAST::Int(1, Position::new(1, 3))),
                        Box::new(
                            ExprAST::BinExpr(
                                BinOp::Mul,
                                Box::new(ExprAST::Int(2, Position::new(1, 7))),
                                Box::new(ExprAST::Int(3, Position::new(1, 11))),
                                Position::new(1, 9)
                            )
                        ),
                        Position::new(1, 5)
                    )
                ),
                Position::new(1, 1)
            )
        );
    }

    #[test]
    fn parse_postfix() {
        let src = "x++";
        let ast = parse_expression_from_str(src).unwrap().unwrap();
        assert_eq!(
            ast,
            ExprAST::PostInc("x".to_string(), Position::new(1, 1), Position::new(1, 2))
        );

        let src = "x--";
        let ast = parse_expression_from_str(src).unwrap().unwrap();
        assert_eq!(
            ast,
            ExprAST::PostDec("x".to_string(), Position::new(1, 1), Position::new(1, 2))
        );
    }

    #[test]
    fn parse_prefix() {
        let src = "++x";
        let ast = parse_expression_from_str(src).unwrap().unwrap();
        assert_eq!(
            ast,
            ExprAST::PreInc("x".to_string(), Position::new(1, 3), Position::new(1, 1))
        );

        let src = "--x";
        let ast = parse_expression_from_str(src).unwrap().unwrap();
        assert_eq!(
            ast,
            ExprAST::PreDec("x".to_string(), Position::new(1, 3), Position::new(1, 1))
        );
    }

    #[test]
    fn parse_add_assign() {
        let src = "x += 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::Add,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 6))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_sub_assign() {
        let src = "x -= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::Sub,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 6))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_mul_assign() {
        let src = "x *= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::Mul,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 6))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_div_assign() {
        let src = "x /= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::Div,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 6))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_mod_assign() {
        let src = "x %= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::Mod,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 6))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_shift_left_assign() {
        let src = "x <<= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::ShiftLeft,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 7))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_shift_right_assign() {
        let src = "x >>= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::ShiftRight,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 7))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_bit_and_assign() {
        let src = "x &= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::BitAnd,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 6))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_bit_or_assign() {
        let src = "x |= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::BitOr,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 6))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_bit_xor_assign() {
        let src = "x ^= 1";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::OpAssign(
                BinOp::BitXor,
                Box::new(ExprAST::Symbol("x".to_string(), Position::new(1, 1))),
                Box::new(ExprAST::Int(1, Position::new(1, 6))),
                Position::new(1, 3)
            )
        );
    }

    #[test]
    fn parse_increment() {
        let src = "x++;";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::PostInc(
                "x".to_string(),
                Position::new(1, 1),
                Position::new(1, 2)
            )            
        );
    }

    #[test]
    fn parse_decrement() {
        let src = "x--;";
        let ast = parse_expression_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::PostDec(
                "x".to_string(),
                Position::new(1, 1),
                Position::new(1, 2)
            )            
        );
    }

    #[test]
    fn parse_type_specifier_qualifier() {
        let src = "int volatile short unsigned * volatile * const foo(";

        let token_list = Tokenizer::tokenize(src).unwrap();
        let mut iter = token_list.iter().peekable();
        let parser = Parser::new();
        let mut defs = Defines::new();
        let mut labels = Vec::new();

        let (sq, type_or_variadic, _pos) = parser.parse_type_specifier_qualifier(&mut iter, &mut defs, &mut Some(&mut labels)).unwrap();
        let typ = type_or_variadic.get_type().unwrap();
        let pointer = parser.parse_pointer(&mut iter, &mut defs);
        let pointer = pointer.unwrap().unwrap();

        assert_eq!(typ, &Type::Number(NumberType::UnsignedShort));
        assert_eq!(sq,  SpecifierQualifier {
            auto: false,
            register: false,
            static_: false,
            extern_: false,
            typedef: false,
            const_: false,
            volatile: true,
        });
        assert_eq!(
            pointer.make_type_to(&typ),
            Type::Pointer(
                Pointer::new(false, true),
                Box::new(Type::Pointer(
                    Pointer::new(true, false),
                    Box::new(Type::Number(NumberType::UnsignedShort))
                ))
            )
        )
    }

    #[test]
    fn parse_function_prototype() -> Result<(), ParserError> {
        let src = "
            int add(int x, int y);
        ";
        let ast = parse_external_declaration_from_str(src).unwrap().unwrap();

        match ast {
            AST::FunProto(FunProto {specifiers, declarator, params}, _pos) => {
                let name = declarator.get_name();
                let ret_type = specifiers.get_type();
                assert_eq!(*name, "add".to_string());
                assert_eq!(*ret_type, Type::Number(NumberType::Int));
                assert_eq!(params.len(), 2);

                let sq = SpecifierQualifier::new();
                let mut defs = Defines::new();
                let ds_x = DeclarationSpecifier::new(Type::Number(NumberType::Int), sq.clone());
                let decl_x = Declarator::new(None, DirectDeclarator::Symbol(String::from("x"), Position::new(2, 24)));
                let ds_y = DeclarationSpecifier::new(Type::Number(NumberType::Int), sq.clone());
                let decl_y = Declarator::new(None, DirectDeclarator::Symbol(String::from("y"), Position::new(2, 31)));
                let pos = Position::new(2, 25);
                assert_eq!(params, Params::from_vec(vec![(ds_x, decl_x), (ds_y, decl_y)], false, &mut defs, &pos)?);
            },
            _ => panic!("ast: {:?}", ast),
        }

        Ok(())
    }

    #[test]
    fn parse_function() -> Result<(), ParserError> {
        let src = "
            int add(int x, int y) {
                return x + y;
            }
        ";
        let ast = parse_external_declaration_from_str(src).unwrap().unwrap();

        match ast {
            AST::Function(Function {specifiers, declarator, params, body, labels: _}, _pos) => {
                let name = declarator.get_name();
                let ret_type = specifiers.get_type();
                assert_eq!(*name, "add".to_string());
                assert_eq!(*ret_type, Type::Number(NumberType::Int));
                assert_eq!(params.len(), 2);

                let sq = SpecifierQualifier::new();
                let mut defs = Defines::new();
                let ds_x = DeclarationSpecifier::new(Type::Number(NumberType::Int), sq.clone());
                let decl_x = Declarator::new(None, DirectDeclarator::Symbol(String::from("x"), Position::new(2, 24)));
                let ds_y = DeclarationSpecifier::new(Type::Number(NumberType::Int), sq.clone());
                let decl_y = Declarator::new(None, DirectDeclarator::Symbol(String::from("y"), Position::new(2, 31)));
                let pos = Position::new(2, 25);
                assert_eq!(params, Params::from_vec(vec![(ds_x, decl_x), (ds_y, decl_y)], false, &mut defs, &pos)?);
                assert_eq!(body,
                    Block {
                        body: vec![
                            AST::Return(Some(Box::new(
                                ExprAST::BinExpr(
                                    BinOp::Add,
                                    Box::new(ExprAST::Symbol(String::from("x"), Position::new(3, 23))),
                                    Box::new(ExprAST::Symbol(String::from("y"), Position::new(3, 27))),
                                    Position::new(3, 25)
                                ))),
                                Position::new(3, 16)
                            )
                        ],
                    }
                );
            },
            _ => panic!("ast: {:?}", ast),
        }

        Ok(())
    }

    #[test]
    fn parse_multiple_declaration() -> Result<(), ParserError> {
        let src = "
            int foo() {
                int x, y = 2, z;
                x = 1;
                z = 3;
                return x + y * z;
            }
        ";
        let ast = parse_external_declaration_from_str(src).unwrap().unwrap();

        match ast {
            AST::Function(Function {specifiers, declarator, params, body, labels: _}, _pos) => {
                let name = declarator.get_name();
                let ret_type = specifiers.get_type();
                assert_eq!(*name, "foo".to_string());
                assert_eq!(*ret_type, Type::Number(NumberType::Int));
                assert_eq!(params.len(), 0);

                let mut defs = Defines::new();
                let pos = Position::new(1, 1);
                assert_eq!(params, Params::from_vec(vec![], false, &mut defs, &pos)?);

                let sq = SpecifierQualifier::new();
                let specifier = DeclarationSpecifier::new(Type::Number(NumberType::Int), sq);

                let mut v = Vec::new();
                let declarator = Declarator::new(None, DirectDeclarator::Symbol(String::from("x"), Position::new(3, 20)));
                let declaration = Declaration::new(declarator, None);
                v.push(declaration);
                let declarator = Declarator::new(None, DirectDeclarator::Symbol(String::from("y"), Position::new(3, 23)));
                let init_expr = ExprAST::Int(2, Position::new(3, 27));
                let declaration = Declaration::new(declarator, Some(Initializer::Simple(init_expr, Position::new(3, 27))));
                v.push(declaration);
                let declarator = Declarator::new(None, DirectDeclarator::Symbol(String::from("z"), Position::new(3, 30)));
                let declaration = Declaration::new(declarator, None);
                v.push(declaration);

                assert_eq!(body,
                    Block {
                        body: vec![
                            AST::DefVar{
                                specifiers: specifier,
                                declarations: v,
                                pos: Position::new(3, 31),
                            },
                            AST::Expr(
                                Box::new(ExprAST::Assign(
                                    Box::new(ExprAST::Symbol(String::from("x"), Position::new(4, 16))),
                                    Box::new(ExprAST::Int(1, Position::new(4, 20))),
                                    Position::new(4, 18)
                                )),
                                Position::new(4, 16),
                            ),
                            AST::Expr(
                                Box::new(ExprAST::Assign(
                                    Box::new(ExprAST::Symbol(String::from("z"), Position::new(5, 16))),
                                    Box::new(ExprAST::Int(3, Position::new(5, 20))),
                                    Position::new(5, 18)
                                )),
                                Position::new(5, 16)
                            ),
                            AST::Return(Some(Box::new(
                                ExprAST::BinExpr(
                                    BinOp::Add,
                                    Box::new(ExprAST::Symbol(String::from("x"), Position::new(6, 23))),
                                    Box::new(ExprAST::BinExpr(
                                        BinOp::Mul,
                                        Box::new(ExprAST::Symbol(String::from("y"), Position::new(6, 27))),
                                        Box::new(ExprAST::Symbol(String::from("z"), Position::new(6, 31))),
                                        Position::new(6, 29)
                                    )),
                                    Position::new(6, 25)
                                ))),
                                Position::new(6, 16)
                            )
                        ],
                    }
                );
            },
            _ => panic!("ast: {:?}", ast),
        }

        Ok(())
    }

    #[test]
    fn parse_multiple_pointer_declaration() -> Result<(), ParserError> {
        let src = "
            void foo() {
                int x, *y, **z;
            }
        ";
        let ast = parse_external_declaration_from_str(src).unwrap().unwrap();

        match ast {
            AST::Function(Function {specifiers, declarator, params, body, labels: _}, _pos) => {
                let name = declarator.get_name();
                let ret_type = specifiers.get_type();
                assert_eq!(*name, "foo".to_string());
                assert_eq!(*ret_type, Type::Void);
                assert_eq!(params.len(), 0);

                let mut defs = Defines::new();
                let pos = Position::new(1, 1);
                assert_eq!(params, Params::from_vec(vec![], false, &mut defs, &pos)?);

                let sq = SpecifierQualifier::new();
                let specifier = DeclarationSpecifier::new(Type::Number(NumberType::Int), sq);

                let mut v = Vec::new();
                let declarator = Declarator::new(None, DirectDeclarator::Symbol(String::from("x"), Position::new(3, 20)));
                let declaration = Declaration::new(declarator, None);
                v.push(declaration);

                let pointer = Pointer::new(false, false);
                let typ = Type::Number(NumberType::Int);
                assert_eq!(
                    pointer.make_type_to(&typ),
                    Type::Pointer(Pointer::new(false, false), Box::new(typ.clone())));
                let declarator = Declarator::new(Some(pointer.clone()), DirectDeclarator::Symbol(String::from("y"), Position::new(3, 24)));
                let declaration = Declaration::new(declarator, None);
                v.push(declaration);

                let handle = Pointer::new_with_next_pointer(false, false, pointer);
                assert_eq!(
                    handle.make_type_to(&typ),
                    Type::Pointer(
                        Pointer::new(false, false),
                        Box::new(Type::Pointer(
                            Pointer::new(false, false),
                            Box::new(Type::Number(NumberType::Int))
                        ))
                    )
                );
                let declarator = Declarator::new(Some(handle), DirectDeclarator::Symbol(String::from("z"), Position::new(3, 29)));
                let declaration = Declaration::new(declarator, None);
                v.push(declaration);

                assert_eq!(body,
                    Block {
                        body: vec![
                            AST::DefVar{
                                specifiers: specifier,
                                declarations: v,
                                pos: Position::new(3, 30)
                            }
                        ],
                    }
                );
            },
            _ => panic!("ast: {:?}", ast),
        }

        Ok(())
    }

    #[test]
    fn parse_struct() -> Result<(), ParserError> {
        let src = "
            struct date {
                int year, month;
                int day;
            };

            typedef struct date Date;

            void foo() {
                Date date;

                date.year = 2022;
                date.month = 12;
                date.day = 31;

                Date* pointer = &date;
                pointer->year = 2023;
                pointer->month = 1;
                pointer->day = 1;
            }
        ";
        let list = parse_translation_unit_from_str(src).unwrap();

        assert_eq!(list.len(), 3);

        //
        // test first statement
        //
        if let AST::DefineStruct {name: Some(id), fields, pos: _} = &list[0] {
            assert_eq!(*id, "date".to_string());

            let fields = fields.get_fields().unwrap();

            assert_eq!(fields.len(), 3);

            let sq = SpecifierQualifier::new();
            let field1 = StructField::NormalField {
                name: Some("year".to_string()),
                sq: sq.clone(),
                typ: Type::Number(NumberType::Int)
            };
            assert_eq!(fields[0], field1);

            let field2 = StructField::NormalField {
                name: Some("month".to_string()),
                sq: sq.clone(),
                typ: Type::Number(NumberType::Int)
            };
            assert_eq!(fields[1], field2);

            let field3 = StructField::NormalField {
                name: Some("day".to_string()),
                sq: sq.clone(),
                typ: Type::Number(NumberType::Int)
            };
            assert_eq!(fields[2], field3);

        }else{
            panic!();
        }

        //
        // test 2nd statement
        //
        if let AST::TypeDef(name, Type::Struct {name: Some(id), fields}, _pos) = &list[1] {
            assert_eq!(*name, "Date".to_string());
            assert_eq!(*id, "date".to_string());
            assert!(fields.has_fields());

        }else{
            panic!("");
        }

        //
        // test 3rd statement
        //
        if let AST::Function(Function {specifiers, declarator, params, body, labels: _}, _pos) = &list[2] {
            let name = declarator.get_name();
            let ret_type = specifiers.get_type();
            assert_eq!(*name, "foo".to_string());
            assert_eq!(*ret_type, Type::Void);
            assert_eq!(params.len(), 0);

            let mut defs = Defines::new();
            let pos = Position::new(1, 1);
            assert_eq!(*params, Params::from_vec(vec![], false, &mut defs, &pos)?);

            let list = &body.body;
            assert_eq!(list.len(), 8);

            //
            // Date date;
            //
            let sq = SpecifierQualifier::new();
            let int_type = Type::Number(NumberType::Int);
            let mut field_list: Vec<StructDeclaration> = Vec::new();

            // member 'year'
            let declarator = StructDeclarator::new(Some(Declarator::new(None, DirectDeclarator::Symbol(String::from("year"), Position::new(3, 20)))), None);
            let declarator_list = vec![declarator];
            field_list.push(StructDeclaration::new(sq.clone(), Some(int_type.clone()), declarator_list));
            // member 'month'
            let declarator = StructDeclarator::new(Some(Declarator::new(None, DirectDeclarator::Symbol(String::from("month"), Position::new(3, 26)))), None);
            let declarator_list = vec![declarator];
            field_list.push(StructDeclaration::new(sq.clone(), Some(int_type.clone()), declarator_list));
            // member 'day'
            let declarator = StructDeclarator::new(Some(Declarator::new(None, DirectDeclarator::Symbol(String::from("day"), Position::new(4, 20)))), None);
            let declarator_list = vec![declarator];
            field_list.push(StructDeclaration::new(sq.clone(), Some(int_type.clone()), declarator_list));

            let dummy_pos = Position::new(1, 1);
            let struct_definition = StructDefinition::try_new(Some("date".to_string()), Some(field_list), &dummy_pos)?;
            let type_struct = Type::Struct { name: Some("date".to_string()), fields: struct_definition.clone() };
            let specifier = DeclarationSpecifier::new(type_struct.clone(), sq);

            let mut v = Vec::new();
            let declarator = Declarator::new(None, DirectDeclarator::Symbol(String::from("date"), Position::new(10, 21)));
            let declaration = Declaration::new(declarator, None);
            v.push(declaration);

            assert_eq!(
                list[0],
                AST::DefVar{
                    specifiers: specifier,
                    declarations: v,
                    pos: Position::new(10, 25),
                }
            );
            // date.year = 2022;
            // date.month = 12;
            // date.day = 31;
            assert_eq!(
                list[1],
                AST::Expr(Box::new(
                    ExprAST::Assign(
                        Box::new(ExprAST::MemberAccess(
                            Box::new(ExprAST::Symbol(String::from("date"), Position::new(12, 16))),
                            String::from("year"),
                            Position::new(12, 20)
                        )),
                        Box::new(ExprAST::Int(2022, Position::new(12, 28))),
                        Position::new(12, 26)
                    )),
                    Position::new(12, 16)
                )
            );
            assert_eq!(
                list[2],
                AST::Expr(Box::new(
                    ExprAST::Assign(
                        Box::new(ExprAST::MemberAccess(
                            Box::new(ExprAST::Symbol(String::from("date"), Position::new(13, 16))),
                            String::from("month"),
                            Position::new(13, 20)
                        )),
                        Box::new(ExprAST::Int(12, Position::new(13, 29))),
                        Position::new(13, 27)
                    )),
                    Position::new(13, 16)
                )
            );
            assert_eq!(
                list[3],
                AST::Expr(Box::new(
                    ExprAST::Assign(
                        Box::new(ExprAST::MemberAccess(
                            Box::new(ExprAST::Symbol(String::from("date"), Position::new(14, 16))),
                                String::from("day"),
                                Position::new(14, 20)
                        )),
                        Box::new(ExprAST::Int(31, Position::new(14, 27))),
                        Position::new(14, 25)
                    )),
                    Position::new(14, 16)
                )
            );

            // Date* pointer = &date;
            if let AST::DefVar { specifiers, declarations: declaration, pos: _ } = &list[4] {
                let typ = specifiers.get_type();
                assert_eq!(*typ, type_struct.clone());


                let sq = SpecifierQualifier::new();
                let specifiers2: DeclarationSpecifier = DeclarationSpecifier::new(typ.clone(), sq);
                assert_eq!(*specifiers, specifiers2);

                assert_eq!(declaration.len(), 1);

                let declarator = declaration[0].get_declarator();
                assert_eq!(
                    *declarator,
                    Declarator::new(
                        Some(Pointer::new(false, false)),
                        DirectDeclarator::Symbol(String::from("pointer"), Position::new(16, 22))
                    )
                );

                if let Some(expr) = declaration[0].get_init_expr() {
                    assert_eq!(
                        *expr,
                        Initializer::Simple(ExprAST::UnaryGetAddress(
                            Box::new(ExprAST::Symbol(String::from("date"), Position::new(16, 33))),
                            Position::new(16, 32)
                        ), Position::new(16, 32))
                    );

                }else{
                    panic!()
                }

            }else{
                panic!();
            }

            // pointer->year = 2023;
            // pointer->month = 1;
            // pointer->day = 1;
            assert_eq!(
                list[5],
                AST::Expr(Box::new(
                    ExprAST::Assign(
                        Box::new(ExprAST::PointerAccess(
                            Box::new(ExprAST::Symbol(String::from("pointer"), Position::new(17, 16))),
                            String::from("year"),
                            Position::new(17, 23)
                        )),
                        Box::new(ExprAST::Int(2023, Position::new(17, 32))),
                        Position::new(17, 30)
                    )),
                    Position::new(17, 16)
                )
            );
            assert_eq!(
                list[6],
                AST::Expr(Box::new(
                    ExprAST::Assign(
                        Box::new(ExprAST::PointerAccess(
                            Box::new(ExprAST::Symbol(String::from("pointer"), Position::new(18, 16))),
                            String::from("month"),
                            Position::new(18, 23)
                        )),
                        Box::new(ExprAST::Int(1, Position::new(18, 33))),
                        Position::new(18, 31)
                    )),
                    Position::new(18, 16)
                )
            );
            assert_eq!(
                list[7],
                AST::Expr(Box::new(
                    ExprAST::Assign(
                        Box::new(ExprAST::PointerAccess(
                            Box::new(ExprAST::Symbol(String::from("pointer"), Position::new(19, 16))),
                            String::from("day"),
                            Position::new(19, 23)
                        )),
                        Box::new(ExprAST::Int(1, Position::new(19, 31))),
                        Position::new(19, 29)
                    )),
                    Position::new(19, 16)
                )
            );

        }else{
            panic!();
        }

        Ok(())
    }

    #[test]
    fn parse_define_array() -> Result<(), ParserError> {
        let src = "
            int ary[2][3];
        ";
        let ast = parse_external_declaration_from_str(src).unwrap().unwrap();

        match ast {
            AST::GlobalDefVar { specifiers: DeclarationSpecifier {typ, specifier_qualifier}, declaration, pos: _ } => {
                assert_eq!(typ, Type::Number(NumberType::Int));
                assert_eq!(specifier_qualifier, SpecifierQualifier::new());

                assert_eq!(declaration.len(), 1);
                let decl = &declaration[0];

                let init_expr = decl.get_init_expr();
                assert_eq!(*init_expr, None);

                let declarator = decl.get_declarator();
                assert_eq!(*declarator.get_pointer(), None);
                let direct_decl = declarator.get_direct_declarator();
                assert_eq!(direct_decl.get_name(), "ary");

                match direct_decl {
                    DirectDeclarator::ArrayDef(second_direct_decl, size_list, _pos) => {
                        assert_eq!(*size_list, vec![2, 3]);
                        assert_eq!(**second_direct_decl, DirectDeclarator::Symbol("ary".to_string(), Position::new(2, 16)));
                },
                    _ => panic!("direct_decl: {:?}", direct_decl),
                }

                Ok(())
            },
            _ => panic!("ast: {:?}", ast),
        }
    }

    #[test]
    fn parse_parened_expr() -> Result<(), ParserError> {
        let src = "
            int x = (1 + 1);
        ";
        let ast = parse_external_declaration_from_str(src).unwrap().unwrap();

        match ast {
            AST::GlobalDefVar { specifiers: DeclarationSpecifier {typ, specifier_qualifier}, declaration, pos: _ } => {
                assert_eq!(typ, Type::Number(NumberType::Int));
                assert_eq!(specifier_qualifier, SpecifierQualifier::new());

                assert_eq!(declaration.len(), 1);
                let decl = &declaration[0];

                let init_expr = decl.get_init_expr();
                assert_eq!(*init_expr,
                    Some(Initializer::Simple(ExprAST::BinExpr(
                        BinOp::Add,
                        Box::new(ExprAST::Int(1, Position::new(2, 21))),
                        Box::new(ExprAST::Int(1, Position::new(2, 25))),
                        Position::new(2, 23)
                    ), Position::new(2, 20)))
                );

                let declarator = decl.get_declarator();
                assert_eq!(*declarator.get_pointer(), None);
                let direct_decl = declarator.get_direct_declarator();
                assert_eq!(direct_decl.get_name(), "x");

                match direct_decl {
                    DirectDeclarator::Symbol(name, pos) => {
                        assert_eq!(name, "x");
                        assert_eq!(*pos, Position::new(2, 16))
                    },
                    _ => panic!("direct_decl: {:?}", direct_decl),
                }

                Ok(())
            }, 
            _ => panic!("ast: {:?}", ast),
        }
    }

    #[test]
    fn parse_type_cast() -> Result<(), ParserError> {
        let src = "
            int* ptr = (int*)malloc(1);
        ";
        let ast = parse_external_declaration_from_str(src).unwrap().unwrap();

        match ast {
            AST::GlobalDefVar { specifiers: DeclarationSpecifier {typ, specifier_qualifier}, declaration, pos: _ } => {
                let int_pointer_type = Type::Pointer(
                    Pointer {
                        is_const: false,
                        is_volatile: false,
                        next_pointer: None,
                    },
                    Box::new(Type::Number(NumberType::Int)));

                assert_eq!(typ, Type::Number(NumberType::Int));
                assert_eq!(specifier_qualifier, SpecifierQualifier::new());

                assert_eq!(declaration.len(), 1);
                let decl = &declaration[0];
                let declarator = decl.get_declarator();
                let ptr = declarator.get_pointer();
                let name = declarator.get_name();
                assert_eq!(*ptr, Some(Pointer {is_const: false, is_volatile: false, next_pointer: None}));
                assert_eq!(name, "ptr");
                let direct_decl = declarator.get_direct_declarator();
                assert_eq!(direct_decl.get_name(), "ptr");

                let init_expr = decl.get_init_expr();
                assert_eq!(*init_expr,
                    Some(Initializer::Simple(ExprAST::Cast(
                        int_pointer_type,
                        Box::new(ExprAST::CallFunction(
                            Box::new(ExprAST::Symbol("malloc".to_string(), Position::new(2, 29))),
                            vec![ExprAST::Int(1, Position::new(2, 36))], Position::new(2, 35))
                        ),
                        Position::new(2, 23)
                    ), Position::new(2, 23)))
                );

                Ok(())
            }, 
            _ => panic!("ast: {:?}", ast),
        }
    }
}

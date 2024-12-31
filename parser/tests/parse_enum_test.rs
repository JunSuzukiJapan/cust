mod common;

mod tests {
    use super::common::*;
    use crate::common::StructDefinition;
    use parser::ast::TupleLiteral;
    use std::{ops::Deref, rc::Rc};

    #[test]
    fn parse_enum() {
        let src = "enum Foo { A, B, C };";
        let ast = parse_translation_unit_from_str(src).unwrap();

        if let ToplevelAST::DefineEnum { name, fields, pos: _ } = &ast[0] {
            assert_eq!(name, "Foo");

            let fields = fields.get_fields();
            assert_eq!(fields.len(), 3);
            assert_eq!(fields[0], Enumerator::new("A", 0));
            assert_eq!(fields[1], Enumerator::new("B", 1));
            assert_eq!(fields[2], Enumerator::new("C", 2));

        }else{
            panic!()
        }
    }

    #[test]
    fn parse_enum2a() {
        let src = "
            enum Foo {
                Bar,
                Zot<int, int>,
            };
        ";
        let ast = parse_translation_unit_from_str(src).unwrap();

        if let ToplevelAST::DefineEnum { name, fields, pos: _ } = &ast[0] {
            assert_eq!(name, "Foo");

            let fields = fields.get_fields();
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0], Enumerator::new("Bar", 0));
            assert_eq!(fields[1], Enumerator::new_tuple("Zot", vec![Rc::new(Type::Number(NumberType::Int)), Rc::new(Type::Number(NumberType::Int))]));

        }else{
            panic!()
        }
    }

    #[test]
    fn parse_enum2b() {
        let src = "
            enum Foo {
                Bar,
                Zot(int, int),
            };
        ";
        let ast = parse_translation_unit_from_str(src).unwrap();

        if let ToplevelAST::DefineEnum { name, fields, pos: _ } = &ast[0] {
            assert_eq!(name, "Foo");

            let fields = fields.get_fields();
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0], Enumerator::new("Bar", 0));
            assert_eq!(fields[1], Enumerator::new_tuple("Zot", vec![Rc::new(Type::Number(NumberType::Int)), Rc::new(Type::Number(NumberType::Int))]));

        }else{
            panic!()
        }
    }

    #[test]
    fn parse_enum3() {
        let src = "
            enum Foo {
                Bar,
                Zot<int, int>,
                Hoge{int x; int y;},
            };
        ";
        let ast = parse_translation_unit_from_str(src).unwrap();

        if let ToplevelAST::DefineEnum { name, fields, pos: _ } = &ast[0] {
            assert_eq!(name, "Foo");

            let fields = fields.get_fields();
            assert_eq!(fields.len(), 3);
            assert_eq!(fields[0], Enumerator::new("Bar", 0));
            assert_eq!(fields[1], Enumerator::new_tuple("Zot", vec![Rc::new(Type::Number(NumberType::Int)), Rc::new(Type::Number(NumberType::Int))]));

            if let Enumerator::TypeStruct {name, struct_type} = &fields[2] {
                assert_eq!(name, "Hoge");

                if let Type::Struct { name, fields } = &**struct_type {
                    assert_eq!(name, &None);

                    assert_eq!(fields.get_name(), &None);

                    if let Some(fields) = fields.get_fields() {
                        let field1 = &fields[0];
                        assert_eq!(field1.is_normal(), true);
                        assert_eq!(field1.get_name().as_ref().unwrap(), "x");
                        assert_eq!(field1.get_type().unwrap().deref(), &Type::Number(NumberType::Int));

                        let field2 = &fields[1];
                        assert_eq!(field2.is_normal(), true);
                        assert_eq!(field2.get_name().as_ref().unwrap(), "y");
                        assert_eq!(field2.get_type().unwrap().deref(), &Type::Number(NumberType::Int));

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
    fn parse_tuple_type_enum() {
        let src = "
            enum Foo {
                Bar,
                Zot(int, int),
            };

            int main() {
                enum Foo x = Foo::Zot(1, 2);

                return 0;
            }
        ";
        let ast = parse_translation_unit_from_str(src).unwrap();
        assert_eq!(ast.len(), 2);

        if let ToplevelAST::DefineEnum { name, fields, pos: _ } = &ast[0] {
            assert_eq!(name, "Foo");

            let fields = fields.get_fields();
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0], Enumerator::new("Bar", 0));
            assert_eq!(fields[1], Enumerator::new_tuple("Zot", vec![Rc::new(Type::Number(NumberType::Int)), Rc::new(Type::Number(NumberType::Int))]));

        }else{
            panic!()
        }

        if let ToplevelAST::Function(Function { specifiers, declarator, params, body, labels }, _pos) = &ast[1] {
            assert_eq!(declarator.get_name(), "main");
            assert_eq!(params.len(), 0);

            if let AST::DefVar { specifiers: _, declarations, pos: _ } = &body.body[0] {
                assert_eq!(declarations.len(), 1);
                let decl = declarations.get(0).unwrap();
                assert_eq!(decl.get_declarator().get_name(), "x");

                if let Initializer::Simple(expr, _pos) = decl.get_init_expr().as_ref().unwrap() {
                    if let ExprAST::EnumLiteral(typ, index, literal, _pos) = expr {
                        // check index
                        assert_eq!(*index, 1);

                        // check typ
                        if let Type::Enum { name, enum_def } = &**typ {
                            assert_eq!(name, "Foo");
                            assert_eq!(enum_def.is_tagged(), true);

                            let fields = enum_def.get_fields();
                            assert_eq!(fields.len(), 2);
                            assert_eq!(fields[0], Enumerator::new("Bar", 0));
                            assert_eq!(
                                fields[1],
                                Enumerator::new_tuple("Zot", vec![Rc::new(Type::Number(NumberType::Int)), Rc::new(Type::Number(NumberType::Int))])
                            );


                        }else{
                            panic!()
                        }

                        // check literal
                        let tuple_literal = literal.get_tuple_literal().unwrap();
                        if let TupleLiteral::ConstLiteral { typ, list, pos: _ } = tuple_literal {
                            assert_eq!(typ.deref(), &Type::Tuple(vec![Rc::new(Type::Number(NumberType::Int)), Rc::new(Type::Number(NumberType::Int))]));

                            assert_eq!(list.len(), 2);
                            assert_eq!(list[0], ConstExpr::Int(1, Position::new(10, 31)));
                            assert_eq!(list[1], ConstExpr::Int(2, Position::new(10, 34)));

                        }else {
                            panic!()
                        }

                    }else{
                        panic!()
                    }
                }else{
                    panic!()
                };

            }else{
                panic!()
            }

            if let AST::Return(expr, _pos) = &body.body[1] {
                assert_eq!(&**expr.as_ref().unwrap(), &ExprAST::Int(0, Position::new(10, 23)));

            }else{
                panic!()
            }
        }else{
            panic!()
        }
    }

    #[test]
    fn parse_struct_type_enum() {
        let src = "
            enum Foo {
                Bar,
                Zot {
                    int x;
                    int y;
                },
            };

            int main() {
                enum Foo x = Foo::Zot {
                    x: 1;
                    y: 2;
                };

                return 0;
            }
        ";
        let ast = parse_translation_unit_from_str(src).unwrap();
        assert_eq!(ast.len(), 2);

        let struct_def = StructDefinition::try_new(
            None,
            Some(vec![
                StructDeclaration::new(
                    SpecifierQualifier::default(),
                    Some(Rc::new(Type::Number(NumberType::Int))),
                    vec![
                        StructDeclarator::new(
                            Some(
                                Declarator::new(
                                    None,
                                    DirectDeclarator::Symbol("x".to_string(), Position::new(5, 17))
                                )
                            ),
                            None
                        ),
                    ]
                ),
                StructDeclaration::new(
                    SpecifierQualifier::default(),
                    Some(Rc::new(Type::Number(NumberType::Int))),
                    vec![
                        StructDeclarator::new(
                            Some(
                                Declarator::new(
                                    None,
                                    DirectDeclarator::Symbol("y".to_string(), Position::new(5, 17))
                                )
                            ),
                            None
                        ),
                    ]
                ),
            ]),
            &Position::new(5, 17)
        ).unwrap();

        if let ToplevelAST::DefineEnum { name, fields, pos: _ } = &ast[0] {
            assert_eq!(name, "Foo");

            let fields = fields.get_fields();
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0], Enumerator::new("Bar", 0));
            assert_eq!(
                fields[1],
                Enumerator::new_struct(
                    "Zot",
                    struct_def.clone()
                )
            );

        }else{
            panic!()
        }

        if let ToplevelAST::Function(Function { specifiers, declarator, params, body, labels }, _pos) = &ast[1] {
            assert_eq!(declarator.get_name(), "main");
            assert_eq!(params.len(), 0);

            if let AST::DefVar { specifiers: _, declarations, pos: _ } = &body.body[0] {
                assert_eq!(declarations.len(), 1);
                let decl = declarations.get(0).unwrap();
                assert_eq!(decl.get_declarator().get_name(), "x");

                if let Initializer::Simple(expr, _pos) = decl.get_init_expr().as_ref().unwrap() {
                    if let ExprAST::EnumLiteral(typ, index, literal, _pos) = expr {
                        // check index
                        assert_eq!(*index, 1);

                        // check typ
                        if let Type::Enum { name, enum_def } = &**typ {
                            assert_eq!(name, "Foo");
                            assert_eq!(enum_def.is_tagged(), true);

                            let fields = enum_def.get_fields();
                            assert_eq!(fields.len(), 2);
                            assert_eq!(fields[0], Enumerator::new("Bar", 0));
                            assert_eq!(
                                fields[1],
                                Enumerator::new_struct(
                                    "Zot",
                                    struct_def.clone()
                                )
                            );


                        }else{
                            panic!()
                        }

                        // check literal
                        let struct_literal = literal.get_struct_literal().unwrap();
                        if let StructLiteral::ConstLiteral (typ, map, _pos)= struct_literal {
                            assert_eq!(typ.deref(), &Type::Struct {
                                name: None,
                                fields: struct_def
                            });

                            assert_eq!(map.len(), 2);
                            assert_eq!(map.get("x").unwrap(), &ConstExpr::Int(1, Position::new(10, 31)));
                            assert_eq!(map.get("y").unwrap(), &ConstExpr::Int(2, Position::new(10, 34)));

                        }else {
                            panic!()
                        }

                    }else{
                        panic!()
                    }
                }else{
                    panic!()
                };

            }else{
                panic!()
            }

            if let AST::Return(expr, _pos) = &body.body[1] {
                assert_eq!(&**expr.as_ref().unwrap(), &ExprAST::Int(0, Position::new(16, 23)));

            }else{
                panic!()
            }
        }else{
            panic!()
        }
    }
}
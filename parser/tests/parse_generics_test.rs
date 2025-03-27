mod common;

mod tests {
    use super::common::*;
    use crate::common::StructDefinition;
    use parser::ast::TupleLiteral;
    use core::panic;
    use std::{ops::Deref, rc::Rc};


    #[test]
    fn parse_enum_generics() {
        let src = "
            enum Option<T> {
                Some<T>,
                None,
            };

            int test(){
                enum Option<int> opt = Some(10);
                if let (Some(x) = opt) {
                    return x;
                }

                return 0;
            }
        ";
        let ast = parse_translation_unit_from_str(src).unwrap();

        assert_eq!(ast.len(), 2);

        if let ToplevelAST::DefineEnum { name, fields, type_variables, pos: _ } = &ast[0] {
            assert_eq!(name, "Option");

            let fields = fields.get_fields();
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0], Enumerator::new_tuple("Some", vec![Rc::new(Type::TypeVariable("T".to_string()))]));
            assert_eq!(fields[1], Enumerator::new("None", 1));

            let type_variables = type_variables.as_ref().unwrap();
            assert_eq!(type_variables.len(), 1);
            assert_eq!(type_variables[0], "T");

        }else{
            panic!()
        }

        if let ToplevelAST::Function(Function { specifiers: _, declarator, params, body, labels }, _pos) = &ast[1] {
            assert_eq!(declarator.get_name(), "test");
            assert_eq!(params.len(), 0);
            assert_eq!(body.body.len(), 3);
            assert_eq!(labels.len(), 0);

            //
            // enum Option<int> opt = Some(10);
            //
            if let AST::DefVar { specifiers: _, declarations, pos: _ } = &body.body[0] {
                assert_eq!(declarations.len(), 1);
                let decl = declarations.get(0).unwrap();
                assert_eq!(decl.get_declarator().get_name(), "opt");

                if let Initializer::Simple(expr, _pos) = decl.get_init_expr().as_ref().unwrap() {
                    if let ExprAST::CallFunction(func, args, _pos) = expr {
                        let (name, _pos) = func.get_symbol().unwrap();
                        assert_eq!(name, "Some");
                        assert_eq!(args.len(), 1);
                        assert_eq!(args[0], ExprAST::Int(10, Position::new(8, 44)));

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
            // if let (Some(x) = opt) {
            //     return x;
            // }
            //
            if let AST::IfLet { pattern_list, pattern_name, expr, then, else_, pos: _ } = &body.body[1] {
                assert_eq!(pattern_list.len(), 1);
                assert!(pattern_name.is_none());
                assert_eq!(expr.deref(), &ExprAST::Symbol("opt".to_string(), Position::new(9, 34)));
                assert!(else_.is_none());

                //
                // Some(x)
                //
                let (pat, _pos) = &pattern_list[0];
                if let Pattern::Enum(EnumPattern::Tuple(name, variant, fields)) = &**pat {
                    assert_eq!(name, "");
                    assert_eq!(variant, "Some");
                    assert_eq!(fields.len(), 1);
                    assert_eq!(fields[0].0.len(), 1);
                    assert_eq!(fields[0].0[0].0.deref(), &Pattern::Var("x".to_string()));

                }else{
                    panic!()
                }

                // {
                //     return x;
                // }
                if let AST::Block(body, _pos) = &**then {
                    assert_eq!(body.body.len(), 1);

                    if let AST::Return(expr_ast, _pos) = &body.body[0] {
                        assert_eq!(&**expr_ast.as_ref().unwrap(), &ExprAST::Symbol("x".to_string(), Position::new(10, 27)));
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
            // return 0;
            //
            if let AST::Return(expr_ast, _pos) = &body.body[2] {
                assert_eq!(&**expr_ast.as_ref().unwrap(), &ExprAST::Int(0, Position::new(13, 23)));
            }else{
                panic!()
            }

        }else{
            panic!()
        }
    }


    #[test]
    fn parse_struct_generics() {
        let src = "
            struct Foo<T, U> {
                T bar;
                U baz;
            };

            int test(){
                struct Foo<int, char> opt = {10, 'a'};

                return opt.bar;
            }
        ";
        let ast = parse_translation_unit_from_str(src).unwrap();

        assert_eq!(ast.len(), 2);

        if let ToplevelAST::DefineStruct { name, fields, type_variables, pos: _ } = &ast[0] {
            assert_eq!(name.as_ref().unwrap(), "Foo");

            let fields = fields.get_fields().unwrap();
            assert_eq!(fields.len(), 2);

            let field1 = &fields[0];
            assert!(field1.is_normal());
            assert_eq!(field1.get_name().as_ref().unwrap(), "bar");
            assert_eq!(field1.get_type().unwrap(), &Rc::new(Type::TypeVariable("T".to_string())));

            let field2 = &fields[1];
            assert!(field2.is_normal());
            assert_eq!(field2.get_name().as_ref().unwrap(), "baz");
            assert_eq!(field2.get_type().unwrap(), &Rc::new(Type::TypeVariable("U".to_string())));

            let type_variables = type_variables.as_ref().unwrap();
            assert_eq!(type_variables.len(), 2);
            assert_eq!(type_variables[0], "T");
            assert_eq!(type_variables[1], "U");

        }else{
            panic!()
        }

        if let ToplevelAST::Function(Function { specifiers: _, declarator, params, body, labels }, _pos) = &ast[1] {
            assert_eq!(declarator.get_name(), "test");
            assert_eq!(params.len(), 0);
            assert_eq!(body.body.len(), 2);
            assert_eq!(labels.len(), 0);

            //
            // struct Foo<int, char> opt = {10, 'a'};
            //
            if let AST::DefVar { specifiers: _, declarations, pos: _ } = &body.body[0] {
                assert_eq!(declarations.len(), 1);
                let decl = declarations.get(0).unwrap();
                assert_eq!(decl.get_declarator().get_name(), "opt");

                if let Initializer::Struct(expr, _typ, _pos) = decl.get_init_expr().as_ref().unwrap() {
                    assert_eq!(expr.len(), 2);
                    let expr1 = expr[0].as_ref();
                    let expr2 = expr[1].as_ref();

                    if let Initializer::Simple(expr, _pos) = expr1 {
                        if let ExprAST::Int(val, _pos) = expr {
                            assert_eq!(*val, 10);
                        }else{
                            panic!()
                        }
                    }else{
                        panic!()
                    }

                    if let Initializer::Simple(expr, _pos) = expr2 {
                        if let ExprAST::Char(val, _pos) = expr {
                            assert_eq!(*val, 'a');
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

            //
            // return opt.bar;
            //
            if let AST::Return(expr_ast, _pos) = &body.body[1] {
                if let ExprAST::MemberAccess(receiver, msg, _pos) = &**expr_ast.as_ref().unwrap() {
                    if let ExprAST::Symbol(name, _pos) = &**receiver {
                        assert_eq!(name, "opt");
                    }else{
                        panic!()
                    }

                    assert_eq!(msg, "bar");

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
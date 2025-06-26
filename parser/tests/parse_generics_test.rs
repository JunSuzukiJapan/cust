mod common;

mod tests {
    use super::common::*;
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
                enum Option<int> opt = Option::Some(10);
                if let (Option::Some(x) = opt) {
                    return x;
                }

                return 0;
            }
        ";
        let mut defs = Defines::new();
        let list = vec![
            Enumerator::new_tuple("Some", vec![Rc::new(Type::TypeVariable("T".to_string()))]),
            Enumerator::new("None", 1),
        ];
        let enum_def = EnumDefinition::new_tagged("Option".to_string(), list);
        defs.set_enum("Option", enum_def, None, &Position::new(0, 0)).unwrap();
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
                    if let ExprAST::EnumLiteral(typ, 0, _literal, pos) = expr {
                        let enum_def = typ.get_enum_definition().unwrap();
                        assert_eq!(enum_def.get_name(), "Option");

                        assert_eq!(pos, &Position::new(8, 39));

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
            if let AST::IfLet { pattern_list, expr, then, else_, pos: _ } = &body.body[1] {
                assert_eq!(pattern_list.len(), 1);
                assert_eq!(expr.deref(), &ExprAST::Symbol("opt".to_string(), Position::new(9, 42)));
                assert!(else_.is_none());

                //
                // Some(x)
                //
                let pat = &pattern_list[0];
                if let Pattern::Enum(EnumPattern::Tuple(_typ, name, variant, fields), _name, _pos) = &**pat {
                    assert_eq!(name, "Option");
                    assert_eq!(variant, "Some");
                    assert_eq!(fields.len(), 1);
                    assert_eq!(fields[0].len(), 1);
                    assert_eq!(fields[0][0].deref(), &Pattern::Var("x".to_string(), None, Position::new(9, 37)));

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

}
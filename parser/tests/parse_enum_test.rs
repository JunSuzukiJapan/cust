mod common;

mod tests {
    use super::common::*;
    use crate::common::StructDefinition;
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
}
mod common;

mod tests {
    use super::common::*;
    use std::rc::Rc;

    #[test]
    fn parse_enum() {
        let src = "enum Foo { A, B, C };";
        let ast = parse_translation_unit_from_str(src).unwrap();

        if let ToplevelAST::DefineEnum { name, fields, pos } = &ast[0] {
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
}
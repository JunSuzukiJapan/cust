mod common;

mod tests {
    use super::common::*;
    use std::rc::Rc;

    #[test]
    fn parse_const_char_pointer() -> Result<(), ParserError> {
        let src = "
            int add(const char* const str1, const char* str2, char* const str3) {
                return 0;
            }
        ";
        let ast = parse_external_declaration_from_str(src).unwrap().unwrap();

        match ast {
            ToplevelAST::Function(Function { specifiers: _, declarator: _, params, body: _, labels: _ }, _pos) => {
                let params = params.get_params();

                let param1 = &params[0];
                let param2 = &params[1];
                let param3 = &params[2];

                // param 1
                let type1 = param1.get_type();
                assert_eq!(type1.is_pointer(), true);
                let ptr1 = type1.get_pointer().unwrap();
                assert_eq!(ptr1.is_const, true);
                assert_eq!(ptr1.is_volatile, false);
                let pointee = type1.peel_off_pointer().unwrap();
                assert_eq!(pointee.is_char(), true);
                // assert_eq!(pointee.is_const(), true);
                assert_eq!(param1.is_const(), true);
                assert_eq!(param1.is_volatile(), false);

                // parama2
                let type2 = param2.get_type();
                assert_eq!(type2.is_pointer(), true);
                let ptr2 = type2.get_pointer().unwrap();
                assert_eq!(ptr2.is_const, false);
                assert_eq!(ptr2.is_volatile, false);
                let pointee = type2.peel_off_pointer().unwrap();
                assert_eq!(pointee.is_char(), true);
                assert_eq!(param2.is_const(), true);
                assert_eq!(param2.is_volatile(), false);

                // param 3
                let type3 = param3.get_type();
                assert_eq!(type3.is_pointer(), true);
                let ptr3 = type3.get_pointer().unwrap();
                assert_eq!(ptr3.is_const, true);
                assert_eq!(ptr3.is_volatile, false);
                let pointee = type3.peel_off_pointer().unwrap();
                assert_eq!(pointee.is_char(), true);
                assert_eq!(param3.is_const(), false);
                assert_eq!(param3.is_volatile(), false);
            },
            _ => panic!(),
        }

        Ok(())
    }
}
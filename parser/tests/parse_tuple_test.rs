mod common;

mod tests {
    use super::common::*;

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
}
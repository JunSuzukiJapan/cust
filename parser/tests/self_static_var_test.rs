mod common;

mod tests {
    use super::common::*;

    #[test]
    fn parse_static_var() {
        let src = "
            struct foo {
                int bar;
            };

            impl foo {
                int test() {
                    return 0;
                }
            }
        ";
/*
        let src = "
            int printf(char* format, ...);

            typedef struct foo {
                int x;
            } Foo;

            int test(int x){
                Foo foo;

                foo.x = x;
                int y = ++foo.x;
                return foo.x + y;
            }
        ";
*/
        let list = parse_from_str(src).unwrap();
        assert_eq!(list.len(), 2);

        // panic!("ast: {:?}", list[1]);
    }

}
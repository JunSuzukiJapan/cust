extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;

#[test]
fn code_gen_do_match_int() {
    let src = "
        int test(int input){
            int output = 0;

            do match (input) {
                0 => {
                    output = 1;
                },
                1 => {
                    output = 2;
                },
                2 => {
                    output = 3;
                },
                _ => {
                    output = 4;
                }
            }

            return output;
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    for i in 0..asts.len() {
        let _any_value = gen.gen_toplevel(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(0) }, 1);
    assert_eq!(unsafe { f.call(1) }, 2);
    assert_eq!(unsafe { f.call(2) }, 3);
    assert_eq!(unsafe { f.call(3) }, 4);
    assert_eq!(unsafe { f.call(10) }, 4);
}

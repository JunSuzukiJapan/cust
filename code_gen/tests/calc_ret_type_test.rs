extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;


#[test]
fn code_gen_for_ret_type() -> Result<(), CodeGenError> {
    let src = "
        int printf(char* format, ...);

        int test() {
            for(int i = 0; i < 3; i++){
                return 2;
            }
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

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, 2);

    Ok(())
}

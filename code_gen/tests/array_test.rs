extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;


#[test]
fn code_gen_simple_array() -> Result<(), CodeGenError> {
    let src = "
        int printf(char* format, ...);

        int test() {
            int ary[3];

            for(int i = 0; i < 3; i++){
                ary[i] = i;
            }

            int sum = 0;
            for(int i = 0; i < 3; i++){
                sum += ary[i];
            }

            return sum;
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
    assert_eq!(unsafe { f.call() }, 0 + 1 + 2);

    Ok(())
}

#[test]
fn code_gen_array() -> Result<(), CodeGenError> {
    let src = "
        int printf(char* format, ...);

        int test() {
            int ary[2][3];

            for(int i = 0; i < 2; i++){
                for(int j = 0; j < 3; j++){
                    ary[i][j] = i * 10 + j;
                }
            }

            printf(\"ary[0][0] = %d\\n\", ary[0][0]);
            printf(\"ary[0][1] = %d\\n\", ary[0][1]);
            printf(\"ary[0][2] = %d\\n\", ary[0][2]);
            printf(\"ary[1][0] = %d\\n\", ary[1][0]);
            printf(\"ary[1][1] = %d\\n\", ary[1][1]);
            printf(\"ary[1][2] = %d\\n\", ary[1][2]);

            int sum = 0;
            for(int i = 0; i < 2; i++){
                for(int j = 0; j < 3; j++){
                    sum += ary[i][j];
                }
            }

            return sum;
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
    assert_eq!(unsafe { f.call() }, 0 + 1 + 2 + 10 + 11 + 12);

    Ok(())
}

#[test]
fn code_gen_init_array1() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        int test(){
            int num[3] = {1, 2, 3};

            return num[0] + num[1] + num[2];
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    for i in 0..asts.len() {
        let _any_value = gen.gen_toplevel(&asts[i], &mut env, None, None)?;
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    let result = unsafe { f.call() };
    assert_eq!(6, result);

    Ok(())
}

#[test]
fn code_gen_init_array2() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        int test() {
            int num[2][3] = {{1, 2, 3},
                             {4, 5, 6}};

            return num[0][0] + num[0][1] + num[0][2]
                 + num[1][0] + num[1][1] + num[1][2];
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    for i in 0..asts.len() {
        let _any_value = gen.gen_toplevel(&asts[i], &mut env, None, None)?;
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    let result = unsafe { f.call() };
    assert_eq!(21, result);

    Ok(())
}

#[test]
fn code_gen_init_global_array2() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        int num[2][3] = {{1, 2, 3},
                         {4, 5, 6}};

        int test() {

            return num[0][0] + num[0][1] + num[0][2]
                 + num[1][0] + num[1][1] + num[1][2];
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    for i in 0..asts.len() {
        let _any_value = gen.gen_toplevel(&asts[i], &mut env, None, None)?;
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    let result = unsafe { f.call() };
    assert_eq!(21, result);

    Ok(())
}

#[test]
fn code_gen_array_and_pointer() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        int num[2][2] = {{1, 2}, {3, 4}};

        int test() {
            int* ptr = num[0];
            int* ptr2 = num;

            return ptr[0] + ptr[1] + ptr[2] + ptr[3]
                 + ptr2[0] + ptr2[1] + ptr2[2] + ptr2[3];
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    for i in 0..asts.len() {
        let _any_value = gen.gen_toplevel(&asts[i], &mut env, None, None)?;
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    let result = unsafe { f.call() };
    assert_eq!(20, result);

    Ok(())
}

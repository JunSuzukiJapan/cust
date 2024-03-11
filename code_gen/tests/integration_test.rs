extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;


#[test]
fn code_gen_formula() -> Result<(), Box<dyn Error>> {
    let src = "10 + 11";
    assert_eq!(code_gen_from_str(src)?, 21);

    let src = "10 - 11";
    assert_eq!(code_gen_from_str(src)?, -1);

    let src = "2 * 3";
    assert_eq!(code_gen_from_str(src)?, 6);

    let src = "7 / 2";
    assert_eq!(code_gen_from_str(src)?, 3);

    let src = "7 % 2";
    assert_eq!(code_gen_from_str(src)?, 1);

    let src = "-777";
    assert_eq!(code_gen_from_str(src)?, -777);

    let src = "-(1 + 2 * 3)";
    assert_eq!(code_gen_from_str(src)?, -7);

    let src = "1 && 0";
    assert_eq!(code_gen_from_str(src)?, 0);

    let src = "0 && 1";
    assert_eq!(code_gen_from_str(src)?, 0);

    let src = "1 && 1";
    assert!(code_gen_from_str(src)? != 0);

    let src = "0 && 0";
    assert_eq!(code_gen_from_str(src)?, 0);

    let src = "1 || 1";
    assert!(code_gen_from_str(src)? != 0);

    let src = "1 || 0";
    assert!(code_gen_from_str(src)? != 0);

    let src = "0 || 1";
    assert!(code_gen_from_str(src)? != 0);

    let src = "0 || 0";
    assert_eq!(code_gen_from_str(src)?, 0);

    let src = "1 == 1";
    assert!(code_gen_from_str(src)? != 0);

    let src = "0 == 1";
    assert_eq!(code_gen_from_str(src)?, 0);

    let src = "1 == 0";
    assert_eq!(code_gen_from_str(src)?, 0);

    let src = "1 != 1";
    assert_eq!(code_gen_from_str(src)?, 0);

    let src = "1 != 0";
    assert!(code_gen_from_str(src)? != 0);

    let src = "0 != 1";
    assert!(code_gen_from_str(src)? != 0);

    let src = "0 < 1";
    assert!(code_gen_from_str(src)? != 0);

    let src = "1 < 0";
    assert_eq!(code_gen_from_str(src)?, 0);

    let src = "1 < 1";
    assert_eq!(code_gen_from_str(src)?, 0);

    let src = "0 <= 1";
    assert!(code_gen_from_str(src)? != 0);

    let src = "1 <= 0";
    assert_eq!(code_gen_from_str(src)?, 0);

    let src = "1 <= 1";
    assert!(code_gen_from_str(src)? != 0);

    let src = "0 > 1";
    assert_eq!(code_gen_from_str(src)?, 0);

    let src = "1 > 0";
    assert!(code_gen_from_str(src)? != 0);

    let src = "1 > 1";
    assert_eq!(code_gen_from_str(src)?, 0);

    let src = "0 >= 1";
    assert_eq!(code_gen_from_str(src)?, 0);

    let src = "1 >= 0";
    assert!(code_gen_from_str(src)? != 0);

    let src = "1 >= 1";
    assert!(code_gen_from_str(src)? != 0);

    let src = "!0";
    assert!(code_gen_from_str(src)? != 0);

    let src = "!1";
    assert_eq!(code_gen_from_str(src)?, 0);

    let src =        "~0b00000000_11111111_00000000_11111111";
    let result = code_gen_from_str(src)? as u32;
    assert_eq!(result, 0b11111111_00000000_11111111_00000000_u32);

    Ok(())
}

#[test]
fn code_gen_fun_id() {
    let src = "
        int id(int x){
            return x;
        }
    ";

    // parse
    let ast = &parse_from_str(src).unwrap()[0];

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    let _any_value = gen.gen_stmt(&ast, &mut env, None, None).unwrap();

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("id").ok().unwrap() };
    let result = unsafe { f.call(11) };
    assert_eq!(result, 11);
}

#[test]
fn code_gen_fun_add() {
    // parse
    let ast = &parse_from_str("
        int add(int x, int y) {
            return x + y;
        }
    ").unwrap()[0];

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    let _any_value = gen.gen_stmt(&ast, &mut env, None, None).unwrap();

    let f: JitFunction<FuncType_i32i32_i32> = unsafe { gen.execution_engine.get_function("add").ok().unwrap() };
    let result = unsafe { f.call(2, 3) };

    assert_eq!(result, 5);
}

#[test]
fn code_gen_fun_add3() {
    let src = "
        int add3(int x, int y, int z){
            return x + y + z;
        }
    ";

    // parse
    let ast = &parse_from_str(src).unwrap()[0];

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    let _any_value = gen.gen_stmt(&ast, &mut env, None, None).unwrap();

    let f: JitFunction<FuncType_i32i32i32_i32> = unsafe { gen.execution_engine.get_function("add3").ok().unwrap() };
    let result = unsafe { f.call(3, 4, 5) };
    assert_eq!(result, 12);
}

#[test]
fn code_gen_fun_add_mul() {
    let src = "
        int add_mul(int x, int y, int z){
            return x + y * z;
        }
    ";

    // parse
    let ast = &parse_from_str(src).unwrap()[0];

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    let _any_value = gen.gen_stmt(&ast, &mut env, None, None).unwrap();

    let f: JitFunction<FuncType_i32i32i32_i32> = unsafe { gen.execution_engine.get_function("add_mul").ok().unwrap() };
    let result = unsafe { f.call(3, 4, 5) };
    assert_eq!(result, 23);
}

#[test]
fn code_gen_fun_helloworld() -> Result<(), Box<dyn Error>> {
    let src = "
        int printf(char* format, ...);

        void hello_world(){
            printf(\"Hello, World!\\\n\");
            return;
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();

    for i in 0..asts.len() {
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None)?.unwrap();
    }

    let f: JitFunction<FuncType_void_void> = unsafe { gen.execution_engine.get_function("hello_world").ok().unwrap() };
    let _result = unsafe { f.call() };

    Ok(())
}

#[test]
fn code_gen_if() {
    let src = "
        int printf(char* format, ...);

        int test(int x){
            if(x){
                return 10;
            }else{
                return 20;
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
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(0) }, 20);
    assert_eq!(unsafe { f.call(1) }, 10);
}

#[test]
fn code_gen_add_assign() {
    let src = "
        int printf(char* format, ...);

        int test(int x){
            x += 2;
            return x;
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();

    for i in 0..asts.len() {
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(0) }, 2);
    assert_eq!(unsafe { f.call(1) }, 3);
}

#[test]
fn code_gen_sub_assign() {
    let src = "
        int printf(char* format, ...);

        int test(int x){
            x -= 2;
            return x;
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();

    for i in 0..asts.len() {
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(0) }, -2);
    assert_eq!(unsafe { f.call(10) }, 8);
}

#[test]
fn code_gen_mul_assign() {
    let src = "
        int printf(char* format, ...);

        int test(int x){
            x *= 2;
            return x;
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();

    for i in 0..asts.len() {
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(0) }, 0);
    assert_eq!(unsafe { f.call(5) }, 10);
}

#[test]
fn code_gen_div_assign() {
    let src = "
        int printf(char* format, ...);

        int test(int x){
            x /= 2;
            return x;
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();

    for i in 0..asts.len() {
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(10) }, 5);
    assert_eq!(unsafe { f.call(11) }, 5);
}

#[test]
fn code_gen_mod_assign() {
    let src = "
        int printf(char* format, ...);

        int test(int x, int y){
            x %= y;
            return x;
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();

    for i in 0..asts.len() {
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_i32i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(10, 3) }, 1);
    assert_eq!(unsafe { f.call(11, 3) }, 2);
}

#[test]
fn code_gen_pre_increment() {
    let src = "
        int printf(char* format, ...);

        int test(int x){
            int y = ++x;
            return x + y;
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();

    for i in 0..asts.len() {
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(0) }, 2);
    assert_eq!(unsafe { f.call(1) }, 4);
}

#[test]
fn code_gen_post_increment() {
    let src = "
        int test(int x){
            int y = x++;
            return x + y;
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();

    for i in 0..asts.len() {
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(0) }, 1);
    assert_eq!(unsafe { f.call(1) }, 3);
}

#[test]
fn code_gen_pre_decrement() {
    let src = "
        int test(int x){
            int y = --x;
            return x + y;
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();

    for i in 0..asts.len() {
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(0) }, -2);
    assert_eq!(unsafe { f.call(1) }, 0);
}

#[test]
fn code_gen_post_decrement() {
    let src = "
        int test(int x){
            int y = x--;
            return x + y;
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();

    for i in 0..asts.len() {
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(0) }, -1);
    assert_eq!(unsafe { f.call(1) }, 1);
}

#[test]
fn code_gen_multiple_pointer_declaration() -> Result<(), CodeGenError> {
    let src = "
        int test() {
            int x, *y, **z;

            x = 3;
            y = &x;
            z = &y;

            *y = 4;
            **z = 5;

            return x;
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();

    for i in 0..asts.len() {
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, 5);

    Ok(())
}

#[test]
fn code_gen_switch() {
    let src = "
        int test(int x){
            switch(x){
            case 0:
                x += 1000;
            case 1:
                x += 20000;
                break;
            case 2:
                x += 300000;
                break;
            default:
                x += 4000000;
            }
            return x;
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();

    for i in 0..asts.len() {
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(0) }, 21000);
    assert_eq!(unsafe { f.call(1) }, 20001);
    assert_eq!(unsafe { f.call(2) }, 300002);
    assert_eq!(unsafe { f.call(3) }, 4000003);
}

#[test]
fn code_gen_enum() -> Result<(), Box<dyn Error>> {
    let src = "
        int printf(char* format, ...);
        typedef int bool;

        enum Weekday {
            Sunday,
            Monday,
            Tuesday,
            Wednesday = 10,
            Thursday,
            Friday,
            Saturday,
        };

        int test() {
            printf(\"%d\\\n\",Sunday);
            printf(\"%d\\\n\", Monday);
            printf(\"%d\\\n\", Tuesday);
            printf(\"%d\\\n\", Wednesday);
            printf(\"%d\\\n\", Thursday);
            printf(\"%d\\\n\", Friday);
            printf(\"%d\\\n\", Saturday);

            return Sunday +
                   Monday +
                   Tuesday +
                   Wednesday +
                   Thursday +
                   Friday +
                   Saturday;
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();
    assert_eq!(4, asts.len());

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    for i in 0..asts.len() {
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None)?;
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, 0 + 1 + 2 + 10 + 11 + 12 + 13);

    Ok(())
}

#[test]
fn code_gen_ternary() {
    // parse
    let ast = &parse_from_str("
        int test(int p, int x, int y) {
            return p ? x : y;
        }
    ").unwrap()[0];

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    let _any_value = gen.gen_stmt(&ast, &mut env, None, None).unwrap();

    let f: JitFunction<FuncType_i32i32i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call(1, 1, 2)}, 1);
    assert_eq!(unsafe { f.call(0, 1, 2)}, 2);
}

#[test]
fn code_gen_pointr_cast() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        typedef unsigned int size_t;
        void *malloc(size_t size);
        void free(void *ptr);

        int test() {
            int* ptr = (int*)malloc(1);
            free(ptr);
            return 0;
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();
    assert_eq!(4, asts.len());

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    for i in 0..asts.len() {
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None)?;
    }

    let _dummy: Result<JitFunction<FuncType_void_void>, inkwell::execution_engine::FunctionLookupError> = unsafe { gen.execution_engine.get_function("test") };
    let f: JitFunction<FuncType_void_void> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    let _result = unsafe { f.call() };

    Ok(())
}

#[test]
fn code_gen_handle() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        int printf(char* format, ...);

        int test(int i) {
            int x = i;
            int* ptr = &x;
            int** handle = &ptr;

            printf(\"x = %d\\\n\", x);
            printf(\"*ptr = %d\\\n\", *ptr);
            printf(\"**handle = %d\\\n\", **handle);

            return x + *ptr + **handle;
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    for i in 0..asts.len() {
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None)?;
    }

    let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    let result = unsafe { f.call(1) };
    assert_eq!(3, result);

    Ok(())
}

#[test]
fn code_gen_parened_define() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        void test(){
            int (i) = 1;
            int (*foo) = &i;
            int (*(*bar)) = &foo;
            int *(*(*(zot)));
        }
    ";

    // parse
    let asts = parse_from_str(src).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();

    let mut env = Env::new();
    for i in 0..asts.len() {
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None)?;
    }

    let f: JitFunction<FuncType_void_void> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    let _result = unsafe { f.call() };

    Ok(())
}

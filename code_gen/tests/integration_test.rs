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
fn code_gen_pre_increment_member() {
    let src = "
        int printf(char* format, ...);

        struct foo {
            int x;
        };

        typedef struct foo Foo;

        int test(int x){
            Foo foo;

            foo.x = x;
            int y = ++foo.x;
            return foo.x + y;
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
fn code_gen_post_increment_member() {
    let src = "
        int printf(char* format, ...);

        struct foo {
            int x;
        };

        typedef struct foo Foo;

        int test(int x){
            Foo foo;

            foo.x = x;
            int y = foo.x++;
            return foo.x + y;
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
fn code_gen_pre_decrement_member() {
    let src = "
        int printf(char* format, ...);

        struct foo {
            int x;
        };

        typedef struct foo Foo;

        int test(int x){
            Foo foo;

            foo.x = x;
            int y = --foo.x;
            return foo.x + y;
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
fn code_gen_post_decrement_member() {
    let src = "
        int printf(char* format, ...);

        struct foo {
            int x;
        };

        typedef struct foo Foo;

        int test(int x){
            Foo foo;

            foo.x = x;
            int y = foo.x--;
            return foo.x + y;
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
fn code_gen_struct() -> Result<(), CodeGenError> {
    let src = "
        struct date {
            int year, month;
            int day;
        };

        typedef struct date Date;

        int test() {
            Date date;

            date.year = 2022;
            date.month = 12;
            date.day = 31;

            Date* pointer = &date;
            pointer->year = 2023;
            pointer->month = 1;
            pointer->day = 1;

            return date.year + pointer->month + pointer->day;
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
    assert_eq!(unsafe { f.call() }, 2025);

    Ok(())
}

#[test]
fn code_gen_struct_init() -> Result<(), CodeGenError> {
    let src = "
        struct date {
            int year, month;
            int day;
        };

        typedef struct date Date;

        int test() {
            Date date = {2023, 1, 1};
            Date* pointer = &date;

            return date.year + pointer->month + pointer->day;
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
    assert_eq!(unsafe { f.call() }, 2025);

    Ok(())
}

#[test]
fn code_gen_global_struct_init() -> Result<(), CodeGenError> {
    let src = "
        int i = 1;

        struct date {
            int year, month;
            int day;
        };

        typedef struct date Date;
        Date date = {2023, 3, 3};

        int test() {
            Date* pointer = &date;
            Date date2 = {2023, 1, 1};

            i = 2;

            return i + date.year + date2.month + pointer->day;
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
    assert_eq!(unsafe { f.call() }, 2029);

    Ok(())
}

#[test]
fn code_gen_union() -> Result<(), CodeGenError> {
    let src = "
        union foo {
            int i_value;
            double d_value;
        };

        typedef union foo Bar;

        int test() {
            Bar bar;

            bar.i_value = 100;
            int i = bar.i_value;

            bar.d_value = 3.14;
            double d = bar.d_value;

            return i;
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
    assert_eq!(unsafe { f.call() }, 100);

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
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, 0 + 1 + 2 + 10 + 11 + 12);

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
fn code_gen_member_function() {
    let src = "
        int printf(char* format, ...);
        typedef unsigned char bool;

        struct Date {
            int year;
            int month;
            int day;
        };

        impl Date {
            int getYear(&self) {
                return self.year;
            }

            int getMonth(&self) {
                return self.month;
            }

            int getDay(&self) {
                return self.day;
            }

            bool isLeapYear(&self) {
                return self.year % 4 == 0 && self.year % 100 != 0;
            }

            void nextDay(&self) {
                self.day++;

                switch(self.month) {
                    case 1:
                    case 3:
                    case 5:
                    case 7:
                    case 8:
                    case 10:
                        if (self.day == 32) {
                            self.day = 1;
                            self.month++;
                        }
                        break;
                    case 12:
                        if (self.day == 32) {
                            self.day = 1;
                            self.month = 1;
                            self.year++;
                        }
                        break;
                    case 4:
                    case 6:
                    case 9:
                    case 11:
                        if(self.day == 31){
                            self.day = 1;
                            self.month++;
                        }
                        break;
                    case 2:
                        if(self.isLeapYear()){
                            if(self.day == 30){
                                self.day = 1;
                                self.month = 3;
                            }
                        }else{
                            if(self.day == 29){
                                self.day = 1;
                                self.month = 3;
                            }
                        }
                    }
                }
            }
        }

        int test() {
            struct Date date = {2000, 2, 28};
            date.nextDay();
            bool p1 = date.year == 2000 && date.month == 3 && date.day == 1;

            struct Date date2 = {2024, 2, 28};
            date2.nextDay();
            bool p2 = date2.year == 2024 && date2.month == 2 && date2.day == 29;

            return p1 && p2;
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
    assert_eq!(unsafe { f.call() }, 1);
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
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None)?;
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
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None)?;
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
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None)?;
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
        let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None)?;
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    let result = unsafe { f.call() };
    assert_eq!(20, result);

    Ok(())
}

/* main関数から実行するときは起きないが、テストだとスタックオーバーフローになる。

#[test]
fn code_gen_struct_array_and_pointer() -> Result<(), Box<dyn Error>> {
    // parse
    let src = "
        int printf(char* format, ...);

        struct date {
            int year, month;
            int day;
        };
        typedef struct date Date;

        Date days[2][3] = {{{2024, 1, 1},
                            {2024, 1, 2},
                            {2024, 1, 3}
                           },
                           {{2024, 2, 1},
                            {2024, 2, 2},
                            {2024, 2, 3}
                           }
                          };

        int test() {
            Date* ptr = days;

            return ptr[0].year + ptr[0].month + ptr[0].day
                 + ptr[1].year + ptr[1].month + ptr[1].day
                 + ptr[2].year + ptr[2].month + ptr[2].day
                 + ptr[3].year + ptr[3].month + ptr[3].day
                 + ptr[4].year + ptr[4].month + ptr[4].day
                 + ptr[5].year + ptr[5].month + ptr[5].day;
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

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    let result = unsafe { f.call() };
    assert_eq!(12165, result);

    Ok(())
}

#[test]
fn code_gen_init_struct_array() -> Result<(), Box<dyn Error>> {
    let src = "
        struct date {
            int year, month;
            int day;
        };
        typedef struct date Date;

        Date days[2][3] = {{{2024, 1, 1},
                            {2024, 1, 2},
                            {2024, 1, 3}
                           },
                           {{2024, 2, 1},
                            {2024, 2, 2},
                            {2024, 2, 3}
                           }
                          };

        int test() {
            return days[0][0].year + days[0][0].month + days[0][0].day
                 + days[0][1].year + days[0][1].month + days[0][1].day
                 + days[0][2].year + days[0][2].month + days[0][2].day
                 + days[1][0].year + days[1][0].month + days[1][0].day
                 + days[1][1].year + days[1][1].month + days[1][1].day
                 + days[1][2].year + days[1][2].month + days[1][2].day;
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

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    let result = unsafe { f.call() };
    assert_eq!(12165, result);

    Ok(())
}
*/
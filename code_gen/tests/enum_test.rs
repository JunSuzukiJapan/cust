extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;

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
        let _any_value = gen.gen_toplevel(&asts[i], &mut env, None, None)?;
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, 0 + 1 + 2 + 10 + 11 + 12 + 13);

    Ok(())
}

#[test]
fn code_gen_enum2() -> Result<(), Box<dyn Error>> {
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
            printf(\"%d\\\n\", Weekday::Sunday);
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
        let _any_value = gen.gen_toplevel(&asts[i], &mut env, None, None)?;
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, 0 + 1 + 2 + 10 + 11 + 12 + 13);

    Ok(())
}
/*
#[test]
fn code_gen_global_enum_init() -> Result<(), CodeGenError> {
    let src = "
        enum foo {
            Bar (int, int),
            Zot {
                int x;
                int y;
            },
        };

        typedef enum foo Foo;
        Foo foo = Foo::Bar(100, 200);

        int test() {
            if let (Foo::Bar ( x, y ) = foo) {
                return x + y;
            }else{
                return 0;
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
    assert_eq!(unsafe { f.call() }, 2029);

    Ok(())
}
*/
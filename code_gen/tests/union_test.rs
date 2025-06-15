extern crate tokenizer;
extern crate parser;
extern crate code_gen;

mod common;

use common::*;


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
            bar.d_value = 3.14;

            return bar.d_value;
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

    let f: JitFunction<FuncType_void_f64> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, 3.14);

    Ok(())
}


#[test]
fn code_gen_union2() -> Result<(), CodeGenError> {
    let src = "
        typedef union foo {
            int i_value;
            double d_value;
        } Bar;

        int test() {
            Bar bar;

            bar.d_value = 3.14;
            bar.i_value = 100;

            return bar.i_value;
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
    assert_eq!(unsafe { f.call() }, 100);

    Ok(())
}

#[test]
fn code_gen_global_union() -> Result<(), CodeGenError> {
    let src = "
        union foo {
            int i_value;
            double d_value;
        };

        typedef union foo Bar;
        Bar bar;

        int test() {
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
        let _any_value = gen.gen_toplevel(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, 100);

    Ok(())
}

#[test]
fn code_gen_global_union2() -> Result<(), CodeGenError> {
    let src = "
        union foo {
            int i_value;
            double d_value;
        };

        typedef union foo Bar;
        Bar bar = {123};

        int test() {
            int i = bar.i_value;
            bar.i_value = 100;

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
        let _any_value = gen.gen_toplevel(&asts[i], &mut env, None, None).unwrap();
    }

    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, 123);

    Ok(())
}

#[test]
fn code_gen_global_union3() -> Result<(), CodeGenError> {
    let src = "
        union foo {
            int i_value;
            double d_value;
        };

        typedef union foo Bar;
        Bar bar = {.d_value = 2.34};

        double test() {
            double d = bar.d_value;

            bar.i_value = 100;
            int i = bar.i_value;

            bar.d_value = 3.14;

            return d;
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

    let f: JitFunction<FuncType_void_f64> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, 2.34);

    Ok(())
}


#[test]
fn code_gen_global_union4() -> Result<(), CodeGenError> {
    let src = "
        union foo {
            int i_value;
            double d_value;
        };

        typedef union foo Bar;
        Bar bar = {.d_value = 2.34};

        double test() {
            double d = bar.d_value;

            bar.i_value = 100;
            int i = bar.i_value;

            bar.d_value = 3.14;

            return bar.d_value;
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

    let f: JitFunction<FuncType_void_f64> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, 3.14);

    Ok(())
}

#[test]
fn code_gen_global_union5() -> Result<(), CodeGenError> {
    let src = "
        union foo {
            double d_value;
            int i_value;
        };

        typedef union foo Bar;
        Bar bar = {.d_value = 2.34};

        double test() {
            bar.i_value = 100;
            bar.d_value = 3.14;

            return bar.d_value;
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

    let f: JitFunction<FuncType_void_f64> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    assert_eq!(unsafe { f.call() }, 3.14);

    Ok(())
}

#[test]
fn code_gen_global_union6() -> Result<(), CodeGenError> {
    let src = "
        union foo {
            double d_value;
            int i_value;
        };

        typedef union foo Bar;
        Bar bar = {.i_value = 123};

        double test() {
            bar.d_value = 3.14;
            bar.i_value = 100;

            return bar.i_value;
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
    assert_eq!(unsafe { f.call() }, 100);

    Ok(())
}

#[test]
fn code_gen_global_union7() -> Result<(), CodeGenError> {
    let src = "
        union foo {
            double d_value;
            int i_value;
        };

        typedef union foo Bar;
        Bar bar = {.i_value = 123};

        double test() {
            return bar.i_value;
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
    assert_eq!(unsafe { f.call() }, 123);

    Ok(())
}

#[test]
fn code_gen_global_complex_union() -> Result<(), CodeGenError> {
    let src = "
        struct date {
            int year;
            int month;
            int day;
        };
        typedef struct date Date;

        union foo {
            $<int, int> tpl;
            Date date;
        };
        typedef union foo Foo;

        Foo foo = {$(1, 2)};

        int test() {
            return foo.tpl.0 + foo.tpl.1;
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
    assert_eq!(unsafe { f.call() }, 3);

    Ok(())
}

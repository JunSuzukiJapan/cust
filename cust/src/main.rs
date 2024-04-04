#![allow(non_camel_case_types)]
#![allow(dead_code)]

extern crate tokenizer;
extern crate parser;
extern crate code_gen;

use parser::{Parser, ExprAST, Tokenizer, ParserError, Defines};
use code_gen::{CodeGen, Env};

// use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
// use inkwell::types::BasicType;
// use inkwell::values::{AnyValue, FunctionValue};
// use inkwell::execution_engine::FunctionLookupError;

type FuncType = unsafe extern "C" fn(i64, i64) -> i64;
type NoArgFunc = unsafe extern "C" fn() -> u64;
type FuncType_i32_i32 = unsafe extern "C" fn(i32) -> i32;
type FuncType_void_i32 = unsafe extern "C" fn() -> i32;
// type FuncType_i64_i64 = unsafe extern "C" fn(i64) -> i64;
// type FuncType_i64i64_i64 = unsafe extern "C" fn(i64, i64) -> i64;
// type FuncType_i64i64i64_i64 = unsafe extern "C" fn(i64, i64, i64) -> i64;
type FuncType_void_void = unsafe extern "C" fn() -> i64;

fn parse_expression_from_str(src: &str) -> Result<Option<ExprAST>, ParserError> {
    let token_list = Tokenizer::tokenize(src).unwrap();
    let mut iter = token_list.iter().peekable();
    let parser = Parser::new();
    let mut defs = Defines::new();
    let mut labels = Vec::new();
    parser.parse_expression(&mut iter, &mut defs, &mut Some(&mut labels))
}

fn main() {

    let src = "
        union foo {
            int i_value;
            double d_value;
        };

        typedef union foo Foo;

        int test() {
            Foo foo = foo {
                i_value: 1;
            };

            int i = foo.i_value;

            foo.d_value = 3.14;
            double d = foo.d_value;

            return i;
        }
    ";

    // tokenize
    let tokenized = Tokenizer::tokenize(src).unwrap();
    // parse
    let asts = Parser::parse(tokenized).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();
    println!("<<code parsed.>>");
    let mut env = Env::new();

    // for index in 0..asts.len() {
    //     let _any_value = gen.gen_code(&asts[index], &mut env, None, None).unwrap();
    // }
    gen.gen_toplevels(&asts, &mut env).unwrap();

    gen.module.print_to_stderr();

    println!("<<get llvm function>>");
    // let f: JitFunction<FuncType_void_void> = unsafe { gen.execution_engine.get_function("test").unwrap() };
    // let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    // let f: JitFunction<NoArgFunc> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
    println!("<<call llvm function>>");
    let result = unsafe { f.call() };
    // let result = unsafe { f.call(1) };
    // assert_eq!(result, 1);
    println!("result: {result}");
    println!("<<end call llvm function>>");

    println!("<<all end>>");
}

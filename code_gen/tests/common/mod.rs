#![allow(unused)]
#![allow(non_camel_case_types)]

extern crate tokenizer;
extern crate parser;
extern crate code_gen;

pub use inkwell::execution_engine::{JitFunction, FunctionLookupError};
pub use inkwell::values::{BasicValue, AnyValueEnum};
pub use inkwell::context::Context;
pub use inkwell::types::FunctionType;
pub use parser::{Parser, Defines, ParserError, ExprAST, AST};
pub use tokenizer::Tokenizer;
pub use code_gen::{CodeGen, Env, Position, CodeGenError};
pub use std::error::Error;

pub type FuncType_void_i32 = unsafe extern "C" fn() -> i32;
pub type FuncType_i32_i32 = unsafe extern "C" fn(i32) -> i32;
pub type FuncType_i32i32_i32 = unsafe extern "C" fn(i32, i32) -> i32;
pub type FuncType_i32i32i32_i32 = unsafe extern "C" fn(i32, i32, i32) -> i32;
pub type FuncType_void_void = unsafe extern "C" fn() -> ();

pub fn parse_from_str(input: &str) -> Result<Vec<AST>, ParserError> {
    let token_list = Tokenizer::tokenize(input)?;
    Parser::parse(token_list)
}

pub fn parse_expression_from_str(src: &str) -> Result<Option<ExprAST>, ParserError> {
    let token_list = Tokenizer::tokenize(src)?;
    let mut iter = token_list.iter().peekable();
    let parser = Parser::new();
    let mut defs = Defines::new();
    let mut labels = Vec::new();
    parser.parse_expression(&mut iter, &mut defs, &mut Some(&mut labels))
}

fn gen_prologue<'ctx>(gen: &CodeGen<'ctx>, fn_name: &str, fn_type: FunctionType<'ctx>) {
    let function = gen.module.add_function(fn_name, fn_type, None);
    let basic_block = gen.context.append_basic_block(function, "entry");

    gen.builder.position_at_end(basic_block);
}

fn gen_epilogue<'ctx>(gen: &CodeGen<'ctx>, fn_name: &str, ret_code: Option<&dyn BasicValue<'ctx>>) -> Option<JitFunction<'ctx, FuncType_void_i32>> {
    gen.builder.build_return(ret_code);

    unsafe { gen.execution_engine.get_function(fn_name).ok() }
}

pub fn code_gen_from_str(input: &str) -> Result<i32, Box<dyn Error>> {
    let ast = parse_expression_from_str(input)?.unwrap();

    let fn_name = "from_str_function";
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "module_from_str")?;
    let i32_type = gen.context.i32_type();
    let fn_type = i32_type.fn_type(&[], false);
    gen_prologue(&gen, fn_name, fn_type);

    let mut env = Env::new();
    let value = gen.gen_expr(&ast, &mut env, None, None)?.ok_or(CodeGenError::illegal_end_of_input(Position::new(1, 1)))?;
    let result = value.get_value();
    let func = match result {
        AnyValueEnum::IntValue(_) => {
            let value = result.into_int_value();
            gen_epilogue(&gen, fn_name, Some(&value)).ok_or(format!("Unable to JIT compile `{}`", input))?
        },
        AnyValueEnum::FloatValue(_) => {
            let value = result.into_float_value();
            gen_epilogue(&gen, fn_name, Some(&value)).ok_or(format!("Unable to JIT compile `{}`", input))?
        },
        _ => unimplemented!(),  // No plans to support
    };

    unsafe {
        Ok(func.call())
    }
}

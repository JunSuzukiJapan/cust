#![allow(non_camel_case_types)]
#![allow(dead_code)]

extern crate tokenizer;
extern crate parser;
extern crate code_gen;
extern crate clap;

use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use inkwell::context::Context;
use clap::*;

use parser::{ExprAST, Tokenizer, ParserError, Defines};
use code_gen::{CodeGen, Env};

#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Name of source file
    #[arg(short)]
    compile: String,
}

fn parse_expression_from_str(src: &str) -> Result<Option<ExprAST>, ParserError> {
    let token_list = Tokenizer::tokenize(src).unwrap();
    let mut iter = token_list.iter().peekable();
    let parser = parser::Parser::new();
    let mut defs = Defines::new();
    let mut labels = Vec::new();
    parser.parse_expression(&mut iter, &mut defs, &mut Some(&mut labels))
}

fn main() {
    let args = Args::parse();
    let filename = args.compile;

    let path = Path::new(&filename);
    let stem = path.file_stem().unwrap().to_str().unwrap();
    let dest = path.with_extension("llvm");

println!("cano: {}", path.canonicalize().unwrap().to_str().unwrap());
println!("stem: {stem}");
println!("dest: {}", dest.to_str().unwrap());

    // read file
    let mut f = File::open(filename).expect("file not found");
    let mut src = String::new();
    f.read_to_string(&mut src)
        .expect("something went wrong reading the file");  // error while read file

    // tokenize
    let token_list = Tokenizer::tokenize(&src).unwrap();

    // parse
    let asts = parser::Parser::parse(token_list).unwrap();

    // code gen
    let context = Context::create();
    let gen = CodeGen::try_new(&context, "test run").unwrap();
    let mut env = Env::new();
    gen.gen_toplevels(&asts, &mut env).unwrap();

    // gen.module.print_to_stderr();
    gen.module.print_to_file(dest).expect("something went wrong writing the file");


}

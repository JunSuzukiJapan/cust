#![allow(dead_code)]

use crate::parser::{AST, ExprAST, BinOp, Type, Pointer, Block, Params, StructDefinition, StructField, NumberType, Function, FunProto, FunOrProto, EnumDefinition, Enumerator};
use crate::parser::{CompileError, Declaration, DeclarationSpecifier};
use crate::compiler::CompiledValue;
use super::Env;
use super::env::{BreakCatcher, ContinueCatcher, TypeOrUnion};
use super::caster::Caster;
#[cfg(test)]
use crate::parser::{SpecifierQualifier, DirectDeclarator, Declarator, Defines, Param};
use crate::parser::{Switch, Case};

use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::values::{BasicValueEnum, BasicMetadataValueEnum, AnyValueEnum, AnyValue, FunctionValue, InstructionOpcode, PointerValue, InstructionValue, BasicValue, IntValue};
use inkwell::types::{BasicTypeEnum, AnyTypeEnum, FunctionType, BasicType, BasicMetadataTypeEnum};
use inkwell::{IntPredicate, FloatPredicate};
use inkwell::AddressSpace;
use inkwell::types::AnyType;
use inkwell::types::StructType;
use std::error::Error;
use std::collections::HashMap;

// use inkwell::data_layout::DataLayout;

#[cfg(test)]
#[allow(unused_macros)]
macro_rules! debug_println {
    ($this:expr, $env:expr) => ($this.debug_print($env, "\n", &[]));
    ($this:expr, $env:expr, $fmt:expr) => ($this.debug_print($env, concat!($fmt, "\n"), &[]));
    ($this:expr, $env:expr, $fmt:expr, $($arg:tt)*) => ($this.debug_print($env, concat!($fmt, "\n"), &[$($arg)*]));
}

#[cfg(test)]
#[allow(unused_macros)]
macro_rules! to_metadata {
    ($this:expr, $e:expr) => {
        $this.try_as_basic_metadata_value(&$e.as_any_value_enum())?        
    };
}

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn try_new(context: &'ctx Context, module_name: &str) -> Result<CodeGen<'ctx>, Box<dyn Error>> {
        let module = context.create_module(module_name);
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
        Ok(CodeGen {
            context: &context,
            module,
            builder: context.create_builder(),
            execution_engine,
        })
    }

    pub fn gen_toplevels(&self, asts: &'ctx Vec<AST>, env: &mut Env<'ctx>) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {
        let mut any_value = None;

        for ast in asts {
            any_value = self.gen_stmt(&ast, env, None, None)?;
        }

        Ok(any_value)
    }

    pub fn gen_stmt<'b, 'c>(&self,
        ast: &'ctx AST,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        match ast {
            AST::DefVar{specifiers, declarations} => {
                self.gen_def_var(specifiers, declarations, env, break_catcher, continue_catcher)?;
                Ok(None)
            },
            AST::GlobalDefVar{specifiers, declaration} => {
                let base_type = specifiers.get_type();

                for decl in declaration {
                    let declarator = decl.get_declarator();
                    let typ = declarator.make_type(base_type);
                    let name = declarator.get_name();
                    let basic_type = typ.to_basic_type_enum(self.context)?;
                    let ptr = self.module.add_global(basic_type, Some(AddressSpace::Const), name);
    
                    match decl.get_init_expr() {
                        Some(ast) => {
                            if typ.is_struct() {



                                unimplemented!()
                            }else if typ.is_array() {



                                unimplemented!()
                            }else{
                                let init = self.gen_expr(ast, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                                let basic_value = self.try_as_basic_value(&init.get_value())?;
        
                                // self.builder.build_store(ptr, basic_value);
                                ptr.set_initializer(&basic_value);
                            }
                        },
                        None => (),  // do nothing
                    };
    
                    env.insert_global_var(name, typ.clone(), ptr);
                }

                Ok(None)
            },
            AST::TypeDef(name, typ) => {
                env.insert_typedef(name, typ, self.context)?;
                Ok(None)
            },
            AST::Block(block) => self.gen_block(block, env, break_catcher, continue_catcher),
            AST::Return(opt_expr) => {
                let function = env.get_current_function().ok_or(CompileError::return_without_function(None))?;
                let opt_required_ret_type = function.get_type().get_return_type();

                let result: InstructionValue;
                if let Some(expr) = opt_expr {
                    let expr_type = expr.get_type(env)?;




                    let mut real_ret = self.gen_expr(expr, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                    let real_ret_type = self.try_as_basic_type(&real_ret.get_value().get_type())?;


                    if opt_required_ret_type.is_none() {
                        return Err(Box::new(CompileError::return_type_mismatch(None, Some(&real_ret_type), None)));
                    }

                    let required_ret_type = opt_required_ret_type.unwrap();
                    if required_ret_type != real_ret_type {
                        // let casted = self.gen_cast(&real_ret.get_value(), &required_ret_type)?;
                        // real_ret = CompiledValue::new(real_ret.get_type().clone(), casted);
                        unimplemented!()
                    }

                    let ret = self.try_as_basic_value(&real_ret.get_value())?;
                    result = self.builder.build_return(Some(&ret));
                }else{
                    if let Some(ret_type) = opt_required_ret_type {
                        return Err(Box::new(CompileError::return_type_mismatch(None, None, Some(&ret_type))));
                    }

                    result = self.builder.build_return(None);
                }
                Ok(Some(result.as_any_value_enum()))
            },
            AST::Function(Function {specifiers, declarator, params, body, labels}) => {
                let name = declarator.get_name();
                let ret_type = specifiers.get_type();
                let function = self.gen_code_function(name, ret_type, params, body, labels, env, break_catcher, continue_catcher)?;
                let fun_type = Self::make_fun_type(&name, ret_type, params, env)?;

                env.insert_function(&name, fun_type, function);

                Ok(Some(AnyValueEnum::FunctionValue(function)))
            },
            AST::FunProto(FunProto {specifiers, declarator, params}) => {
                let name = declarator.get_name();
                let ret_type = specifiers.get_type();
                let fun_proto = self.gen_code_function_prototype(name, ret_type, params, env)?;
                let fun_type = Self::make_fun_type(&name, ret_type, params, env)?;

                env.insert_function(&name, fun_type, fun_proto);

                Ok(Some(AnyValueEnum::FunctionValue(fun_proto)))
            },
            AST::If(condition, then, _else) => {
                let func: FunctionValue<'ctx> = env.get_current_function().ok_or(CompileError::no_current_function(None))?;
                let cond_block = self.context.append_basic_block(func, "if.cond");
                let then_block = self.context.append_basic_block(func, "if.then");
                let else_block = self.context.append_basic_block(func, "if.else");
                let end_block  = self.context.append_basic_block(func, "if.end");

                self.builder.build_unconditional_branch(cond_block);
                self.builder.position_at_end(cond_block);

                // check condition
                let cond = self.gen_expr(condition, env, break_catcher, continue_catcher)?.ok_or(CompileError::condition_is_not_number(None, condition))?;
                let mut comparison = cond.get_value().into_int_value();
                let i1_type = self.context.bool_type();
                comparison = self.builder.build_int_cast(comparison, i1_type, "cast to i1");  // cast to i1
                self.builder.build_conditional_branch(comparison, then_block, else_block);

                // then block
                self.builder.position_at_end(then_block);
                self.gen_stmt(then, env, break_catcher, continue_catcher)?;
                if let Some(blk) = self.builder.get_insert_block() {
                    if ! self.last_is_jump_statement(blk) {
                        self.builder.position_at_end(blk);
                        self.builder.build_unconditional_branch(end_block);
                    }
                }
                if ! self.last_is_jump_statement(then_block) {
                    self.builder.position_at_end(then_block);
                    self.builder.build_unconditional_branch(end_block);
                }

                // else block
                self.builder.position_at_end(else_block);
                if let Some(expr) = _else {
                    self.gen_stmt(expr, env, break_catcher, continue_catcher)?;
                    if let Some(blk) = self.builder.get_insert_block() {
                        if ! self.last_is_jump_statement(blk) {
                            self.builder.position_at_end(blk);
                            self.builder.build_unconditional_branch(end_block);
                        }
                    }
                }
                if ! self.last_is_jump_statement(else_block) {
                    self.builder.position_at_end(else_block);
                    self.builder.build_unconditional_branch(end_block);
                }

                // end block
                self.builder.position_at_end(end_block);

                Ok(None)
            },
            AST::Loop {init_expr, pre_condition, body, update_expr, post_condition} => {
                self.gen_loop(init_expr, pre_condition, body, update_expr, post_condition, env, break_catcher, continue_catcher)
            },
            AST::Break => {
                let break_block = break_catcher.ok_or(CompileError::break_not_in_loop_or_switch(None))?.get_block();
                self.builder.build_unconditional_branch(*break_block);

                Ok(None)
            },
            AST::Continue => {
                let continue_block = continue_catcher.ok_or(CompileError::continue_not_in_loop(None))?.get_block();
                self.builder.build_unconditional_branch(*continue_block);

                Ok(None)
            },
            AST::Goto(id) => {
                let block = env.get_block(id).ok_or(CompileError::no_such_a_label(None, id))?;
                self.builder.build_unconditional_branch(*block);

                Ok(None)
            },
            AST::Labeled(id, opt_stmt) => {
                let block = env.get_block(id).ok_or(CompileError::no_such_a_label(None, id))?;

                // if let Some(blk) = env.get_current_function().unwrap().get_last_basic_block() {
                //     if ! self.last_is_jump_statement(blk) {
                //         self.builder.build_unconditional_branch(*block);
                //     }
                // }
                self.builder.build_unconditional_branch(*block);

                self.builder.position_at_end(*block);

                if let Some(stmt) = opt_stmt {
                    self.gen_stmt(stmt, env, break_catcher, continue_catcher)
                }else{
                    Ok(None)
                }
            },
            AST::DefineStruct{name, fields} => {
                self.gen_define_struct(name, fields, env, break_catcher, continue_catcher)?;
                Ok(None)
            },
            AST::DefineUnion { name, fields } => {
                self.gen_define_union(name, fields, env, break_catcher, continue_catcher)?;
                Ok(None)
            },
            AST::DefineEnum { name, fields } => {
                self.gen_define_enum(name, fields, env, break_catcher, continue_catcher)?;
                Ok(None)
            },
            AST::Impl { name, typ, for_type, functions } => {
                self.gen_impl(name, typ, for_type, functions, env, break_catcher, continue_catcher)?;

                Ok(None)
            },
            AST::Switch(switch) => {
                self.gen_switch(switch, env, break_catcher, continue_catcher)
            },
            AST::Case(case) => {
                self.gen_case(case, env, break_catcher, continue_catcher)
            },
            AST::Default(stmt) => {
                self.gen_default(stmt, env, break_catcher, continue_catcher)
            },
            AST::_self => {
                if let Some((_typ, ptr)) = env.get_self_ptr() {
                    let basic_val = self.builder.build_load(ptr, "get_self");
                    let any_val = basic_val.as_any_value_enum();

                    Ok(Some(any_val))
                }else{
                    Err(Box::new(CompileError::no_such_a_variable(None, "self")))
                }
            },
            AST::Expr(expr_ast) => {
                if let Some(value) = self.gen_expr(expr_ast, env, break_catcher, continue_catcher)? {
                    Ok(Some(value.get_value()))
                }else{
                    Ok(None)
                }
            },



            _ => unimplemented!("ast: {:?}", ast),
        }
    }

    pub fn gen_expr<'b, 'c>(&self,
        expr_ast: &ExprAST,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {

        match expr_ast {
            ExprAST::Int(num) => {
                let i32_type = self.context.i32_type();
                let result = i32_type.const_int(*num as u64, true);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::Int), result.as_any_value_enum())))
            },
            ExprAST::Short(num) => {
                let i16_type = self.context.i16_type();
                let result = i16_type.const_int(*num as u64, true);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::Short), result.as_any_value_enum())))
            },
            ExprAST::Long(num) => {
                let i64_type = self.context.i64_type();
                let result = i64_type.const_int(*num as u64, true);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::Long), result.as_any_value_enum())))
            },
            // ExprAST::LongLong(num) => {
            //     let i128_type = self.context.i128_type();
            //     let result = i128_type.const_int(*num as u128, true);
            //     Ok(Some(result.as_any_value_enum()))
            // },
            ExprAST::UInt(num) => {
                let i32_type = self.context.i32_type();
                let result = i32_type.const_int(*num as u64, true);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::UnsignedInt), result.as_any_value_enum())))
            },
            ExprAST::UShort(num) => {
                let i16_type = self.context.i16_type();
                let result = i16_type.const_int(*num as u64, true);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::UnsignedShort), result.as_any_value_enum())))
            },
            ExprAST::ULong(num) => {
                let i64_type = self.context.i64_type();
                let result = i64_type.const_int(*num as u64, true);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::UnsignedLong), result.as_any_value_enum())))
            },
            ExprAST::Double(num) => {
                let f64_type = self.context.f64_type();
                let result = f64_type.const_float(*num);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::Double), result.as_any_value_enum())))
            },
            ExprAST::StringLiteral(s) => {
                // let result = self.context.const_string(s.as_bytes(), false);
                let result = self.builder.build_global_string_ptr(s, &format!("global_str_{}", s));
                let pointer = Pointer::new(false, false);
                Ok(Some(CompiledValue::new(Type::Pointer(pointer, Box::new(Type::Number(NumberType::Char))), result.as_any_value_enum())))
            },
            ExprAST::UnaryMinus(boxed_ast) => {
                let code = self.gen_expr(&*boxed_ast, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;

                let result = self.builder.build_int_neg(code.get_value().into_int_value(), "neg");
                Ok(Some(CompiledValue::new(code.get_type().clone(), result.as_any_value_enum())))
            },
            ExprAST::Not(boxed_ast) => {
                let value = self.gen_expr(&*boxed_ast, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let code = value.get_value().into_int_value();
                let zero = self.context.i32_type().const_int(0, false);

                let result = self.builder.build_int_compare(IntPredicate::EQ, code, zero, "Not");
                Ok(Some(CompiledValue::new(value.get_type().clone(), result.as_any_value_enum())))
            },
            ExprAST::UnaryTilda(boxed_ast) => {
                let value = self.gen_expr(&*boxed_ast, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let code = value.get_value().into_int_value();
                let all_ones = self.context.i32_type().const_all_ones();

                let result = self.builder.build_xor(code, all_ones, "bit_reversal");
                Ok(Some(CompiledValue::new(value.get_type().clone(), result.as_any_value_enum())))
            },
            ExprAST::BinExpr(op, left, right) => self.gen_bin_expr(op, &**left, &**right, env, break_catcher, continue_catcher),
            ExprAST::DefVar{specifiers, declarations} => {
                self.gen_def_var(specifiers, declarations, env, break_catcher, continue_catcher)?;
                Ok(None)
            },
            ExprAST::UnaryGetAddress(boxed_ast) => {
                let ast = &**boxed_ast;
                match ast {
                    ExprAST::Symbol(name) => {
                        let (typ, ptr) = env.get_ptr(name).ok_or(CompileError::no_such_a_variable(None, name))?;
                        let ptr = PointerValue::try_from(ptr).ok().ok_or(CompileError::cannot_get_pointer(None))?;

                        Ok(Some(CompiledValue::new(typ.clone(), ptr.into())))
                    },






                    _ => unimplemented!(),
                }
            },
            ExprAST::UnaryPointerAccess(boxed_ast) => {
                let ast = &**boxed_ast;
                match ast {
                    ExprAST::Symbol(name) => {
                        let (typ, ptr_to_ptr) = env.get_ptr(name).ok_or(CompileError::no_such_a_variable(None, name))?;
                        let ptr = self.builder.build_load(ptr_to_ptr, &format!("get_ptr_from_{}", name));
                        let basic_val = self.builder.build_load(ptr.into_pointer_value(), &format!("get_value_from_{}", name));
                        let any_val = basic_val.as_any_value_enum();
                        Ok(Some(CompiledValue::new(typ.clone(), any_val)))
                    },




                    _ => unimplemented!(),
                }
            },
            ExprAST::Symbol(name) => {
                if let Some((typ, ptr)) = env.get_ptr(name) {
                    let basic_val = self.builder.build_load(ptr, name);
                    let any_val = basic_val.as_any_value_enum();

                    Ok(Some(CompiledValue::new(typ.clone(), any_val)))

                }else if let Some((typ, val)) = env.get_value(name) {
                    Ok(Some(CompiledValue::new(typ.clone(), val.as_any_value_enum())))
    
                }else{
                    Err(Box::new(CompileError::no_such_a_variable(None, name)))
                }
            },
            ExprAST::Assign(l_value, r_value) => self.gen_assign(&**l_value, &**r_value, env, break_catcher, continue_catcher),
            ExprAST::CallFunction(fun, args) => {
                let mut v: Vec<BasicMetadataValueEnum> = Vec::new();
                for expr in args {
                    let result = self.gen_expr(expr, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                    v.push(self.try_as_basic_metadata_value(&result.get_value())?);
                }

                match &**fun {
                    ExprAST::Symbol(name) => {
                        self.gen_call_function(name, v, env, break_catcher, continue_catcher)
                    },
                    ExprAST::MemberAccess(ast, fun_name) => {
                        let typ = ast.get_type(env)?;
                        let (_t, obj) = self.get_l_value(&**ast, env, break_catcher, continue_catcher)?;
                        let class_name = typ.get_type_name();
                        let method_name = self.make_function_name_in_impl(&class_name, fun_name);
                        let mut args: Vec<BasicMetadataValueEnum> = Vec::new();
                        args.push(obj.into());

                        args.append(&mut v);

                        self.gen_call_function(&method_name, args, env, break_catcher, continue_catcher)

                    },
                    _ => unimplemented!("'{:?}' in CallFunction.", **fun),
                }
            },
            ExprAST::MemberAccess(_boxed_ast, field_name) => {
                let (typ, ptr) = self.get_l_value(expr_ast, env, break_catcher, continue_catcher)?;
                let basic_val = self.builder.build_load(ptr, &format!("access_to_field_{}", field_name));
                let any_val = basic_val.as_any_value_enum();
                Ok(Some(CompiledValue::new(typ.clone(), any_val)))
            },
            ExprAST::PointerAccess(_boxed_ast, field_name) => {
                let (typ, ptr) = self.get_l_value(expr_ast, env, break_catcher, continue_catcher)?;
                let basic_val = self.builder.build_load(ptr, &format!("pointer_access_to_field_{}", field_name));
                let any_val = basic_val.as_any_value_enum();
                Ok(Some(CompiledValue::new(typ.clone(), any_val)))
            },
            ExprAST::ArrayAccess(_boxed_ast, _index) => {
                let (typ, ptr) = self.get_l_value(expr_ast, env, break_catcher, continue_catcher)?;
                let basic_val = self.builder.build_load(ptr, "get_value_from_array");
                let any_val = basic_val.as_any_value_enum();
                Ok(Some(CompiledValue::new(typ.get_element_type()?.clone(), any_val)))
            },
            ExprAST::_self => {
                if let Some((typ, ptr)) = env.get_self_ptr() {
                    let basic_val = self.builder.build_load(ptr, "get_self");
                    let any_val = basic_val.as_any_value_enum();

                    Ok(Some(CompiledValue::new(typ.clone(), any_val)))
                }else{
                    Err(Box::new(CompileError::no_such_a_variable(None, "self")))
                }
            },
            ExprAST::InitializerList(_ast_list) => {
                // never reached, maybe
                unimplemented!()
            },




            _ => unimplemented!("ast: {:?}", expr_ast),
        }
    }

    fn gen_def_var<'b, 'c>(&self,
        specifiers: &DeclarationSpecifier,
        declarations: &Vec<Declaration>,
        env:  &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<(), Box<dyn Error>> {

        let base_type = specifiers.get_type();
        for decl in declarations {
            let declarator = decl.get_declarator();
            let typ = declarator.make_type(base_type);
            let name = declarator.get_name();
            let basic_type = env.basic_type_enum_from_type(&typ, self.context)?;
            let ptr = self.builder.build_alloca(basic_type, name);

println!("def var '{:?}'. type: {:?}", name, typ);

            match decl.get_init_expr() {
                Some(ast) => {
                    if let Type::Struct { fields, .. } = &typ {
                        self.gen_struct_init(&fields, ptr, &**ast, env, break_catcher, continue_catcher)?;

                    }else if typ.is_array() {


                        unimplemented!()
                    }else{
                        let compiled_value = self.gen_expr(&**ast, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                        let mut init_value = compiled_value.get_value();
                        let init_type = compiled_value.get_type();
println!("VarType: {:?}, InitType: {:?}", typ, init_type);

                        if typ != *init_type {
println!("typ != *init_type");
                            init_value = self.gen_cast(&init_value, &init_type, &typ)?;
                        }

                        let basic_value = self.try_as_basic_value(&init_value)?;
                        self.builder.build_store(ptr, basic_value);
                    }
                },
                None => (),  // do nothing
            };

            env.insert_local(name, typ.clone(), ptr);
        }

        Ok(())
    }

    pub fn gen_struct_init<'b, 'c>(&self,
        target_fields: &StructDefinition,
        target_struct_ptr: PointerValue<'ctx>,
        init: &ExprAST,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let init_value_list = if let ExprAST::InitializerList(list) = init {
            list
        }else{
            return Err(Box::new(CompileError::mismatch_initializer_type(None)));
        };

        let target_len = target_fields.len();
        let init_len = init_value_list.len();
        if target_len < init_len {
            return  Err(Box::new(CompileError::initial_list_is_too_long(None)));
        }

        if target_len == 0 {
            return Ok(None);
        }
        // target_len > 0

        let fields = target_fields.get_fields().unwrap();
        for i in 0..target_len {
            let field = &fields[i];

            let target_field_ptr = self.builder.build_struct_gep(target_struct_ptr, i as u32, "init_struct_member");
            if let Ok(ptr) = target_field_ptr {
                let field_type = field.get_type().unwrap();

                if init_len > i {
                    let init_value = &init_value_list[i];
                    let init_type = init_value.get_type(env)?;
                    if *field_type != init_type {
                        return Err(Box::new(CompileError::mismatch_initializer_type(None)));
                    }

                    let compiled_val = self.gen_expr(init_value, env, break_catcher, continue_catcher)?.ok_or(CompileError::mismatch_initializer_type(None))?;
                    let value = self.try_as_basic_value(&compiled_val.get_value())?;
                    let _result = self.builder.build_store(ptr, value);

                }else{  // zero clear
                    let zero_value = self.const_zero(field_type)?;
                    let _result = self.builder.build_store(ptr, zero_value);
                }

            }else{
                return Err(Box::new(CompileError::cannot_init_struct_member(None)));
            }
        }

        Ok(None)
    }

    fn const_zero(&self, typ: &Type) -> Result<BasicValueEnum, Box<dyn Error>> {
        let t = typ.to_llvm_type(self.context)?;
        match t {
            BasicMetadataTypeEnum::ArrayType(_) => {
                unimplemented!()
            },
            BasicMetadataTypeEnum::FloatType(t) => {
                Ok(t.const_zero().as_basic_value_enum())
            },
            BasicMetadataTypeEnum::IntType(t) => {
                Ok(t.const_zero().as_basic_value_enum())
            },
            BasicMetadataTypeEnum::PointerType(t) => {
                Ok(t.const_zero().as_basic_value_enum())
            },
            BasicMetadataTypeEnum::StructType(_) => {
                unimplemented!()
            },
            BasicMetadataTypeEnum::VectorType(_) => {
                unimplemented!()
            },
            _ => {
                Err(Box::new(CompileError::cannot_get_zero_value(None)))
            },
        }
    }

    pub fn gen_call_function<'b, 'c>(&self,
        name: &str,
        args: Vec<BasicMetadataValueEnum<'ctx>>,
        env: &mut Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        _continue_catcher: Option<&'c ContinueCatcher>
   ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {
        if let Some((fn_typ, function)) = env.get_function(name) {
            let call_site_value = self.builder.build_call(*function, &args, &format!("call_function_{}", name));

            let tried = call_site_value.try_as_basic_value();
            if tried.is_left() {  // BasicValueEnum
                let any_val = tried.left().unwrap().as_any_value_enum();
                Ok(Some(CompiledValue::new(fn_typ.get_fn_ret_type()?.clone(), any_val)))

            }else{                 // InstructionValue
                Ok(Some(CompiledValue::new(fn_typ.get_fn_ret_type()?.clone(), AnyValueEnum::InstructionValue(tried.right().unwrap()))))
            }

        }else{
            return Err(Box::new(CompileError::no_such_a_function(None, &name)));
        }
    }

    fn str_to_basic_metadata_value_enum(&self, s: &str) -> Result<BasicMetadataValueEnum<'ctx>, CompileError> {
        let global_str = self.builder.build_global_string_ptr(s, &format!("global_str_{}", s)).as_any_value_enum();
        Ok(self.try_as_basic_metadata_value(&global_str)?)
    }

    pub fn gen_switch<'b, 'c>(&self,
        switch: &'ctx Switch,
        env: &mut Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let func: FunctionValue<'ctx> = env.get_current_function().ok_or(CompileError::no_current_function(None))?;
        let cond_block  = self.context.append_basic_block(func, "switch.cond");
        let end_block  = self.context.append_basic_block(func, "switch.end");
        let break_catcher = BreakCatcher::new(&end_block);
        let break_catcher = Some(&break_catcher);

        //
        // condition expr
        //
        self.builder.build_unconditional_branch(cond_block);
        self.builder.position_at_end(cond_block);
        let opt_cond_expr = switch.get_cond_expr();
        let opt_cond = if let Some(e) = opt_cond_expr {
            self.gen_expr(e, env, break_catcher, continue_catcher)?
        }else{
            return Ok(None);
        };
        if opt_cond.is_none(){
            return Ok(None);
        }
        let cond_expr = opt_cond.unwrap();

        //
        // cases
        //
        env.add_new_switch_case();

        let opt_stmt = switch.get_stmt();
        let _opt_result = if let Some(stmt) = opt_stmt {
            self.gen_stmt(stmt, env, break_catcher, continue_catcher)?
        }else{
            None
        };

        let case_list = env.get_case_list();
        let len = case_list.len();
        let mut opt_default = None;
        let mut current_block = cond_block;
        for i in 0..len {
            let case = &case_list[i];
            let block = case.get_block();

            //
            // add branch if last statement is not branch
            //
            if ! self.last_is_jump_statement(*block) {
                let next_block = if i == len - 1 {
                    end_block
                }else{
                    *case_list[i+1].get_block()
                };

                self.builder.position_at_end(*block);
                self.builder.build_unconditional_branch(next_block);
            }

            let insert_block = case.get_insert_block();
            if let Some(blk) = insert_block {
                if ! self.last_is_jump_statement(*blk) {
                    let next_block = if i == len - 1 {
                        end_block
                    }else{
                        *case_list[i+1].get_block()
                    };

                    self.builder.position_at_end(*blk);
                    self.builder.build_unconditional_branch(next_block);
                }
            }

            //
            // conditional jump
            //
            self.builder.position_at_end(current_block);

            if case.is_case() {  // case
                if opt_default.is_some() {
                    return Err(Box::new(CompileError::case_after_default(None)));
                }

                let case_cond = case.get_cond().unwrap();
                let value = case_cond.as_i32_value();
                let i32_type = self.context.i32_type();
                let case_value = i32_type.const_int(value as u64, true);
                let real_value = self.try_as_basic_value(&cond_expr.get_value())?.into_int_value();
                let comparison = self.builder.build_int_compare(IntPredicate::EQ, real_value, case_value, "compare_switch_case");



                if i < len - 1 {
                    current_block = self.context.append_basic_block(func, &format!("cond_block_{}", i + 1));
                }else{
                    current_block = end_block;
                }
                self.builder.build_conditional_branch(comparison, *case.get_block(), current_block);

            }else{               // default
                if opt_default.is_some() {
                    return Err(Box::new(CompileError::already_default_defined(None)));
                }
                opt_default = Some(case);

                self.builder.build_unconditional_branch(*case.get_block());
            }
        }

        // if let Some(default) = opt_default {
        //     self.builder.position_at_end(cond_block);
        //     self.builder.build_unconditional_branch(*default.get_block());
        // }


        env.remove_switch_case();

        //
        // end block
        //
        self.builder.position_at_end(end_block);

        Ok(None)
    }

    pub fn gen_case<'b, 'c>(&self,
        case: &'ctx Case,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let func: FunctionValue<'ctx> = env.get_current_function().ok_or(CompileError::no_current_function(None))?;
        let case_block  = self.context.append_basic_block(func, "switch.case");

        self.builder.position_at_end(case_block);

        let const_cond = case.get_cond();
        let ast = case.get_stmt();
        let code = self.gen_stmt(ast, env, break_catcher, continue_catcher)?;
        let insert_block= self.builder.get_insert_block();

        env.insert_case(const_cond, case_block, code, insert_block);

        Ok(None)
    }

    pub fn gen_default<'b, 'c>(&self,
        stmt: &'ctx AST,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let func: FunctionValue<'ctx> = env.get_current_function().ok_or(CompileError::no_current_function(None))?;
        let default_block  = self.context.append_basic_block(func, "switch.default");

        self.builder.position_at_end(default_block);
        let code = self.gen_stmt(stmt, env, break_catcher, continue_catcher)?;
        let insert_block= self.builder.get_insert_block();

        env.insert_default(default_block, code, insert_block);

        Ok(None)
    }

    fn gen_cast(&self, value: &AnyValueEnum<'ctx>, from_type: &Type, to_type: &Type) -> Result<AnyValueEnum<'ctx>, Box<dyn Error>> {
        Caster::gen_cast(&self.builder, value, from_type, to_type)
    }

    #[cfg(test)]
    fn debug_print<'b, 'c>(&self,
        env: &mut Env<'ctx>,
        fmt: &str,
        args: &[BasicMetadataValueEnum<'ctx>] ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>>
    {
        let mut v = Vec::new();

        let fmt_string = self.builder.build_global_string_ptr(fmt, &format!("global_str_{}", fmt)).as_any_value_enum();
        v.push(self.try_as_basic_metadata_value(&fmt_string)?);
        for item in args {
            v.push(*item);
        }

        let call_site_value = if let Some((_typ, function)) = env.get_function("printf") {
            self.builder.build_call(*function, &v, &format!("call_function_printf_in_debug_print"))
        }else{
            let name = "printf";
            let ret_type = Type::Number(NumberType::Int);

            let sq = SpecifierQualifier::new();
            let ds = DeclarationSpecifier { typ: Type::Number(NumberType::Char), specifier_qualifier: sq };


            let pointer = Pointer::new(false, false);
            let dd = DirectDeclarator::Symbol("format".to_string());
            let decl = Declarator::new(Some(pointer), dd);

            let mut defs = Defines::new();
            let param = Param::new(ds, decl, &mut defs)?;
            let param_list = vec![param];
            let params = Params::new(param_list, true);
            let fun_proto = self.gen_code_function_prototype(name, &ret_type, &params, env)?;
            let fun_type = Self::make_fun_type(&name, &ret_type, &params, env)?;

            env.insert_function(&name, fun_type, fun_proto);

            let (_typ, function) = env.get_function("printf").unwrap();
            self.builder.build_call(*function, &v, &format!("call_function_printf_in_debug_print"))
        };

        let tried = call_site_value.try_as_basic_value();
        if tried.is_left() {  // BasicValueEnum
            let any_val = tried.left().unwrap().as_any_value_enum();
            Ok(Some(any_val))
        }else{                 // InstructionValue
            Ok(Some(AnyValueEnum::InstructionValue(tried.right().unwrap())))
        }
    }

    fn gen_impl<'b, 'c>(
        &self,
        class_name: &str,
        typ: &Type,
        for_type: &Option<String>,
        functions: &'ctx Vec<FunOrProto>,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<(), Box<dyn Error>> {
        if for_type.is_some() {
            self.gen_impl_for(class_name, typ, for_type, functions, env, break_catcher, continue_catcher)
        }else{
            self.gen_impl_no_for(class_name, typ, functions, env, break_catcher, continue_catcher)
        }
    }

    fn gen_impl_no_for<'b, 'c>(
        &self,
        class_name: &str,
        typ: &Type,
        functions: &'ctx Vec<FunOrProto>,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<(), Box<dyn Error>> {

        let _class = env.get_type(class_name).ok_or(Box::new(CompileError::no_such_a_struct(None, class_name)))?;

        for function in functions {
            self.gen_impl_function(class_name, typ, function, env, break_catcher, continue_catcher)?;
        }

        Ok(())
    }

    #[allow(unused_variables)]
    fn gen_impl_for<'b, 'c>(
        &self,
        class_name: &str,
        typ: &Type,
        for_type: &Option<String>,
        functions: &Vec<FunOrProto>,
        env: &mut Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        _continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<(), Box<dyn Error>> {







        unimplemented!()
    }

    fn gen_impl_function<'b, 'c>(
        &self,
        class_name: &str,
        _typ: &Type,
        fun_or_proto: &'ctx FunOrProto,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let declarator = fun_or_proto.get_declarator();
        let specifiers = fun_or_proto.get_specifiers();
        let ret_type = specifiers.get_type();
        let params = fun_or_proto.get_params();
        let decl_name = declarator.get_name();
        let fun_name = self.make_function_name_in_impl(class_name, decl_name);
        let body = fun_or_proto.get_body();
        let function = if let Some(b) = body {
            // Function
            let labels = fun_or_proto.get_labels().unwrap();
            self.gen_code_function(&fun_name, ret_type, params, b, labels, env, break_catcher, continue_catcher)?
        }else{
            // Function Prototype
            self.gen_code_function_prototype(&fun_name, ret_type, params, env)?
        };

        let fun_type = Self::make_fun_type(&fun_name, ret_type, params, env)?;
        env.insert_function(&fun_name, fun_type, function);

        Ok(Some(AnyValueEnum::FunctionValue(function)))
    }

    fn make_function_name_in_impl(&self, class_name: &str, fun_name: &str) -> String {
        format!("${}${}", class_name, fun_name)
    }

    pub fn gen_define_struct<'b, 'c>(
        &self,
        name: &Option<String>,
        fields: &StructDefinition,
        env: &mut Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        _continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyTypeEnum<'ctx>>, Box<dyn Error>> {

        let (struct_type, index_map) = Self::struct_from_struct_definition(name, fields, self.context)?;
        if let Some(id) = name {
            env.insert_struct(id, &struct_type, index_map)?;
        }

        Ok(Some(struct_type.as_any_type_enum()))
    }

    pub fn struct_from_struct_definition(name: &Option<String>, struct_def: &StructDefinition, ctx: &'ctx Context) -> Result<(StructType<'ctx>, HashMap<String, usize>), CompileError> {
        let mut list: Vec<BasicTypeEnum<'ctx>> = Vec::new();
        let mut packed = false;
        let mut index_map: HashMap<String, usize> = HashMap::new();
        let mut index = 0;

        if let Some(fields) = struct_def.get_fields() {
            for field in fields {
                match field {
                    StructField::NormalField { name: _, sq: _, typ } => {
                        let t = typ.to_basic_type_enum(ctx)?;
                        list.push(t);

                        if let Some(id) = name {
                            index_map.insert(id.clone(), index);
                        }
                    },
                    StructField::BitField { name, sq: _, typ, bit_size } => {
                        packed = true;

                        Self::bit_size_check(typ, *bit_size)?;
                        let t = ctx.custom_width_int_type((*bit_size) as u32);
                        list.push(t.as_basic_type_enum());

                        if let Some(id) = name {
                            index_map.insert(id.clone(), index);
                        }
                    },
                }

                index += 1;
            }
        }

        Ok((ctx.struct_type(&list, packed), index_map))
    }

    pub fn gen_define_union<'b, 'c>(
        &self,
        name: &Option<String>,
        fields: &StructDefinition,
        env: &mut Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        _continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyTypeEnum<'ctx>>, Box<dyn Error>> {

        let (type_list, index_map, max_size, max_size_type) = Self::union_from_struct_definition(name, fields, self.context)?;
        if let Some(id) = name {
            env.insert_union(id, type_list, index_map, max_size, max_size_type)?;
        }

        if let Some(t) = max_size_type {
            Ok(Some(t.as_any_type_enum()))
        }else{
            Ok(None)
        }
    }

    pub fn union_from_struct_definition(_name: &Option<String>, struct_def: &StructDefinition, ctx: &'ctx Context)
      -> Result<(Vec<(Type, BasicTypeEnum<'ctx>)>, HashMap<String, usize>, u64, Option<BasicTypeEnum<'ctx>>), CompileError>
    {
        let mut list: Vec<(Type, BasicTypeEnum<'ctx>)> = Vec::new();
        let mut index_map: HashMap<String, usize> = HashMap::new();
        let mut index = 0;
        let mut max_size = 0;
        let mut max_size_type: Option<BasicTypeEnum> = None;
 
        if let Some(fields) = struct_def.get_fields() {
            for field in fields {
                match field {
                    StructField::NormalField { name: field_name, sq: _, typ } => {
                        let t = typ.to_basic_type_enum(ctx)?;
                        list.push((typ.clone(), t.clone()));

                        let size = Self::size_of(&t)?;
                        if size > max_size {
                            max_size = size;
                            max_size_type = Some(t);
                        }

                        if max_size_type.is_none() {
                            max_size = size;
                            max_size_type = Some(t.clone())
                        }

                        if let Some(id) = field_name {
                            index_map.insert(id.clone(), index);
                        }
                    },
                    StructField::BitField { name, sq: _, typ, bit_size } => {
                        Self::bit_size_check(&typ, *bit_size)?;
                        let t = ctx.custom_width_int_type((*bit_size) as u32);
                        let t = t.as_basic_type_enum();

                        if let Some(typ2) = typ {
                            list.push((typ2.clone(), t.clone()));
                        }else{
                            list.push((Type::BitField, t.clone()))
                        }

                        if let Some(size) = t.size_of() {
                            if let Some(s) = size.get_zero_extended_constant() {
                                if s > max_size {
                                    max_size = s;
                                    max_size_type = Some(t);
                                }
                            }
                        }

                        if let Some(id) = name {
                            index_map.insert(id.clone(), index);
                        }
                    },
                }

                index += 1;
            }
        }

        Ok((list, index_map, max_size, max_size_type))
    }

    pub fn gen_define_enum<'b, 'c>(
        &self,
        name: &Option<String>,
        enum_def: &EnumDefinition,
        env: &mut Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        _continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<(), Box<dyn Error>> {

        let (enumerator_list, index_map) = self.enum_from_enum_definition(enum_def, env)?;
        if let Some(id) = name {
            let i32_type = self.context.i32_type();
            env.insert_enum(id, &i32_type, enumerator_list, index_map)?;
        }

        Ok(())
    }

    pub fn enum_from_enum_definition(&self, enum_def: &EnumDefinition, _env: &mut Env<'ctx>) -> Result<(Vec<(String, IntValue<'ctx>)>, HashMap<String, usize>), CompileError> {
        let mut enumerator_list: Vec<(String, IntValue<'ctx>)> = Vec::new();
        let mut index_map: HashMap<String, usize> = HashMap::new();
        let mut index: usize = 0;

        if let Some(fields) = enum_def.get_fields() {
            for field in fields {
                match field {
                    Enumerator::Const { name, const_value } => {
                        let i32_type = self.context.i32_type();
                        let i32_value = i32_type.const_int(*const_value as u64, false);
                        enumerator_list.push((name.to_string(), i32_value));

                        index_map.insert(name.clone(), index);
                    },
                }

                index += 1;
            }
        }

        Ok((enumerator_list, index_map))
    }

    fn size_of(basic_type: &BasicTypeEnum) -> Result<u64, CompileError> {
        if let Some(size) = basic_type.size_of() {
            if let Some(s) = size.get_zero_extended_constant() {
                return Ok(s);
            }
        }

        let size = Self::calc_type_size(basic_type);
        Ok(size)
     }

    fn calc_type_size(basic_type: &dyn BasicType) -> u64 {
        use inkwell::execution_engine::JitFunction;
        type Func_void_u64 = unsafe extern "C" fn() -> u64;

        let context = Context::create();
        let module = context.create_module("foo");
        let builder = context.create_builder();
        let engine = module.create_jit_execution_engine(inkwell::OptimizationLevel::None).unwrap();
    
        let fn_name = "__calc_type_size__";
        let i32_type = context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let function = module.add_function(fn_name, fn_type, None);
        let basic_block = context.append_basic_block(function, "entry");
        builder.position_at_end(basic_block);
    
        let array_type = basic_type.array_type(2);
        let array_ptr = builder.build_alloca(array_type, "array");
        // let zero = i32_type.const_int(0, false);
        // let array_ptr = zero.const_to_pointer(array_ptr_type);
        let const_one = i32_type.const_int(1, false);
        let ptr = unsafe { builder.build_in_bounds_gep(array_ptr, &[const_one], "gep for array") };
    
        let base = array_ptr.const_to_int(i32_type);
        let index1 = ptr.const_to_int(i32_type);
        let size = builder.build_int_sub(index1, base, "sub");
        let _tmp = builder.build_return(Some(&size));

        let f = unsafe { engine.get_function(fn_name) };
        let f: JitFunction<Func_void_u64> = f.ok().unwrap();
        let result: u64 = unsafe { f.call() };

        result
    }

    fn bit_size_check(opt_type: &Option<Type>, size: u64) -> Result<(), CompileError> {
        if let Some(Type::Number(typ)) = opt_type {
            match typ {
                NumberType::_Bool => {
                    if size > 1 {
                        return Err(CompileError::illegal_bit_size(None, typ, size));
                    }
                },
                NumberType::Char => {
                    if size > 8 {
                        return Err(CompileError::illegal_bit_size(None, typ, size));
                    }
                },
                NumberType::Short => {
                    if size > 16 {
                        return Err(CompileError::illegal_bit_size(None, typ, size));
                    }
                },
                NumberType::Int => {
                    if size > 32 {
                        return Err(CompileError::illegal_bit_size(None, typ, size));
                    }
                },
                NumberType::Long => {
                    if size > 64 {
                        return Err(CompileError::illegal_bit_size(None, typ, size));
                    }
                },
                NumberType::LongLong => {
                    if size > 128 {
                        return Err(CompileError::illegal_bit_size(None, typ, size));
                    }
                },
                NumberType::UnsignedChar => {
                    if size > 8 {
                        return Err(CompileError::illegal_bit_size(None, typ, size));
                    }
                },
                NumberType::UnsignedShort => {
                    if size > 16 {
                        return Err(CompileError::illegal_bit_size(None, typ, size));
                    }
                },
                NumberType::UnsignedInt => {
                    if size > 32 {
                        return Err(CompileError::illegal_bit_size(None, typ, size));
                    }
                },
                NumberType::UnsignedLong => {
                    if size > 64 {
                        return Err(CompileError::illegal_bit_size(None, typ, size));
                    }
                },
                NumberType::UnsignedLongLong => {
                    if size > 128 {
                        return Err(CompileError::illegal_bit_size(None, typ, size));
                    }
                },
                NumberType::Float | NumberType::Double => {
                    return Err(CompileError::cannot_use_float_for_bitsize(None));
                }
            }
        }

        Ok(())
    }

    fn last_is_jump_statement(&self, block: inkwell::basic_block::BasicBlock) -> bool {
        if let Some(inst) = block.get_last_instruction() {
            let op_code = inst.get_opcode();
            match op_code {
                InstructionOpcode::Br | InstructionOpcode::Return => true,
                _ => false,
            }
        }else{
            false
        }
    }

    fn gen_loop<'b, 'c>(&self,
        init_expr: &'ctx Option<Box<ExprAST>>,
        pre_condition: &'ctx Option<Box<ExprAST>>,
        body: &'ctx Option<Box<AST>>,
        update_expr: &'ctx Option<Box<ExprAST>>,
        post_condition: &'ctx Option<Box<ExprAST>>,
        env: &mut Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        _continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let func: FunctionValue<'ctx> = env.get_current_function().ok_or(CompileError::no_current_function(None))?;
        let pre_condition_block = self.context.append_basic_block(func, "loop.pre_condition");
        let start_block = self.context.append_basic_block(func, "loop.start");
        // let post_condition_block = self.context.append_basic_block(func, "loop.post_condition");
        let update_block = self.context.append_basic_block(func, "loop.update");
        let end_block = self.context.append_basic_block(func, "loop.end");

        let break_catcher = BreakCatcher::new(&end_block);
        let continue_catcher = ContinueCatcher::new(&update_block);
        let break_catcher = Some(&break_catcher);
        let continue_catcher = Some(&continue_catcher);

        //
        // init exprs
        //
        if let Some(expr) = init_expr {
            self.gen_expr(expr, env, break_catcher, continue_catcher)?;
        }

        //
        // check pre condition
        //
        self.builder.build_unconditional_branch(pre_condition_block);
        self.builder.position_at_end(pre_condition_block);

        if let Some(cond) = pre_condition {
            let cond = self.gen_expr(cond, env, break_catcher, continue_catcher)?.ok_or(CompileError::condition_is_not_number(None, cond))?;
            let comparison = cond.get_value().into_int_value();
            self.builder.build_conditional_branch(comparison, start_block, end_block);
        }else{
            self.builder.build_unconditional_branch(start_block);
        }

        // loop start
        self.builder.position_at_end(start_block);

        //
        // body
        //
        if let Some(stmt) = body {
            self.gen_stmt(stmt, env, break_catcher, continue_catcher)?;
        }

        //
        // update
        //
        self.builder.build_unconditional_branch(update_block);
        self.builder.position_at_end(update_block);
        if let Some(expr) = update_expr {
            self.gen_expr(expr, env, break_catcher, continue_catcher)?;
        }

        //
        // check post condtition
        //
        if let Some(cond) = post_condition {
            let cond = self.gen_expr(cond, env, break_catcher, continue_catcher)?.ok_or(CompileError::condition_is_not_number(None, cond))?;
            let comparison = cond.get_value().into_int_value();
            self.builder.build_conditional_branch(comparison, pre_condition_block, end_block);
        }else{
            self.builder.build_unconditional_branch(pre_condition_block);
        }

        // loop end
        self.builder.position_at_end(end_block);

        Ok(None)
    }

    fn bin_expr_implicit_cast(&self, left: CompiledValue<'ctx>, right: CompiledValue<'ctx>) -> Result<(CompiledValue<'ctx>, CompiledValue<'ctx>), Box<dyn Error>> {
        if let (Type::Number(left_type), Type::Number(right_type)) = (left.get_type(), right.get_type()) {
            if left_type == right_type {
println!("no cast");
                Ok((left, right))
            }else if left_type < right_type {
//                 let value = self.gen_cast(&left.get_value(), &right_type.to_basic_type_enum(self.context)?)?;
// println!("CAST left type {:?} to {:?}", left_type, right_type);
//                 Ok((CompiledValue::new(Type::Number(right_type.clone()), value), right))
                unimplemented!()
            }else{  // left_type > right_type
//                 let value = self.gen_cast(&right.get_value(), &left_type.to_basic_type_enum(self.context)?)?;
// println!("CAST right type {:?} to {:?}", right_type, left_type);
//                 Ok((left, CompiledValue::new(Type::Number(right_type.clone()), value)))
                unimplemented!()
            }
        }else{
            Ok((left, right))
        }
    }

    fn gen_bin_expr<'b, 'c>(&self,
         op: &BinOp,
         left: &ExprAST,
         right: &ExprAST,
         env: &mut Env<'ctx>,
         break_catcher: Option<&'b BreakCatcher>,
         continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {
        match op {
            BinOp::Add => {
                let left = self.gen_expr(&left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    let result = self.builder.build_int_add(left_value.into_int_value(), right_value.into_int_value(), "add_int");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_add(left_value.into_float_value(), right_value.into_float_value(), "add_float");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CompileError::cannot_add_value(None, left_type)))
                }
            },
            BinOp::Sub => {
                // let left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();
                // let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();

                // let result = self.builder.build_int_sub(left, right, "sub");
                // Ok(Some(result.as_any_value_enum()))
                let left = self.gen_expr(&left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    let result = self.builder.build_int_sub(left_value.into_int_value(), right_value.into_int_value(), "sub_int");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_sub(left_value.into_float_value(), right_value.into_float_value(), "sub_float");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CompileError::cannot_sub_value(None, left_type)))
                }
            },
            BinOp::Mul => {
                // let left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();
                // let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();

                // let result = self.builder.build_int_mul(left, right, "mul");
                // Ok(Some(result.as_any_value_enum()))
                let left = self.gen_expr(&left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    let result = self.builder.build_int_mul(left_value.into_int_value(), right_value.into_int_value(), "mul_int");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_mul(left_value.into_float_value(), right_value.into_float_value(), "mul_float");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CompileError::cannot_mul_value(None, left_type)))
                }
            },
            BinOp::Div => {
                // let left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();
                // let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();

                // let result = self.builder.build_int_signed_div(left, right, "div");
                // Ok(Some(result.as_any_value_enum()))
                let left = self.gen_expr(&left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    if left_type.is_signed()? {
                        let result = self.builder.build_int_signed_div(left_value.into_int_value(), right_value.into_int_value(), "div_int");
                        Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }else{
                        let result = self.builder.build_int_unsigned_div(left_value.into_int_value(), right_value.into_int_value(), "div_int");
                        Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_div(left_value.into_float_value(), right_value.into_float_value(), "div_float");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CompileError::cannot_div_value(None, left_type)))
                }
            },
            BinOp::Mod => {
                // let left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();
                // let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();

                // let result = self.builder.build_int_signed_rem(left, right, "mod");
                // Ok(Some(result.as_any_value_enum()))
                let left = self.gen_expr(&left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    if left_type.is_signed()? {
                        let result = self.builder.build_int_signed_rem(left_value.into_int_value(), right_value.into_int_value(), "mod_int");
                        Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }else{
                        let result = self.builder.build_int_unsigned_rem(left_value.into_int_value(), right_value.into_int_value(), "mod_int");
                        Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_rem(left_value.into_float_value(), right_value.into_float_value(), "mod_float");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CompileError::cannot_mod_value(None, left_type)))
                }
            },
            BinOp::Equal => {
                // let left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();
                // let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();

                // let result = self.builder.build_int_compare(IntPredicate::EQ, left, right, "Equal");
                // Ok(Some(result.as_any_value_enum()))
                let left = self.gen_expr(&left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    let result = self.builder.build_int_compare(IntPredicate::EQ, left_value.into_int_value(), right_value.into_int_value(), "eq_int");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_compare(FloatPredicate::OEQ, left_value.into_float_value(), right_value.into_float_value(), "eq_float");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CompileError::cannot_compare_value(None, left_type)))
                }
            },
            BinOp::NotEqual => {
                // let left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();
                // let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();

                // let result = self.builder.build_int_compare(IntPredicate::NE, left, right, "NotEqual");
                // Ok(Some(result.as_any_value_enum()))
                let left = self.gen_expr(&left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    let result = self.builder.build_int_compare(IntPredicate::NE, left_value.into_int_value(), right_value.into_int_value(), "not_eq_int");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_compare(FloatPredicate::ONE, left_value.into_float_value(), right_value.into_float_value(), "not_eq_float");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CompileError::cannot_compare_value(None, left_type)))
                }
            },
            BinOp::Less => {
                // let lhs = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();
                // let rhs = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();

                // let predicate = if env.is_signed(left)? {
                //     IntPredicate::SLT
                // }else{
                //     IntPredicate::ULT
                // };
                // let result = self.builder.build_int_compare(predicate, lhs, rhs, "Less");
                // Ok(Some(result.as_any_value_enum()))
                let left = self.gen_expr(&left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    if left_type.is_signed()? {
                         let result = self.builder.build_int_compare(IntPredicate::SLT, left_value.into_int_value(), right_value.into_int_value(), "less_int");
                        Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }else{
                        let result = self.builder.build_int_compare(IntPredicate::ULT, left_value.into_int_value(), right_value.into_int_value(), "less_int");
                         Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_compare(FloatPredicate::OLT, left_value.into_float_value(), right_value.into_float_value(), "less_float");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CompileError::cannot_compare_value(None, left_type)))
                }
            },
            BinOp::LessEqual => {
                // let lhs = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();
                // let rhs = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();

                // let predicate = if env.is_signed(left)? {
                //     IntPredicate::SLE
                // }else{
                //     IntPredicate::ULE
                // };
                // let result = self.builder.build_int_compare(predicate, lhs, rhs, "LessEqual");
                // Ok(Some(result.as_any_value_enum()))
                let left = self.gen_expr(&left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    if left_type.is_signed()? {
                         let result = self.builder.build_int_compare(IntPredicate::SLE, left_value.into_int_value(), right_value.into_int_value(), "less_eq_int");
                        Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }else{
                        let result = self.builder.build_int_compare(IntPredicate::ULE, left_value.into_int_value(), right_value.into_int_value(), "less_eq_int");
                         Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_compare(FloatPredicate::OLE, left_value.into_float_value(), right_value.into_float_value(), "less_eq_float");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CompileError::cannot_compare_value(None, left_type)))
                }
            },
            BinOp::Greater => {
                // let lhs = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();
                // let rhs = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();

                // let predicate = if env.is_signed(left)? {
                //     IntPredicate::SGT
                // }else{
                //     IntPredicate::UGT
                // };
                // let result = self.builder.build_int_compare(predicate, lhs, rhs, "Greater");
                // Ok(Some(result.as_any_value_enum()))
                let left = self.gen_expr(&left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    if left_type.is_signed()? {
                         let result = self.builder.build_int_compare(IntPredicate::SGT, left_value.into_int_value(), right_value.into_int_value(), "greater_int");
                        Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }else{
                        let result = self.builder.build_int_compare(IntPredicate::UGT, left_value.into_int_value(), right_value.into_int_value(), "greater_int");
                         Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_compare(FloatPredicate::OGT, left_value.into_float_value(), right_value.into_float_value(), "greater_float");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CompileError::cannot_compare_value(None, left_type)))
                }
            },
            BinOp::GreaterEqual => {
                // let lhs = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();
                // let rhs = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();

                // let predicate = if env.is_signed(left)? {
                //     IntPredicate::SGE
                // }else{
                //     IntPredicate::UGE
                // };
                // let result = self.builder.build_int_compare(predicate, lhs, rhs, "GreaterEqual");
                // Ok(Some(result.as_any_value_enum()))
                let left = self.gen_expr(&left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    if left_type.is_signed()? {
                         let result = self.builder.build_int_compare(IntPredicate::SGE, left_value.into_int_value(), right_value.into_int_value(), "greater_eq_int");
                        Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }else{
                        let result = self.builder.build_int_compare(IntPredicate::UGE, left_value.into_int_value(), right_value.into_int_value(), "greater_eq_int");
                         Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_compare(FloatPredicate::OGE, left_value.into_float_value(), right_value.into_float_value(), "greater_eq_float");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CompileError::cannot_compare_value(None, left_type)))
                }
            },
            BinOp::And => {
                // let left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();
                // let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();

                // let result = self.builder.build_and(left, right, "And");
                // Ok(Some(result.as_any_value_enum()))
                let left = self.gen_expr(&left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    let result = self.builder.build_and(left_value.into_int_value(), right_value.into_int_value(), "and_int");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CompileError::cannot_apply_logical_op_value(None, left_type)))
                }
            },
            BinOp::Or => {
                // let left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();
                // let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();

                // let result = self.builder.build_or(left, right, "Or");
                // Ok(Some(result.as_any_value_enum()))
                let left = self.gen_expr(&left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    let result = self.builder.build_or(left_value.into_int_value(), right_value.into_int_value(), "or_int");
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CompileError::cannot_apply_logical_op_value(None, left_type)))
                }
            },
            BinOp::Comma => {
                let _left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;

                Ok(Some(right))
            },
            BinOp::ShiftLeft => {
                let left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let left_type = left.get_type();
                let right_type = right.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if ! left_value.is_int_value() {
                    return Err(Box::new(CompileError::not_int_in_shift(None, &left_type)));
                }
                if ! right_value.is_int_value() {
                    return Err(Box::new(CompileError::not_int_in_shift(None, &right_type)));
                }

                let result = self.builder.build_left_shift(left_value.into_int_value(), right_value.into_int_value(), "ShiftLeft");
                Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
            },
            BinOp::ShiftRight => {
                // let value = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();
                // let count = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();

                // let sign_extend = env.is_signed(left)?;
                // let result = self.builder.build_right_shift(value, count, sign_extend, "ShiftRight");
                // Ok(Some(result.as_any_value_enum()))
                let left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let left_type = left.get_type();
                let right_type = right.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if ! left_value.is_int_value() {
                    return Err(Box::new(CompileError::not_int_in_shift(None, &left_type)));
                }
                if ! right_value.is_int_value() {
                    return Err(Box::new(CompileError::not_int_in_shift(None, &right_type)));
                }

                let result = self.builder.build_right_shift(left_value.into_int_value(), right_value.into_int_value(), left_type.is_signed()?, "ShiftRight");
                Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
            },
            BinOp::BitAnd => {
                // let left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();
                // let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();

                // let result = self.builder.build_and(left, right, "BitAnd");
                // Ok(Some(result.as_any_value_enum()))
                let left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let left_type = left.get_type();
                let right_type = right.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if ! left_value.is_int_value() {
                    return Err(Box::new(CompileError::not_int_bit_and(None, &left_type)));
                }
                if ! right_value.is_int_value() {
                    return Err(Box::new(CompileError::not_int_bit_and(None, &right_type)));
                }

                let result = self.builder.build_and(left_value.into_int_value(), right_value.into_int_value(), "BitAnd");
                Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
            },
            BinOp::BitOr => {
                // let left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();
                // let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();

                // let result = self.builder.build_or(left, right, "BitOr");
                // Ok(Some(result.as_any_value_enum()))
                let left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let left_type = left.get_type();
                let right_type = right.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if ! left_value.is_int_value() {
                    return Err(Box::new(CompileError::not_int_bit_or(None, &left_type)));
                }
                if ! right_value.is_int_value() {
                    return Err(Box::new(CompileError::not_int_bit_or(None, &right_type)));
                }

                let result = self.builder.build_and(left_value.into_int_value(), right_value.into_int_value(), "BitOr");
                Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
            },
            BinOp::BitXor => {
                // let left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();
                // let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?.into_int_value();

                // let result = self.builder.build_xor(left, right, "BitXor");
                // Ok(Some(result.as_any_value_enum()))
                let left = self.gen_expr(&*left, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let right = self.gen_expr(&*right, env, break_catcher, continue_catcher)?.ok_or(CompileError::illegal_end_of_input(None))?;
                let left_type = left.get_type();
                let right_type = right.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if ! left_value.is_int_value() {
                    return Err(Box::new(CompileError::not_int_bit_xor(None, &left_type)));
                }
                if ! right_value.is_int_value() {
                    return Err(Box::new(CompileError::not_int_bit_xor(None, &right_type)));
                }

                let result = self.builder.build_xor(left_value.into_int_value(), right_value.into_int_value(), "BitXor");
                Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
            },
            // _ => unimplemented!("BinExpr op: {:?}", op),
        }
    }

    fn gen_assign<'b, 'c>(&self,
        l_value: &ExprAST,
        r_value: &ExprAST,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {
        let compiled_value = self.gen_expr(r_value, env, break_catcher, continue_catcher)?.ok_or(Box::new(CompileError::assign_illegal_value(None, r_value)))?;
        let value = self.try_as_basic_value(&compiled_value.get_value())?;
        let from_type = compiled_value.get_type();
println!("gen_assign. compiled_value: {:?}", compiled_value);

        let (to_type, ptr) = self.get_l_value(l_value, env, break_catcher, continue_catcher)?;
println!("  FROM: {:?} TO: {:?}", from_type, to_type);
        self.builder.build_store(ptr, value);
        Ok(Some(compiled_value))
    }

    fn get_l_value<'b, 'c>(&self,
        ast: &ExprAST,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<(Type, PointerValue<'ctx>), Box<dyn Error>> {
        match ast {
            ExprAST::Symbol(name) => {
                let (typ, ptr) = env.get_ptr(&name).ok_or(Box::new(CompileError::no_such_a_variable(None, &name)))?;
                Ok((typ.clone(), ptr))
            },
            ExprAST::UnaryPointerAccess(boxed_ast) => {
                let ast = &**boxed_ast;
                let (typ, ptr_to_ptr) = self.get_l_value(ast, env, break_catcher, continue_catcher)?;
                let ptr = self.builder.build_load(ptr_to_ptr, &String::from("get ptr from ptr"));

                Ok((Type::new_pointer_type(typ.clone(), false, false), ptr.into_pointer_value()))
            },
            ExprAST::MemberAccess(expr, member_name) => {
                let ast = &**expr;
                let (_typ, ptr) = self.get_l_value(ast, env, break_catcher, continue_catcher)?;
                let typ = ast.get_type(env)?;

                match typ {
                    Type::Struct {name, fields} => {
                        let index = fields.get_index(member_name).ok_or(CompileError::no_such_a_member(None, member_name))?;
                        let elem_type = fields.get_type(member_name).unwrap();
                        let msg = if let Some(id) = name {
                            format!("struct_{}.{}", id, member_name)
                        }else{
                            format!("struct?.{}", member_name)
                        };
                        let elem_ptr = self.builder.build_struct_gep(ptr, index as u32, &msg);
                        if let Ok(p) = elem_ptr {
                            Ok((elem_type.clone(), p))
                        }else{
                            return Err(Box::new(CompileError::cannot_access_struct_member(None, &member_name)));
                        }
                    },
                    Type::Union { name, fields } => {
                        if let Some(id) = name {
                            let type_or_union = env.get_type(&id).ok_or(CompileError::no_such_a_member(None, member_name))?;
                            match type_or_union {
                                TypeOrUnion::Union { type_list, index_map, max_size: _, max_size_type: _ } => {
                                    let idx = index_map[member_name];
                                    // let any_type_enum = type_list[idx];
                                    // let to_type = BasicTypeEnum::try_from(any_type_enum);
                                    // if let Err(_err) = &to_type {
                                    //     return Err(Box::new(CompileError::cannot_convert_to_basic_type_enum(None)));
                                    // }
                                    // let ptr_type = to_type.unwrap().ptr_type(AddressSpace::Generic);
                                    let (typ, to_type) = &type_list[idx];
                                    let ptr_type = to_type.ptr_type(AddressSpace::Generic);

                                    let p = ptr.const_cast(ptr_type);
                                    Ok((typ.clone(), p))
                                },
                                _ => return Err(Box::new(CompileError::not_union(None, &id))),
                            }
                        }else{
                            let typ = fields.get_type(member_name).ok_or(CompileError::no_such_a_member(None, member_name))?;
                            let to_type = typ.to_basic_type_enum(self.context)?;
                            let ptr_type = to_type.ptr_type(AddressSpace::Generic);
                            let p = ptr.const_cast(ptr_type);
                            Ok((typ.clone(), p))
                        }
                    },
                    _ => unimplemented!("member access to {:?}", typ),
                }
            },
            ExprAST::PointerAccess(expr, member_name) => {
                let ast = &**expr;
                let (_typ, ptr) = self.get_l_value(ast, env, break_catcher, continue_catcher)?;
                let ptr = self.builder.build_load(ptr, &format!("pointer_access_to_{}", member_name)).into_pointer_value();
                let typ = ast.get_type(env)?;
                let typ = typ.get_pointed_type()?;

                match typ {
                    Type::Struct {fields, ..} => {
                        let index = fields.get_index(member_name).ok_or(CompileError::no_such_a_member(None, member_name))?;
                        let elem_ptr = self.builder.build_struct_gep(ptr, index as u32, "struct_member_access");
                        if let Ok(p) = elem_ptr {
                            let typ = fields.get_type(member_name).unwrap();
                            Ok((typ.clone(), p))
                        }else{
                            return Err(Box::new(CompileError::cannot_access_struct_member(None, &member_name)));
                        }
                    },
                    Type::Union { name, fields } => {
                        if let Some(id) = name {
                            let type_or_union = env.get_type(&id).ok_or(CompileError::no_such_a_member(None, member_name))?;
                            match type_or_union {
                                TypeOrUnion::Union { type_list, index_map, max_size: _, max_size_type: _ } => {
                                    let idx = index_map[member_name];
                                    let any_type_enum = &type_list[idx];
                                    // let to_type = BasicTypeEnum::try_from(any_type_enum);
                                    // if let Err(_err) = &to_type {
                                    //     return Err(Box::new(CompileError::cannot_convert_to_basic_type_enum(None)));
                                    // }
                                    // let ptr_type = to_type.unwrap().ptr_type(AddressSpace::Generic);
                                    let (typ, to_type) = &type_list[idx];
                                    let ptr_type = to_type.ptr_type(AddressSpace::Generic);

                                    let p = ptr.const_cast(ptr_type);
                                    Ok((typ.clone(), p))
                                },
                                _ => return Err(Box::new(CompileError::not_union(None, &id))),
                            }
                        }else{
                            let typ = fields.get_type(member_name).ok_or(CompileError::no_such_a_member(None, member_name))?;
                            let to_type = typ.to_basic_type_enum(self.context)?;
                            let ptr_type = to_type.ptr_type(AddressSpace::Generic);
                            let p = ptr.const_cast(ptr_type);
                            Ok((typ.clone(), p))
                        }
                    },
                    _ => unimplemented!("member access to {:?}", typ),
                }
            },
            ExprAST::ArrayAccess(expr, index) => {
                let ast = &**expr;
                let (typ, base_ptr) = self.get_l_value(ast, env, break_catcher, continue_catcher)?;

                let value = self.gen_expr(index, env, break_catcher, continue_catcher)?.ok_or(CompileError::no_index_value_while_access_array(None))?;
                let index_val = value.get_value().into_int_value();

                let i32_type = self.context.i32_type();
                let const_zero = i32_type.const_zero();
                let index_list = [const_zero, index_val];
                let ptr = unsafe { base_ptr.const_in_bounds_gep(&index_list) };

                Ok((typ.clone(), ptr))
            },
            ExprAST::_self => {
                let (typ, ptr) = env.get_ptr("self").ok_or(Box::new(CompileError::no_such_a_variable(None, "self")))?;
                Ok((typ.clone(), ptr))
            },
            ExprAST::_Self => {



                unimplemented!()
            },


            _ => unimplemented!("get_l_value from {:?}", ast),
        }
    }

    fn gen_block<'b, 'c>(&self,
        block: &'ctx Block,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {
        env.add_new_local();
        let result = self.gen_block_sub(block, env, break_catcher, continue_catcher);
        env.remove_local();

        result
    }

    fn gen_block_sub<'b, 'c>(&self,
        block: &'ctx Block,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {
        for ast in block.body.iter() {
            let _any_value = self.gen_stmt(ast, env, break_catcher, continue_catcher)?;
        }

        Ok(None)
    }

    fn make_fun_type(name: &str, ret_type: &Type, params: &Params, _env: &Env) -> Result<Type, CompileError> {
        Ok(Type::new_function_type(
            Some(name.to_string()),
            ret_type.clone(),
            params.get_params_type(),
            params.has_variadic()
        ))
    }

    fn to_function(&self, any_val: &AnyValueEnum<'ctx>) -> Result<FunctionValue<'ctx>, CompileError> {
        match any_val {
            AnyValueEnum::FunctionValue(fun) => Ok(*fun),
            _ => Err(CompileError::cannot_call_not_function(None, any_val)),
        }
    }

    fn try_as_basic_metadata_value(&self, any_val: &AnyValueEnum<'ctx>) -> Result<BasicMetadataValueEnum<'ctx>, CompileError> {
        match any_val {
            AnyValueEnum::ArrayValue(val) => Ok(BasicMetadataValueEnum::ArrayValue(*val)),
            AnyValueEnum::IntValue(val) => Ok(BasicMetadataValueEnum::IntValue(*val)),
            AnyValueEnum::FloatValue(val) => Ok(BasicMetadataValueEnum::FloatValue(*val)),
            AnyValueEnum::PointerValue(val) => Ok(BasicMetadataValueEnum::PointerValue(*val)),
            AnyValueEnum::StructValue(val) => Ok(BasicMetadataValueEnum::StructValue(*val)),
            AnyValueEnum::VectorValue(val) => Ok(BasicMetadataValueEnum::VectorValue(*val)),
            _ => Err(CompileError::cannot_convert_anyvalueenum_to_basicmetadatavalueenum(None, any_val)),
        }
    }

    fn try_as_basic_value(&self, any_val: &AnyValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>, CompileError> {
        match any_val {
            AnyValueEnum::ArrayValue(val) => Ok(BasicValueEnum::ArrayValue(*val)),
            AnyValueEnum::IntValue(val) => Ok(BasicValueEnum::IntValue(*val)),
            AnyValueEnum::FloatValue(val) => Ok(BasicValueEnum::FloatValue(*val)),
            AnyValueEnum::PointerValue(val) => Ok(BasicValueEnum::PointerValue(*val)),
            AnyValueEnum::StructValue(val) => Ok(BasicValueEnum::StructValue(*val)),
            AnyValueEnum::VectorValue(val) => Ok(BasicValueEnum::VectorValue(*val)),
            _ => Err(CompileError::cannot_convert_anyvalueenum_to_basicvalueenum(None, any_val)),
        }
    }

    fn try_as_basic_type(&self, any_type: &AnyTypeEnum<'ctx>) -> Result<BasicTypeEnum<'ctx>, CompileError> {
        match any_type {
            AnyTypeEnum::ArrayType(val) => Ok(BasicTypeEnum::ArrayType(*val)),
            AnyTypeEnum::IntType(val) => Ok(BasicTypeEnum::IntType(*val)),
            AnyTypeEnum::FloatType(val) => Ok(BasicTypeEnum::FloatType(*val)),
            AnyTypeEnum::PointerType(val) => Ok(BasicTypeEnum::PointerType(*val)),
            AnyTypeEnum::StructType(val) => Ok(BasicTypeEnum::StructType(*val)),
            AnyTypeEnum::VectorType(val) => Ok(BasicTypeEnum::VectorType(*val)),
            _ => Err(CompileError::cannot_convert_anytypeenum_to_basictypeenum(None, any_type)),
        }
    }

    fn gen_code_function_prototype(&self,
        fn_name: &str,
        ret_type: &Type,
        params: &Params,
        env: &mut Env<'ctx>
    ) -> Result<FunctionValue<'ctx>, Box<dyn Error>> {
        let fn_type = self.make_function_type(&ret_type, params, env)?;
        let function = self.module.add_function(fn_name, fn_type, None);

        Ok(function)
    }

    fn gen_code_function<'b, 'c>(&self,
        fn_name: &str,
        ret_type: &Type,
        params: &'ctx Params,
        body: &'ctx Block,
        labels: &Vec<String>,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<FunctionValue<'ctx>, Box<dyn Error>> {

        let fn_type = self.make_function_type(&ret_type, params, env)?;
        // TODO: 同名のプロトタイプが存在しないか確認する。
        //       存在した場合、プロトタイプと関数の型が一致するかチェックする。

        // prologue
        let function = self.module.add_function(fn_name, fn_type, None);
        env.set_current_function(function);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        env.add_new_function_local();
        let result = self.gen_code_function_sub(&fn_type, &function, params, body, labels, env, break_catcher, continue_catcher);
        env.remove_function_local();

        if let Err(err) = result {
            Err(err)
        }else{
            Ok(function)
        }
    }

    fn gen_code_function_sub<'b, 'c>(&self,
        fn_type: &FunctionType,
        function: &FunctionValue<'ctx>,
        params: &Params,
        body: &'ctx Block,
        labels: &Vec<String>,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<(), Box<dyn Error>> {
        // selfの処理（あれば）
        if let Some(cust_self) = params.get_self() {
            let typ = cust_self.get_type();
            let name = "self";
            let ptr = function.get_nth_param(0).unwrap().into_pointer_value();
            env.insert_local(name, typ.clone(), ptr);
        }

        // 引数の処理
        for (i, param) in params.get_params().iter().enumerate() {
            let typ = param.get_type();
            let name = param.get_name();
            let ptr = self.builder.build_alloca(typ.to_basic_type_enum(self.context)?, name);
            let value = function.get_nth_param(i as u32).unwrap();
            self.builder.build_store(ptr, value);
            env.insert_local(name, typ.clone(), ptr);
        }

        // labels
        for id in labels {
            let block = self.context.append_basic_block(*function, id);
            env.insert_label(&id, block)?;
        }

        // body
        let mut last_stmt = None;
        for ast in body.body.iter() {
            let _any_value = self.gen_stmt(ast, env, break_catcher, continue_catcher)?;
            last_stmt = Some(ast);
        }

        match fn_type.get_return_type() {
            None => {  // 戻り値の型がvoidで、かつ、最後の行がreturn文でないときに、build_returnしておく。
                match last_stmt {
                    Some(AST::Return(None)) => Ok(()),  // do nothing
                    Some(AST::Return(Some(expr))) => {
                        let typ: BasicTypeEnum = expr.get_type(env)?.to_basic_type_enum(self.context)?;
                        return Err(Box::new(CompileError::return_type_mismatch(None, Some(&fn_type.get_return_type().unwrap()), Some(&typ))));
                    },
                    _ => {
                        self.builder.build_return(None);
                        Ok(())
                    },
                }
            },
            Some(ret_type) => {  // TODO: 関数が返す型をチェックする。
                // 最後の文の型をチェックする。
                match last_stmt {
                    Some(AST::Return(None)) => {
                        return Err(Box::new(CompileError::no_return_for_type(None, &fn_type.get_return_type().unwrap())));
                    },
                    Some(AST::Return(Some(_expr))) => {
                        // let expr_type = expr.get_type(env)?.to_basic_type_enum(self.context)?;
                        // if ret_type != expr_type {
                            // return Err(Box::new(CompileError::return_type_mismatch(None, Some(&ret_type), Some(&expr_type))));
                        // }

                        Ok(())
                    },
                    Some(AST::If(_cond, _then, _else)) => {
                        if let Some(typ) = self.calc_ret_type(last_stmt.unwrap(), env)? {
                            if ret_type == typ {
                                // 最後の文が、ifのとき、ラベルif.endの後にコードが生成されないのでセグフォが起きることへのケア
                                self.builder.build_return(None);

                                Ok(())
                            }else{
                                Err(Box::new(CompileError::return_type_mismatch(None, Some(&ret_type), Some(&typ))))
                            }

                        }else{
                            Err(Box::new(CompileError::return_type_mismatch(None, Some(&ret_type), None)))
                        }
                    },
                    Some(stmt) => {  // TODO: if文などの時に内部でreturnしているかもしれないので、その処理。
                        if let Some(typ) = self.calc_ret_type(stmt, env)? {
                            if ret_type == typ {
                                Ok(())
                            }else{
                                Err(Box::new(CompileError::return_type_mismatch(None, Some(&ret_type), Some(&typ))))
                            }

                        }else{
                            Err(Box::new(CompileError::return_type_mismatch(None, Some(&ret_type), None)))
                        }

                    },
                    _ => {
                        // return Err(Box::new(CompileError::no_return_for_type(None, &ret_type)))
                        Ok(())
                    },
                }
            },
        }
    }

    fn calc_ret_type(&self, stmt: &AST, env: &Env) -> Result<Option<BasicTypeEnum>, Box<dyn Error>> {
        match stmt {
            AST::Return(None) => {
                Ok(None)
            },
            AST::Return(Some(expr)) => {
                let expr_type = expr.get_type(env)?.to_basic_type_enum(self.context)?;
                Ok(Some(expr_type))
            },
            AST::If(_cond, if_then, if_else) => {
                let then_type = self.calc_ret_type(if_then, env)?;

                if let Some(typ1) = then_type {
                    if let Some(else_expr) = if_else {
                        if let Some(else_type) = self.calc_ret_type(else_expr, env)? {
                            if typ1 == else_type {
                                Ok(Some(typ1))
                            }else{
                                Err(Box::new(CompileError::mismatch_type_in_if(None, then_type, Some(else_type))))
                            }

                        }else{
                            Err(Box::new(CompileError::mismatch_type_in_if(None, then_type, None)))
                        }

                    }else{
                        Err(Box::new(CompileError::mismatch_type_in_if(None, then_type, None)))
                    }

                }else{
                    if let Some(else_expr) = if_else {
                        let else_type = self.calc_ret_type(else_expr, env)?;
                        if let Some(_typ) = else_type {
                            Err(Box::new(CompileError::mismatch_type_in_if(None, then_type, else_type)))
                        }else{
                            Ok(None)
                        }

                    }else{
                        Ok(None)
                    }
                }
            },
            AST::Block(blk) => {
                let mut typ = None;

                for e in &blk.body {
                    typ = self.calc_ret_type(e, env)?;

                    match e {
                        AST::Return(_) => {
                            break;
                        },
                        _ => (),  // do nothing
                    }
                }

                Ok(typ)
            },
            AST::Switch(..) => {
                Ok(None)
            },
            _ => {
                Ok(None)
            },
        }
    }

    fn make_function_type(&self, ret_type: &Type, params: &Params, _env: &Env<'ctx>) -> Result<FunctionType<'ctx>, Box<dyn Error>> {
        let ret_t = ret_type.to_llvm_any_type(self.context)?;
        let has_variadic = params.has_variadic();

        let mut arg_type_vec = Vec::new();

        if let Some(cust_self) = params.get_self() {
            let typ = cust_self.get_type();
            let typ = Type::new_pointer_type(typ.clone(), false, false);
            let t = typ.to_llvm_type(self.context)?;

            arg_type_vec.push(t);
        }

        for ds in params.get_params() {
            let typ = ds.get_type();

            let t = typ.to_llvm_type(self.context)?;
            arg_type_vec.push(t);
        }
        let params_type = &arg_type_vec;  // get slice

        match ret_t {
            AnyTypeEnum::IntType(t) => {
                Ok(t.fn_type(&params_type, has_variadic))
            },
            AnyTypeEnum::FloatType(t) => {
                Ok(t.fn_type(&params_type, has_variadic))
            },
            AnyTypeEnum::VoidType(t) => {
                Ok(t.fn_type(&params_type, has_variadic))
            },
            AnyTypeEnum::PointerType(t) => {
                Ok(t.fn_type(&params_type, has_variadic))
            },
            AnyTypeEnum::ArrayType(t) => {
                Ok(t.fn_type(&params_type, has_variadic))
            },
            AnyTypeEnum::StructType(t) => {
                Ok(t.fn_type(&params_type, has_variadic))
            },
            AnyTypeEnum::VectorType(t) => {
                Ok(t.fn_type(&params_type, has_variadic))
            },
            // AnyTypeEnum::FunctionType(t) => {
            //     Ok(t.fn_type(&params_type, false))
            // },
            _ => Err(Box::new(CompileError::cannot_make_fn_type(None))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use inkwell::execution_engine::{JitFunction};
    use inkwell::values::BasicValue;

    type FuncType_void_i32 = unsafe extern "C" fn() -> i32;
    type FuncType_i32_i32 = unsafe extern "C" fn(i32) -> i32;
    type FuncType_i32i32_i32 = unsafe extern "C" fn(i32, i32) -> i32;
    type FuncType_i32i32i32_i32 = unsafe extern "C" fn(i32, i32, i32) -> i32;
    type FuncType_void_void = unsafe extern "C" fn() -> ();

    fn gen_prologue<'ctx>(gen: &CodeGen<'ctx>, fn_name: &str, fn_type: FunctionType<'ctx>) {
        let function = gen.module.add_function(fn_name, fn_type, None);
        let basic_block = gen.context.append_basic_block(function, "entry");

        gen.builder.position_at_end(basic_block);
    }

    fn gen_epilogue<'ctx>(gen: &CodeGen<'ctx>, fn_name: &str, ret_code: Option<&dyn BasicValue<'ctx>>) -> Option<JitFunction<'ctx, FuncType_void_i32>> {
        gen.builder.build_return(ret_code);

        unsafe { gen.execution_engine.get_function(fn_name).ok() }
    }

    fn code_gen_from_str(input: &str) -> Result<i32, Box<dyn Error>> {
        // let parser = Parser::new();
        let ast = &Parser::parse_expression_from_str(input)?.unwrap();

        let fn_name = "from_str_function";
        let context = Context::create();
        let gen = CodeGen::try_new(&context, "module_from_str")?;
        let i32_type = gen.context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        gen_prologue(&gen, fn_name, fn_type);

        let mut env = Env::new();
        let value = gen.gen_expr(&ast, &mut env, None, None)?.ok_or(CompileError::illegal_end_of_input(None))?;
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
            _ => unimplemented!(),
        };

        unsafe {
            Ok(func.call())
        }
    }
    

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
        let parser = Parser::new();
        let ast = &parser.parse_from_str(src).unwrap()[0];

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
        let parser = Parser::new();
        let ast = &parser.parse_from_str("
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
        let parser = Parser::new();
        let ast = &parser.parse_from_str(src).unwrap()[0];

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
        let parser = Parser::new();
        let ast = &parser.parse_from_str(src).unwrap()[0];

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
    fn code_gen_fun_helloworld() {
        let src = "
            int printf(char* format, ...);

            void hello_world(){
                printf(\"Hello, World!\\\n\");
                return;
            }
        ";

        // parse
        let parser = Parser::new();
        let asts = parser.parse_from_str(src).unwrap();

        // code gen
        let context = Context::create();
        let gen = CodeGen::try_new(&context, "test run").unwrap();

        let mut env = Env::new();

        for i in 0..asts.len() {
            let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None).unwrap();
        }

        let f: JitFunction<FuncType_void_void> = unsafe { gen.execution_engine.get_function("hello_world").ok().unwrap() };
        let _result = unsafe { f.call() };
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
        let parser = Parser::new();
        let asts = parser.parse_from_str(src).unwrap();

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
        let parser = Parser::new();
        let asts = parser.parse_from_str(src).unwrap();

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
        let parser = Parser::new();
        let asts = parser.parse_from_str(src).unwrap();

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
        let parser = Parser::new();
        let asts = parser.parse_from_str(src).unwrap();

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
        let parser = Parser::new();
        let asts = parser.parse_from_str(src).unwrap();

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
        let parser = Parser::new();
        let asts = parser.parse_from_str(src).unwrap();

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
    fn code_gen_increment() {
        let src = "
            int printf(char* format, ...);

            int test(int x){
                x++;
                ++x;
                return x;
            }
        ";

        // parse
        let parser = Parser::new();
        let asts = parser.parse_from_str(src).unwrap();

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
    fn code_gen_decrement() {
        let src = "
            int printf(char* format, ...);

            int test(int x){
                x--;
                --x;
                return x;
            }
        ";

        // parse
        let parser = Parser::new();
        let asts = parser.parse_from_str(src).unwrap();

        // code gen
        let context = Context::create();
        let gen = CodeGen::try_new(&context, "test run").unwrap();

        let mut env = Env::new();

        for i in 0..asts.len() {
            let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None).unwrap();
        }

        let f: JitFunction<FuncType_i32_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
        assert_eq!(unsafe { f.call(0) }, -2);
        assert_eq!(unsafe { f.call(1) }, -1);
    }

    #[test]
    fn code_gen_multiple_pointer_declaration() -> Result<(), CompileError> {
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
        let parser = Parser::new();
        let asts = parser.parse_from_str(src).unwrap();

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
    fn code_gen_struct() -> Result<(), CompileError> {
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
        let parser = Parser::new();
        let asts = parser.parse_from_str(src).unwrap();

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
    fn code_gen_union() -> Result<(), CompileError> {
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
        let parser = Parser::new();
        let asts = parser.parse_from_str(src).unwrap();

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
    fn code_gen_array() -> Result<(), CompileError> {
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
        let parser = Parser::new();
        let asts = parser.parse_from_str(src).unwrap();

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
        let parser = Parser::new();
        let asts = parser.parse_from_str(src).unwrap();

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
        let parser = Parser::new();
        let asts = parser.parse_from_str(src).unwrap();

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
    fn code_gen_enum() {
        let src = "
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
        let parser = Parser::new();
        let asts = parser.parse_from_str(src).unwrap();

        // code gen
        let context = Context::create();
        let gen = CodeGen::try_new(&context, "test run").unwrap();

        let mut env = Env::new();
        for i in 0..asts.len() {
            let _any_value = gen.gen_stmt(&asts[i], &mut env, None, None).unwrap();
        }

        let f: JitFunction<FuncType_void_i32> = unsafe { gen.execution_engine.get_function("test").ok().unwrap() };
        assert_eq!(unsafe { f.call() }, 0 + 1 + 2 + 10 + 11 + 12 + 13);
    }
}
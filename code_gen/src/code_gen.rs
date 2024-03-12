#![allow(dead_code)]

use crate::parser::{AST, ToplevelAST, ExprAST, BinOp, Type, Pointer, Block, Params, StructDefinition, StructField, NumberType, Function, FunProto, FunOrProto, EnumDefinition, Enumerator};
use crate::parser::{Declaration, DeclarationSpecifier, CustFunctionType, Initializer};
use super::{CompiledValue, CodeGenError};
use super::Env;
use super::env::{BreakCatcher, ContinueCatcher};
use super::caster::Caster;
use super::type_util::TypeUtil;
#[cfg(test)]
use crate::parser::{SpecifierQualifier, DirectDeclarator, Declarator, Defines, Param};
use crate::parser::{Switch, Case};
use crate::Position;

use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, GlobalValue, InstructionOpcode, InstructionValue, IntValue, PointerValue, StructValue};
use inkwell::types::{BasicTypeEnum, AnyTypeEnum, FunctionType, BasicType, BasicMetadataTypeEnum};
use inkwell::basic_block::BasicBlock;
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

    pub fn gen_toplevels(&self, asts: &'ctx Vec<ToplevelAST>, env: &mut Env<'ctx>) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {
        let mut any_value = None;

        for ast in asts {
            any_value = self.gen_toplevel(&ast, env, None, None)?;
        }

        Ok(any_value)
    }

    pub fn gen_toplevel<'b, 'c>(&self,
        ast: &'ctx ToplevelAST,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        match ast {
            ToplevelAST::GlobalDefVar{specifiers, declaration, pos} => {
                let base_type = specifiers.get_type();

                for decl in declaration {
                    let declarator = decl.get_declarator();
                    let typ = declarator.make_type(base_type);
                    let name = declarator.get_name();
                    let basic_type = TypeUtil::to_basic_type_enum(&typ, self.context, ast.get_position())?;
                    let ptr = self.module.add_global(basic_type, Some(AddressSpace::default()), name);
    
                    match decl.get_init_expr() {
                        Some(initializer) => {
                            match &typ {
                                Type::Struct { fields, .. } => {
                                    self.gen_global_struct_init(&fields, ptr, &*initializer, env, break_catcher, continue_catcher)?;
                                },
                                Type::Array { name: _, typ, size_list } => {
                                    self.gen_global_array_init(&size_list, ptr, &*typ, &*initializer, env, break_catcher, continue_catcher)?;
                                },
                                _ => {
                                    let init = self.gen_const_expr(initializer, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(pos.clone()))?;
                                    let basic_value = self.try_as_basic_value(&init.get_value(), initializer.get_position())?;
            
                                    ptr.set_initializer(&basic_value);
                                }
                            }
                        },
                        None => (),  // do nothing
                    };
    
                    env.insert_global_var(name, typ.clone(), ptr);
                }

                Ok(None)
            },
            ToplevelAST::TypeDef(name, typ, pos) => {
                env.insert_typedef(name, typ, self.context, pos)?;
                Ok(None)
            },
            ToplevelAST::Function(Function {specifiers, declarator, params, body, labels}, pos) => {
                let name = declarator.get_name();
                let sp_type = specifiers.get_type();
                let ret_type = declarator.make_type(&sp_type);
                let function = self.gen_code_function(name, &ret_type, params, body, labels, env, break_catcher, continue_catcher, pos)?;
                let fun_type = Self::make_fun_type(&name, &ret_type, params, env)?;

                env.insert_function(&name, fun_type, function);

                Ok(Some(AnyValueEnum::FunctionValue(function)))
            },
            ToplevelAST::FunProto(FunProto {specifiers, declarator, params}, pos) => {
                let name = declarator.get_name();
                let sp_type = specifiers.get_type();
                let ret_type = declarator.make_type(&sp_type);
                let fun_proto = self.gen_code_function_prototype(name, &ret_type, params, env, pos)?;
                let fun_type = Self::make_fun_type(&name, &ret_type, params, env)?;

                env.insert_function(&name, fun_type, fun_proto);

                Ok(Some(AnyValueEnum::FunctionValue(fun_proto)))
            },
            ToplevelAST::DefineStruct{name, fields, pos} => {
                self.gen_define_struct(name, fields, env, break_catcher, continue_catcher, pos)?;
                Ok(None)
            },
            ToplevelAST::DefineUnion { name, fields, pos } => {
                self.gen_define_union(name, fields, env, break_catcher, continue_catcher, pos)?;
                Ok(None)
            },
            ToplevelAST::DefineEnum { name, fields, pos } => {
                self.gen_define_enum(name, fields, env, break_catcher, continue_catcher, pos)?;
                Ok(None)
            },
            ToplevelAST::Impl { name, typ, for_type, functions, pos } => {
                self.gen_impl(name, typ, for_type, functions, env, break_catcher, continue_catcher, pos)?;

                Ok(None)
            },
        }
    }

    pub fn gen_stmt<'b, 'c>(&self,
        ast: &'ctx AST,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        match ast {
            AST::DefVar{specifiers, declarations, pos: _} => {
                self.gen_def_var(specifiers, declarations, env, break_catcher, continue_catcher)?;
                Ok(None)
            },
            AST::Block(block, _pos) => self.gen_block(block, env, break_catcher, continue_catcher),
            AST::Return(opt_expr, pos) => {
                let fun_typ = env.get_current_function_type().ok_or(CodeGenError::return_without_function(pos.clone()))?;
                let required_ret_type = fun_typ.get_return_type();

                let result: InstructionValue;
                if let Some(expr) = opt_expr {
                    let mut real_ret = self.gen_expr(expr, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(expr.get_position().clone()))?;
                    let real_ret_type = real_ret.get_type();

                    if required_ret_type != real_ret_type {
                        let casted = self.gen_implicit_cast(&real_ret.get_value(), &real_ret_type, &required_ret_type, expr.get_position())?;
                        real_ret = CompiledValue::new(real_ret.get_type().clone(), casted);
                    }

                    let ret = self.try_as_basic_value(&real_ret.get_value(), expr.get_position())?;
                    result = self.builder.build_return(Some(&ret))?;
                }else{
                    if ! required_ret_type.is_void() {
                        return Err(Box::new(CodeGenError::return_type_mismatch(Type::Void, required_ret_type.clone(), pos.clone())));
                    }

                    result = self.builder.build_return(None)?;
                }
                Ok(Some(result.as_any_value_enum()))
            },
            AST::If(condition, then, _else, pos) => {
                let (_fun_type, func) = env.get_current_function().ok_or(CodeGenError::no_current_function(pos.clone()))?;
                let func = func.clone();
                let cond_block = self.context.append_basic_block(func, "if.cond");
                let then_block = self.context.append_basic_block(func, "if.then");
                let else_block = self.context.append_basic_block(func, "if.else");
                let end_block  = self.context.append_basic_block(func, "if.end");

                self.builder.build_unconditional_branch(cond_block)?;
                self.builder.position_at_end(cond_block);

                // check condition
                let cond = self.gen_expr(condition, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::condition_is_not_number(condition, (**condition).get_position().clone()))?;
                let mut comparison = cond.get_value().into_int_value();
                let i1_type = self.context.bool_type();
                comparison = self.builder.build_int_cast(comparison, i1_type, "cast to i1")?;  // cast to i1
                self.builder.build_conditional_branch(comparison, then_block, else_block)?;

                // then block
                self.builder.position_at_end(then_block);
                self.gen_stmt(then, env, break_catcher, continue_catcher)?;
                if let Some(blk) = self.builder.get_insert_block() {
                    if ! self.last_is_jump_statement(blk) {
                        self.builder.position_at_end(blk);
                        self.builder.build_unconditional_branch(end_block)?;
                    }
                }
                if ! self.last_is_jump_statement(then_block) {
                    self.builder.position_at_end(then_block);
                    self.builder.build_unconditional_branch(end_block)?;
                }

                // else block
                self.builder.position_at_end(else_block);
                if let Some(expr) = _else {
                    self.gen_stmt(expr, env, break_catcher, continue_catcher)?;
                    if let Some(blk) = self.builder.get_insert_block() {
                        if ! self.last_is_jump_statement(blk) {
                            self.builder.position_at_end(blk);
                            self.builder.build_unconditional_branch(end_block)?;
                        }
                    }
                }
                if ! self.last_is_jump_statement(else_block) {
                    self.builder.position_at_end(else_block);
                    self.builder.build_unconditional_branch(end_block)?;
                }

                // end block
                self.builder.position_at_end(end_block);

                Ok(None)
            },
            AST::Loop {init_expr, pre_condition, body, update_expr, post_condition, pos} => {
                self.gen_loop(init_expr, pre_condition, body, update_expr, post_condition, env, break_catcher, continue_catcher, pos)
            },
            AST::Break(pos) => {
                let break_block = break_catcher.ok_or(CodeGenError::break_not_in_loop_or_switch(pos.clone()))?.get_block();
                self.builder.build_unconditional_branch(*break_block)?;

                Ok(None)
            },
            AST::Continue(pos) => {
                let continue_block = continue_catcher.ok_or(CodeGenError::continue_not_in_loop(pos.clone()))?.get_block();
                self.builder.build_unconditional_branch(*continue_block)?;

                Ok(None)
            },
            AST::Goto(id, pos) => {
                let block = env.get_block(id).ok_or(CodeGenError::no_such_a_label(id, pos.clone()))?;
                self.builder.build_unconditional_branch(*block)?;

                Ok(None)
            },
            AST::Labeled(id, opt_stmt, pos) => {
                let block = env.get_block(id).ok_or(CodeGenError::no_such_a_label(id, pos.clone()))?;
                self.builder.build_unconditional_branch(*block)?;
                self.builder.position_at_end(*block);

                if let Some(stmt) = opt_stmt {
                    self.gen_stmt(stmt, env, break_catcher, continue_catcher)
                }else{
                    Ok(None)
                }
            },
            AST::Switch(switch, pos) => {
                self.gen_switch(switch, env, break_catcher, continue_catcher, pos)
            },
            AST::Case(case, _pos) => {
                self.gen_case(case, env, break_catcher, continue_catcher)
            },
            AST::Default(stmt, pos) => {
                self.gen_default(stmt, env, break_catcher, continue_catcher, pos)
            },
            AST::_self(pos) => {
                if let Some((_typ, ptr)) = env.get_self_ptr() {
                    let basic_val = self.builder.build_load(ptr, "get_self")?;
                    let any_val = basic_val.as_any_value_enum();

                    Ok(Some(any_val))
                }else{
                    Err(Box::new(CodeGenError::no_such_a_variable("self", pos.clone())))
                }
            },
            AST::Expr(expr_ast, _pos) => {
                if let Some(value) = self.gen_expr(expr_ast, env, break_catcher, continue_catcher)? {
                    Ok(Some(value.get_value()))
                }else{
                    Ok(None)
                }
            },
            AST::_Self(pos) => {
                Err(Box::new(CodeGenError::self_is_not_statement(pos.clone())))
            },
        }
    }

    pub fn gen_expr<'b, 'c>(&self,
        expr_ast: &ExprAST,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {

        match expr_ast {
            ExprAST::Char(num, _pos) => {
                let i8_type = self.context.i8_type();
                let result = i8_type.const_int(*num as u64, true);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::Char), result.as_any_value_enum())))
            },
            ExprAST::Int(num, _pos) => {
                let i32_type = self.context.i32_type();
                let result = i32_type.const_int(*num as u64, true);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::Int), result.as_any_value_enum())))
            },
            ExprAST::Short(num, _pos) => {
                let i16_type = self.context.i16_type();
                let result = i16_type.const_int(*num as u64, true);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::Short), result.as_any_value_enum())))
            },
            ExprAST::Long(num, _pos) => {
                let i64_type = self.context.i64_type();
                let result = i64_type.const_int(*num as u64, true);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::Long), result.as_any_value_enum())))
            },
            ExprAST::LongLong(_num, _pos) => {
                let _i128_type = self.context.i128_type();
                // let result = i128_type.const_int(*num as u128, true);
                // let result = i128_type.const_int_arbitrary_precision(words);
                // Ok(Some(CompiledValue::new(Type::Number(NumberType::LongLong), result.as_any_value_enum())))
                unimplemented!()  // long long
            },
            ExprAST::UChar(num, _pos) => {
                let i8_type = self.context.i8_type();
                let result = i8_type.const_int(*num as u64, false);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::UnsignedChar), result.as_any_value_enum())))
            },
            ExprAST::UInt(num, _pos) => {
                let i32_type = self.context.i32_type();
                let result = i32_type.const_int(*num as u64, false);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::UnsignedInt), result.as_any_value_enum())))
            },
            ExprAST::UShort(num, _pos) => {
                let i16_type = self.context.i16_type();
                let result = i16_type.const_int(*num as u64, false);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::UnsignedShort), result.as_any_value_enum())))
            },
            ExprAST::ULong(num, _pos) => {
                let i64_type = self.context.i64_type();
                let result = i64_type.const_int(*num as u64, false);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::UnsignedLong), result.as_any_value_enum())))
            },
            ExprAST::ULongLong(_num, _pos) => {
                let _i128_type = self.context.i128_type();
                // let result = i128_type.const_int(*num as u128, false);
                // let result = i128_type.const_int_arbitrary_precision(words);
                // Ok(Some(CompiledValue::new(Type::Number(NumberType::LongLong), result.as_any_value_enum())))
                unimplemented!()  // long long
            },
            ExprAST::Float(num, _pos) => {
                let f32_type = self.context.f32_type();
                let result = f32_type.const_float(*num as f64);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::Double), result.as_any_value_enum())))
            },
            ExprAST::Double(num, _pos) => {
                let f64_type = self.context.f64_type();
                let result = f64_type.const_float(*num);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::Double), result.as_any_value_enum())))
            },
            ExprAST::StringLiteral(s, _pos) => {
                // let result = self.context.const_string(s.as_bytes(), false);
                let result = self.builder.build_global_string_ptr(s, &format!("global_str_{}", s))?;
                let pointer = Pointer::new(false, false);
                Ok(Some(CompiledValue::new(Type::Pointer(pointer, Box::new(Type::Number(NumberType::Char))), result.as_any_value_enum())))
            },
            ExprAST::PreInc(name, sym_pos, pos) => {
                if let Some((typ, ptr)) = env.get_ptr(name) {
                    let basic_val = self.builder.build_load(ptr, name)?;
                    let any_val = basic_val.as_any_value_enum();
                    let one = TypeUtil::to_llvm_int_type(typ, self.context, pos)?.const_int(1, false);
                    let added = self.builder.build_int_add(any_val.into_int_value(), one, "pre_increment")?;
                    let (_ptr_type, ptr) = env.get_ptr(&name).ok_or(Box::new(CodeGenError::no_such_a_variable(&name, sym_pos.clone())))?;
                    let _result = self.builder.build_store(ptr, added);

                    Ok(Some(CompiledValue::new(typ.clone(), added.as_any_value_enum())))
    
                }else{
                    Err(Box::new(CodeGenError::no_such_a_variable(name, sym_pos.clone())))
                }
            },
            ExprAST::PreDec(name, sym_pos, pos) => {
                if let Some((typ, ptr)) = env.get_ptr(name) {
                    let basic_val = self.builder.build_load(ptr, name)?;
                    let any_val = basic_val.as_any_value_enum();
                    let one = TypeUtil::to_llvm_int_type(typ, self.context, pos)?.const_int(1, false);
                    let subed = self.builder.build_int_sub(any_val.into_int_value(), one, "pre_decrement")?;
                    let (_ptr_type, ptr) = env.get_ptr(&name).ok_or(Box::new(CodeGenError::no_such_a_variable(&name, sym_pos.clone())))?;
                    let _result = self.builder.build_store(ptr, subed);

                    Ok(Some(CompiledValue::new(typ.clone(), subed.as_any_value_enum())))
    
                }else{
                    Err(Box::new(CodeGenError::no_such_a_variable(name, sym_pos.clone())))
                }
            },
            ExprAST::PostInc(name, sym_pos, pos) => {
                if let Some((typ, ptr)) = env.get_ptr(name) {
                    let basic_val = self.builder.build_load(ptr, name)?;
                    let pre_val = basic_val.as_any_value_enum();
                    let one = TypeUtil::to_llvm_int_type(typ, self.context, pos)?.const_int(1, false);
                    let added = self.builder.build_int_add(pre_val.into_int_value(), one, "post_increment")?;
                    let (_ptr_type, ptr) = env.get_ptr(&name).ok_or(Box::new(CodeGenError::no_such_a_variable(&name, sym_pos.clone())))?;
                    let _result = self.builder.build_store(ptr, added);

                    Ok(Some(CompiledValue::new(typ.clone(), pre_val)))
    
                }else{
                    Err(Box::new(CodeGenError::no_such_a_variable(name, sym_pos.clone())))
                }
            },
            ExprAST::PostDec(name, sym_pos, pos) => {
                if let Some((typ, ptr)) = env.get_ptr(name) {
                    let basic_val = self.builder.build_load(ptr, name)?;
                    let pre_val = basic_val.as_any_value_enum();
                    let one = TypeUtil::to_llvm_int_type(typ, self.context, pos)?.const_int(1, false);
                    let subed = self.builder.build_int_sub(pre_val.into_int_value(), one, "post_decrement")?;
                    let (_ptr_type, ptr) = env.get_ptr(&name).ok_or(Box::new(CodeGenError::no_such_a_variable(&name, sym_pos.clone())))?;
                    let _result = self.builder.build_store(ptr, subed);

                    Ok(Some(CompiledValue::new(typ.clone(), pre_val)))
    
                }else{
                    Err(Box::new(CodeGenError::no_such_a_variable(name, sym_pos.clone())))
                }
            },
            ExprAST::PreIncMemberAccess(boxed_ast, pos) => {
                let (typ, ptr) = self.get_l_value(&**boxed_ast, env, break_catcher, continue_catcher)?;
                let name = match &**boxed_ast {
                    ExprAST::MemberAccess(_, field_name, _pos) => field_name,
                    ExprAST::PointerAccess(_, field_name, _pos) => field_name,
                    _ => "member_access",
                };
                let basic_val = self.builder.build_load(ptr, name)?;
                let any_val = basic_val.as_any_value_enum();
                let one = TypeUtil::to_llvm_int_type(&typ, self.context, pos)?.const_int(1, false);
                let added = self.builder.build_int_add(any_val.into_int_value(), one, "pre_increment_member")?;
                let _result = self.builder.build_store(ptr, added);

                Ok(Some(CompiledValue::new(typ.clone(), added.as_any_value_enum())))
            },
            ExprAST::PreDecMemberAccess(boxed_ast, pos) => {
                let (typ, ptr) = self.get_l_value(&**boxed_ast, env, break_catcher, continue_catcher)?;
                let name = match &**boxed_ast {
                    ExprAST::MemberAccess(_, field_name, _pos) => field_name,
                    ExprAST::PointerAccess(_, field_name, _pos) => field_name,
                    _ => "member_access",
                };
                let basic_val = self.builder.build_load(ptr, name)?;
                let any_val = basic_val.as_any_value_enum();
                let one = TypeUtil::to_llvm_int_type(&typ, self.context, pos)?.const_int(1, false);
                let subed = self.builder.build_int_sub(any_val.into_int_value(), one, "pre_decrement_member")?;
                let _result = self.builder.build_store(ptr, subed);

                Ok(Some(CompiledValue::new(typ.clone(), subed.as_any_value_enum())))
            },
            ExprAST::PostIncMemberAccess(boxed_ast, pos) => {
                let (typ, ptr) = self.get_l_value(&**boxed_ast, env, break_catcher, continue_catcher)?;
                let name = match &**boxed_ast {
                    ExprAST::MemberAccess(_, field_name, _pos) => field_name,
                    ExprAST::PointerAccess(_, field_name, _pos) => field_name,
                    _ => "member_access",
                };
                let basic_val = self.builder.build_load(ptr, name)?;
                let pre_val = basic_val.as_any_value_enum();
                let one = TypeUtil::to_llvm_int_type(&typ, self.context, pos)?.const_int(1, false);
                let added = self.builder.build_int_add(pre_val.into_int_value(), one, "post_increment_member")?;
                let _result = self.builder.build_store(ptr, added);

                Ok(Some(CompiledValue::new(typ.clone(), pre_val.as_any_value_enum())))
            },
            ExprAST::PostDecMemberAccess(boxed_ast, pos) => {
                let (typ, ptr) = self.get_l_value(&**boxed_ast, env, break_catcher, continue_catcher)?;
                let name = match &**boxed_ast {
                    ExprAST::MemberAccess(_, field_name, _pos) => field_name,
                    ExprAST::PointerAccess(_, field_name, _pos) => field_name,
                    _ => "member_access",
                };
                let basic_val = self.builder.build_load(ptr, name)?;
                let pre_val = basic_val.as_any_value_enum();
                let one = TypeUtil::to_llvm_int_type(&typ, self.context, pos)?.const_int(1, false);
                let subed = self.builder.build_int_sub(pre_val.into_int_value(), one, "post_decrement_member")?;
                let _result = self.builder.build_store(ptr, subed);

                Ok(Some(CompiledValue::new(typ.clone(), pre_val.as_any_value_enum())))
            },
            ExprAST::UnaryMinus(boxed_ast, pos) => {
                let code = self.gen_expr(&*boxed_ast, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(pos.clone()))?;

                let result = self.builder.build_int_neg(code.get_value().into_int_value(), "neg")?;
                Ok(Some(CompiledValue::new(code.get_type().clone(), result.as_any_value_enum())))
            },
            ExprAST::Not(boxed_ast, pos) => {
                let value = self.gen_expr(&*boxed_ast, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(pos.clone()))?;
                let code = value.get_value().into_int_value();
                let zero = self.context.i32_type().const_int(0, false);

                let result = self.builder.build_int_compare(IntPredicate::EQ, code, zero, "Not")?;
                Ok(Some(CompiledValue::new(value.get_type().clone(), result.as_any_value_enum())))
            },
            ExprAST::UnaryTilda(boxed_ast, pos) => {
                let value = self.gen_expr(&*boxed_ast, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(pos.clone()))?;
                let code = value.get_value().into_int_value();
                let all_ones = self.context.i32_type().const_all_ones();

                let result = self.builder.build_xor(code, all_ones, "bit_reversal")?;
                Ok(Some(CompiledValue::new(value.get_type().clone(), result.as_any_value_enum())))
            },
            ExprAST::BinExpr(op, left, right, _pos) => self.gen_bin_expr(op, &**left, &**right, env, break_catcher, continue_catcher),
            ExprAST::DefVar{specifiers, declarations, pos: _} => {
                self.gen_def_var(specifiers, declarations, env, break_catcher, continue_catcher)?;
                Ok(None)
            },
            ExprAST::UnaryGetAddress(boxed_ast, pos) => {  // &var
                let ast = &**boxed_ast;
                let (typ, ptr) = self.get_l_value(ast, env, break_catcher, continue_catcher)?;
                let ptr = PointerValue::try_from(ptr).ok().ok_or(CodeGenError::cannot_get_pointer(pos.clone()))?;
                let typ = Type::new_pointer_type(typ.clone(), false, false);

                Ok(Some(CompiledValue::new(typ, ptr.into())))
            },
            ExprAST::UnaryPointerAccess(boxed_ast, pos) => {  // *pointer
                let ast = &**boxed_ast;
                let ptr = self.gen_expr(&ast, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::not_pointer(&TypeUtil::get_type(&ast, env)?, pos.clone()))?;
                let typ = ptr.get_type();

                let basic_val = self.builder.build_load(ptr.get_value().into_pointer_value(), &format!("get_value_from_pointer"))?;
                let any_val = basic_val.as_any_value_enum();
                let type2 = typ.peel_off_pointer().ok_or(CodeGenError::not_pointer(&typ, pos.clone()))?;

                Ok(Some(CompiledValue::new(type2, any_val)))
            },
            ExprAST::Assign(l_value, r_value, _pos) => self.gen_assign(&**l_value, &**r_value, env, break_catcher, continue_catcher),
            ExprAST::OpAssign(op, l_value, r_value, _pos) => self.gen_op_assign(op, &**l_value, &**r_value, env, break_catcher, continue_catcher),
            ExprAST::CallFunction(fun, args, pos) => {
                let mut v: Vec<BasicMetadataValueEnum> = Vec::new();
                for expr in args {
                    let result = self.gen_expr(expr, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(pos.clone()))?;
                    v.push(self.try_as_basic_metadata_value(&result.get_value(), pos)?);
                }

                match &**fun {
                    ExprAST::Symbol(name, _pos2) => {
                        self.gen_call_function(name, v, env, break_catcher, continue_catcher, pos)
                    },
                    ExprAST::MemberAccess(ast, fun_name, _pos2) => {
                        let typ = TypeUtil::get_type(ast, env)?;
                        let (_t, obj) = self.get_l_value(&**ast, env, break_catcher, continue_catcher)?;
                        let class_name = typ.get_type_name();
                        let method_name = Self::make_function_name_in_impl(&class_name, fun_name);
                        let mut args: Vec<BasicMetadataValueEnum> = Vec::new();
                        args.push(obj.into());

                        args.append(&mut v);

                        self.gen_call_function(&method_name, args, env, break_catcher, continue_catcher, pos)

                    },
                    _ => {
                        Err(Box::new(CodeGenError::not_function(&format!("{:?}", fun), pos.clone())))
                    }
                }
            },
            ExprAST::MemberAccess(_boxed_ast, field_name, _pos) => {
                let (typ, ptr) = self.get_l_value(expr_ast, env, break_catcher, continue_catcher)?;
                let basic_val = self.builder.build_load(ptr, &format!("access_to_field_{}", field_name))?;
                let any_val = basic_val.as_any_value_enum();
                Ok(Some(CompiledValue::new(typ.clone(), any_val)))
            },
            ExprAST::PointerAccess(_boxed_ast, field_name, _pos) => {
                let (typ, ptr) = self.get_l_value(expr_ast, env, break_catcher, continue_catcher)?;
                let basic_val = self.builder.build_load(ptr, &format!("pointer_access_to_field_{}", field_name))?;
                let any_val = basic_val.as_any_value_enum();
                Ok(Some(CompiledValue::new(typ.clone(), any_val)))
            },
            ExprAST::ArrayAccess(_boxed_ast, _index, _pos) => {
                let (typ, ptr) = self.get_l_value(expr_ast, env, break_catcher, continue_catcher)?;
                if let Type::Array { typ, .. } = typ {
                    let any_val = ptr.as_any_value_enum();
                    let t = Type::new_pointer_type(*typ.clone(), false, false);
                    Ok(Some(CompiledValue::new(t, any_val)))

                }else{
                    let basic_val = self.builder.build_load(ptr, "get_value_from_array")?;
                    let any_val = basic_val.as_any_value_enum();
                    Ok(Some(CompiledValue::new(typ, any_val)))
                }
            },
            ExprAST::Symbol(name, pos) => {
                if let Some((typ, ptr)) = env.get_ptr(name) {
                    // let basic_val = self.builder.build_load(ptr, name)?;
                    // let any_val = basic_val.as_any_value_enum();
                    // Ok(Some(CompiledValue::new(typ.clone(), any_val)))

                    if let Type::Array { typ, .. } = typ {
                        let any_val = ptr.as_any_value_enum();
                        let t = Type::new_pointer_type(*typ.clone(), false, false);
                        Ok(Some(CompiledValue::new(t, any_val)))
    
                    }else{
                        let basic_val = self.builder.build_load(ptr, name)?;
                        let any_val = basic_val.as_any_value_enum();
                        Ok(Some(CompiledValue::new(typ.clone(), any_val)))
                    }

                }else if let Some((typ, val)) = env.get_value(name) {
                    if let Type::Array { typ, .. } = typ {
                        let any_val = val.as_any_value_enum();
                        let t = Type::new_pointer_type(*typ.clone(), false, false);
                        Ok(Some(CompiledValue::new(t, any_val)))
    
                    }else{
                        Ok(Some(CompiledValue::new(typ.clone(), val.as_any_value_enum())))
                    }
    
                }else{
                    Err(Box::new(CodeGenError::no_such_a_variable(name, pos.clone())))
                }
            },
            ExprAST::_self(pos) => {
                if let Some((typ, ptr)) = env.get_self_ptr() {
                    let basic_val = self.builder.build_load(ptr, "get_self")?;
                    let any_val = basic_val.as_any_value_enum();

                    Ok(Some(CompiledValue::new(typ.clone(), any_val)))
                }else{
                    Err(Box::new(CodeGenError::no_such_a_variable("self", pos.clone())))
                }
            },
            ExprAST::UnarySizeOfExpr(expr, pos) => {
                let typ = TypeUtil::get_type(&**expr, env)?;
                let llvm_type = TypeUtil::to_llvm_any_type(&typ, self.context, pos)?;
                let size = llvm_type.size_of().ok_or(CodeGenError::cannot_get_size_of(typ.clone(), pos.clone()))?;

                Ok(Some(CompiledValue::new(Type::Number(NumberType::Int), size.as_any_value_enum())))
            },
            ExprAST::UnarySizeOfTypeName(typ, pos) => {
                let llvm_type = TypeUtil::to_llvm_any_type(typ, self.context, pos)?;
                let size = llvm_type.size_of().ok_or(CodeGenError::cannot_get_size_of(typ.clone(), pos.clone()))?;

                Ok(Some(CompiledValue::new(Type::Number(NumberType::Int), size.as_any_value_enum())))
            },
            ExprAST::TernaryOperator(condition, then, _else, pos) => {
                let (_fun_type, func) = env.get_current_function().ok_or(CodeGenError::no_current_function(pos.clone()))?;
                let func = func.clone();
                let cond_block = self.context.append_basic_block(func, "ternary.cond");
                let then_block = self.context.append_basic_block(func, "ternary.then");
                let else_block = self.context.append_basic_block(func, "ternary.else");
                let end_block  = self.context.append_basic_block(func, "ternary.end");

                self.builder.build_unconditional_branch(cond_block)?;
                self.builder.position_at_end(cond_block);

                // check condition
                let cond = self.gen_expr(condition, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::condition_is_not_number(condition, condition.get_position().clone()))?;
                let mut comparison = cond.get_value().into_int_value();
                let i1_type = self.context.bool_type();
                comparison = self.builder.build_int_cast(comparison, i1_type, "cast to i1")?;  // cast to i1
                self.builder.build_conditional_branch(comparison, then_block, else_block)?;

                // then block
                self.builder.position_at_end(then_block);
                let then_result = self.gen_expr(then, env, break_catcher, continue_catcher)?.unwrap();
                if let Some(blk) = self.builder.get_insert_block() {
                    if ! self.last_is_jump_statement(blk) {
                        self.builder.position_at_end(blk);
                        self.builder.build_unconditional_branch(end_block)?;
                    }
                }
                if ! self.last_is_jump_statement(then_block) {
                    self.builder.position_at_end(then_block);
                    self.builder.build_unconditional_branch(end_block)?;
                }

                // else block
                self.builder.position_at_end(else_block);
                let else_result = self.gen_expr(_else, env, break_catcher, continue_catcher)?.unwrap();
                if let Some(blk) = self.builder.get_insert_block() {
                    if ! self.last_is_jump_statement(blk) {
                        self.builder.position_at_end(blk);
                        self.builder.build_unconditional_branch(end_block)?;
                    }
                }
                if ! self.last_is_jump_statement(else_block) {
                    self.builder.position_at_end(else_block);
                    self.builder.build_unconditional_branch(end_block)?;
                }

                // end block
                self.builder.position_at_end(end_block);

                let typ = then_result.get_type();
                let llvm_type = TypeUtil::to_basic_type_enum(typ, self.context, then.get_position())?;
                let phi_value = self.builder.build_phi(llvm_type, "ternary.phi")?;
                let then_value = if let Ok(val) = BasicValueEnum::try_from(then_result.get_value()) {
                    val
                }else{
                    return Err(Box::new(CodeGenError::cannot_convert_to_basic_value((**then).clone(), then.get_position().clone())));
                };
                let else_value = if let Ok(val) = BasicValueEnum::try_from(else_result.get_value()) {
                    val
                }else{
                    return Err(Box::new(CodeGenError::cannot_convert_to_basic_value((**_else).clone(), _else.get_position().clone())));
                };
                let incoming: [(&dyn BasicValue<'ctx>, BasicBlock<'ctx>); 2] = [(&then_value, then_block), (&else_value, else_block)];
                phi_value.add_incoming(&incoming);

                Ok(Some(CompiledValue::new(typ.clone(), phi_value.as_any_value_enum())))
            },
            ExprAST::Cast(to_type, expr, _pos) => {
                let from = TypeUtil::get_type(&**expr, env)?;
                let value = self.gen_expr(&**expr, env, break_catcher, continue_catcher)?.unwrap().get_value();
                let result = Caster::gen_cast(&self.builder, self.context, &value, &from, to_type, &**expr)?;
                Ok(Some(CompiledValue::new(to_type.clone(), result)))
            },
            ExprAST::_Self(_) => {
                // never reached, maybe
                unimplemented!()  // _Self
            },
        }
    }

    pub fn gen_const_expr<'b, 'c>(&self,
        init: &Initializer,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {

        match init {
            Initializer::Simple(expr, _pos) => self.gen_expr(expr, env, break_catcher, continue_catcher),
            Initializer::Array(_list, _typ, _pos) => {






                unimplemented!()
            },
            Initializer::Struct(_list, _typ, _pos) => {






                unimplemented!()
            }
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
            let basic_type = env.basic_type_enum_from_type(&typ, self.context, declarator.get_position())?;
            let ptr = self.builder.build_alloca(basic_type, name)?;
            let init_expr = decl.get_init_expr();

            match init_expr {
                Some(const_expr) => {
                    match &typ {
                        Type::Struct { fields, .. } => {
                            self.gen_struct_init(&fields, ptr, &*const_expr, env, break_catcher, continue_catcher)?;
                        },
                        Type::Array { name: _, typ, size_list } => {
                            self.gen_array_init(&size_list, ptr, typ, &*const_expr, env, break_catcher, continue_catcher)?;
                        },
                        _ => {
                            let compiled_value = self.gen_const_expr(&*const_expr, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(const_expr.get_position().clone()))?;
                            let mut init_value = compiled_value.get_value();
                            let init_type = compiled_value.get_type();

                            if typ != *init_type {
                                init_value = self.gen_implicit_cast(&init_value, &init_type, &typ, (*const_expr).get_position())?;
                            }

                            let basic_value = self.try_as_basic_value(&init_value, (*const_expr).get_position())?;
                            self.builder.build_store(ptr, basic_value)?;
                        }
                    }
                },
                None => (),  // do nothing
            };

            env.insert_local(name, typ.clone(), ptr);
        }

        Ok(())
    }

    pub fn make_struct_init_value<'b, 'c>(&self,
        target_fields: &StructDefinition,
        init_pos: &Position,
        init_value_list: &Vec<Box<Initializer>>,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<StructValue<'ctx>, Box<dyn Error>> {

        let target_len = target_fields.len();
        let init_len = init_value_list.len();
        if target_len < init_len {
            return  Err(Box::new(CodeGenError::initial_list_is_too_long(init_pos.clone())));
        }

        let mut vec = Vec::new();
        let fields = target_fields.get_fields().unwrap();
        for i in 0..target_len {
            let field = &fields[i];
            let field_type = field.get_type().unwrap();

            if init_len > i {
                let init_value = &init_value_list[i];
                let init_type = TypeUtil::get_initializer_type(init_value, env)?;
                if *field_type != init_type {
                    return Err(Box::new(CodeGenError::mismatch_initializer_type(init_value.get_position().clone())));
                }

                let compiled_val = self.gen_const_expr(init_value, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::mismatch_initializer_type(init_value.get_position().clone()))?;
                let value = self.try_as_basic_value(&compiled_val.get_value(), init_value.get_position())?;
                vec.push(value);

            }else{  // zero clear
                let zero_value = self.const_zero(field_type, init_pos)?;
                vec.push(zero_value);
            }
        }

        let values = self.context.const_struct(&vec, false);
        Ok(values)
    }

    pub fn gen_global_struct_init<'b, 'c>(&self,
        target_fields: &StructDefinition,
        target_struct_ptr: GlobalValue<'ctx>,
        init: &Initializer,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let init_value_list = if let Initializer::Struct(list, _typ, _pos) = init {
            list
        }else{
            return Err(Box::new(CodeGenError::mismatch_initializer_type(init.get_position().clone())));
        };

        let target_len = target_fields.len();
        let init_len = init_value_list.len();
        if target_len < init_len {
            return  Err(Box::new(CodeGenError::initial_list_is_too_long(init.get_position().clone())));
        }

        if target_len == 0 {
            return Ok(None);
        }

        //
        // target_len > 0
        //

        let values = self.make_struct_init_value(target_fields, init.get_position(), init_value_list, env, break_catcher, continue_catcher)?;
        target_struct_ptr.set_initializer(&values.as_basic_value_enum());

        Ok(None)
    }

    pub fn gen_struct_init<'b, 'c>(&self,
        target_fields: &StructDefinition,
        target_struct_ptr: PointerValue<'ctx>,
        init: &Initializer,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let init_value_list = if let Initializer::Struct(list, _typ, _pos) = init {
            list
        }else{
            return Err(Box::new(CodeGenError::mismatch_initializer_type(init.get_position().clone())));
        };

        let target_len = target_fields.len();
        let init_len = init_value_list.len();
        if target_len < init_len {
            return  Err(Box::new(CodeGenError::initial_list_is_too_long(init.get_position().clone())));
        }

        if target_len == 0 {
            return Ok(None);
        }

        //
        // target_len > 0
        //

        let values = self.make_struct_init_value(target_fields, init.get_position(), init_value_list, env, break_catcher, continue_catcher)?;
        let _result = self.builder.build_store(target_struct_ptr, values.as_basic_value_enum());

        Ok(None)
    }

    pub fn make_array_init_value<'b, 'c>(&self,
        _size_list: &Vec<usize>,
        target_type: &Type,
        init_value_list: &Vec<Box<Initializer>>,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<StructValue<'ctx>, Box<dyn Error>> {

        let init_len = init_value_list.len();

        //
        // init_len > 0
        //
        let mut vec = Vec::new();
        for i in 0..init_len {
            let init_value = &init_value_list[i];
            let init_type = TypeUtil::get_initializer_type(init_value, env)?;
            if *target_type != init_type {
                return Err(Box::new(CodeGenError::mismatch_initializer_type(init_value.get_position().clone())));
            }

            let any_val = self.gen_initializer(init_value, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::mismatch_initializer_type(init_value.get_position().clone()))?;
            let value = self.try_as_basic_value(&any_val, init_value.get_position())?;

            vec.push(value);
        }

        let values = self.context.const_struct(&vec, false);
        Ok(values)
    }

    pub fn gen_global_array_init<'b, 'c>(&self,
        size_list: &Vec<usize>,
        target_array_ptr: GlobalValue<'ctx>,
        target_type: &Type,
        init: &Initializer,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let init_value_list = if let Initializer::Array(list, _typ, _pos) = init {
            list
        }else{
            return Err(Box::new(CodeGenError::mismatch_initializer_type(init.get_position().clone())));
        };

        let init_len = init_value_list.len();
        if init_len == 0 {
            return Ok(None);
        }

        let values = self.make_array_init_value(size_list, target_type, init_value_list, env, break_catcher, continue_catcher)?;
        target_array_ptr.set_initializer(&values.as_basic_value_enum());

        Ok(None)
    }

    pub fn gen_array_init<'b, 'c>(&self,
        size_list: &Vec<usize>,
        target_array_ptr: PointerValue<'ctx>,
        target_type: &Type,
        init: &Initializer,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let init_value_list = if let Initializer::Array(list, _typ, _pos) = init {
            list
        }else{
            return Err(Box::new(CodeGenError::mismatch_initializer_type(init.get_position().clone())));
        };

        let init_len = init_value_list.len();
        if init_len == 0 {
            return Ok(None);
        }

        let values = self.make_array_init_value(size_list, target_type, init_value_list, env, break_catcher, continue_catcher)?;
        let _result = self.builder.build_store(target_array_ptr, values.as_basic_value_enum());

        Ok(None)
    }

    pub fn gen_initializer<'b, 'c>(&self,
        init: &Initializer,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {
        match init {
            Initializer::Simple(expr, _pos) => {
                if let Some(v) = self.gen_expr(expr, env, break_catcher, continue_catcher)? {
                    Ok(Some(v.get_value()))
                }else{
                    Ok(None)
                }
            },
            Initializer::Array(vec_init, _typ, _pos) => {
                let mut list = Vec::new();
                for init_value in vec_init {
                    let compiled_val = self.gen_initializer(init_value, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::mismatch_initializer_type(init_value.get_position().clone()))?;
                    let value = self.try_as_basic_value(&compiled_val, init_value.get_position())?;

                    list.push(value);
                }

                let values = self.context.const_struct(&list, false);
                let any_val = values.as_any_value_enum();
                Ok(Some(any_val))
            },
            Initializer::Struct(vec_init, _typ, _pos) => {
                let mut list = Vec::new();
                for init_value in vec_init {
                    let compiled_val = self.gen_initializer(init_value, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::mismatch_initializer_type(init_value.get_position().clone()))?;
                    let value = self.try_as_basic_value(&compiled_val, init_value.get_position())?;

                    list.push(value);
                }

                let values = self.context.const_struct(&list, false);
                let any_val = values.as_any_value_enum();
                Ok(Some(any_val))
            }
        }
    }

    fn const_zero(&self, typ: &Type, pos: &Position) -> Result<BasicValueEnum, Box<dyn Error>> {
        let t = TypeUtil::to_llvm_type(typ, self.context, pos)?;
        match t {
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
            BasicMetadataTypeEnum::ArrayType(_) => {
                unimplemented!()
            },
            _ => {
                Err(Box::new(CodeGenError::cannot_get_zero_value(pos.clone())))
            },
        }
    }

    pub fn gen_call_function<'b, 'c>(&self,
        name: &str,
        args: Vec<BasicMetadataValueEnum<'ctx>>,
        env: &mut Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        _continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
   ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {
        if let Some((fn_typ, function)) = env.get_function(name) {
            let call_site_value = self.builder.build_call(*function, &args, &format!("call_function_{}", name))?;

            let tried = call_site_value.try_as_basic_value();
            if tried.is_left() {  // BasicValueEnum
                let any_val = tried.left().unwrap().as_any_value_enum();
                Ok(Some(CompiledValue::new(fn_typ.get_return_type().clone(), any_val)))

            }else{                 // InstructionValue
                Ok(Some(CompiledValue::new(fn_typ.get_return_type().clone(), AnyValueEnum::InstructionValue(tried.right().unwrap()))))
            }

        }else{
            return Err(Box::new(CodeGenError::no_such_a_function(&name, pos.clone())));
        }
    }

    fn str_to_basic_metadata_value_enum(&self, s: &str, pos: &Position) -> Result<BasicMetadataValueEnum<'ctx>, Box<dyn Error>> {
        let global_str = self.builder.build_global_string_ptr(s, &format!("global_str_{}", s))?.as_any_value_enum();
        Ok(self.try_as_basic_metadata_value(&global_str, pos)?)
    }

    pub fn gen_switch<'b, 'c>(&self,
        switch: &'ctx Switch,
        env: &mut Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let (_fun_type, func) = env.get_current_function().ok_or(CodeGenError::no_current_function(pos.clone()))?;
        let func = func.clone();
        let cond_block  = self.context.append_basic_block(func, "switch.cond");
        let end_block  = self.context.append_basic_block(func, "switch.end");
        let break_catcher = BreakCatcher::new(&end_block);
        let break_catcher = Some(&break_catcher);

        //
        // condition expr
        //
        self.builder.build_unconditional_branch(cond_block)?;
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
                self.builder.build_unconditional_branch(next_block)?;
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
                    self.builder.build_unconditional_branch(next_block)?;
                }
            }

            //
            // conditional jump
            //
            self.builder.position_at_end(current_block);

            if case.is_case() {  // case
                if opt_default.is_some() {
                    return Err(Box::new(CodeGenError::case_after_default(case.get_position().clone())));
                }

                let case_cond = case.get_cond().unwrap();
                let value = case_cond.as_i32_value();
                let i32_type = self.context.i32_type();
                let case_value = i32_type.const_int(value as u64, true);
                let real_value = self.try_as_basic_value(&cond_expr.get_value(), case_cond.get_position())?.into_int_value();
                let comparison = self.builder.build_int_compare(IntPredicate::EQ, real_value, case_value, "compare_switch_case")?;



                if i < len - 1 {
                    current_block = self.context.append_basic_block(func.clone(), &format!("cond_block_{}", i + 1));
                }else{
                    current_block = end_block;
                }
                self.builder.build_conditional_branch(comparison, *case.get_block(), current_block)?;

            }else{               // default
                if opt_default.is_some() {
                    return Err(Box::new(CodeGenError::already_default_defined(case.get_position().clone())));
                }
                opt_default = Some(case);

                self.builder.build_unconditional_branch(*case.get_block())?;
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

        let (_fun_type, func) = env.get_current_function().ok_or(CodeGenError::no_current_function(case.get_position().clone()))?;
        let case_block  = self.context.append_basic_block(func.clone(), "switch.case");

        self.builder.position_at_end(case_block);

        let const_cond = case.get_cond();
        let ast = case.get_stmt();
        let code = self.gen_stmt(ast, env, break_catcher, continue_catcher)?;
        let insert_block= self.builder.get_insert_block();

        env.insert_case(const_cond, case_block, code, insert_block, case.get_position().clone());

        Ok(None)
    }

    pub fn gen_default<'b, 'c>(&self,
        stmt: &'ctx AST,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let (_fun_type, func) = env.get_current_function().ok_or(CodeGenError::no_current_function(pos.clone()))?;
        let default_block  = self.context.append_basic_block(func.clone(), "switch.default");

        self.builder.position_at_end(default_block);
        let code = self.gen_stmt(stmt, env, break_catcher, continue_catcher)?;
        let insert_block= self.builder.get_insert_block();

        env.insert_default(default_block, code, insert_block, pos.clone());

        Ok(None)
    }

    fn gen_implicit_cast(&self, value: &AnyValueEnum<'ctx>, from_type: &Type, to_type: &Type, pos: &Position) -> Result<AnyValueEnum<'ctx>, Box<dyn Error>> {
        Caster::gen_implicit_cast(&self.builder, &self.context, value, from_type, to_type, pos)
    }

    #[cfg(test)]
    fn debug_print<'b, 'c>(&self,
        env: &mut Env<'ctx>,
        fmt: &str,
        args: &[BasicMetadataValueEnum<'ctx>] ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>>
    {
        let mut v = Vec::new();
        let dummy_pos = Position::new(1, 1);

        let fmt_string = self.builder.build_global_string_ptr(fmt, &format!("global_str_{}", fmt))?.as_any_value_enum();
        v.push(self.try_as_basic_metadata_value(&fmt_string, &dummy_pos)?);
        for item in args {
            v.push(*item);
        }

        let call_site_value = if let Some((_typ, function)) = env.get_function("printf") {
            self.builder.build_call(*function, &v, &format!("call_function_printf_in_debug_print"))?
        }else{
            let name = "printf";
            let ret_type = Type::Number(NumberType::Int);

            let sq = SpecifierQualifier::new();
            let ds = DeclarationSpecifier { typ: Type::Number(NumberType::Char), specifier_qualifier: sq };

            let pointer = Pointer::new(false, false);
            let dd = DirectDeclarator::Symbol("format".to_string(), dummy_pos.clone());
            let decl = Declarator::new(Some(pointer), dd);

            let mut defs = Defines::new();
            let param = Param::new(ds, decl, &mut defs, &dummy_pos)?;
            let param_list = vec![param];
            let params = Params::new(param_list, true);
            let fun_proto = self.gen_code_function_prototype(name, &ret_type, &params, env, &dummy_pos)?;
            let fun_type = Self::make_fun_type(&name, &ret_type, &params, env)?;

            env.insert_function(&name, fun_type, fun_proto);

            let (_typ, function) = env.get_function("printf").unwrap();
            self.builder.build_call(*function, &v, &format!("call_function_printf_in_debug_print"))?
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
        continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<(), Box<dyn Error>> {
        if for_type.is_some() {
            self.gen_impl_for(class_name, typ, for_type, functions, env, break_catcher, continue_catcher)
        }else{
            self.gen_impl_no_for(class_name, typ, functions, env, break_catcher, continue_catcher, pos)
        }
    }

    fn gen_impl_no_for<'b, 'c>(
        &self,
        class_name: &str,
        typ: &Type,
        functions: &'ctx Vec<FunOrProto>,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<(), Box<dyn Error>> {

        let _class = env.get_type(class_name).ok_or(Box::new(CodeGenError::no_such_a_struct(class_name, pos.clone())))?;

        for function in functions {
            self.gen_impl_function(class_name, typ, function, env, break_catcher, continue_catcher, pos)?;
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
        continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let declarator = fun_or_proto.get_declarator();
        let specifiers = fun_or_proto.get_specifiers();
        let sp_type = specifiers.get_type();
        let ret_type = declarator.make_type(&sp_type);
        let params = fun_or_proto.get_params();
        let decl_name = declarator.get_name();
        let fun_name = Self::make_function_name_in_impl(class_name, decl_name);
        let body = fun_or_proto.get_body();
        let function = if let Some(b) = body {
            // Function
            let labels = fun_or_proto.get_labels().unwrap();
            self.gen_code_function(&fun_name, &ret_type, params, b, labels, env, break_catcher, continue_catcher, pos)?
        }else{
            // Function Prototype
            self.gen_code_function_prototype(&fun_name, &ret_type, params, env, pos)?
        };

        let fun_type = Self::make_fun_type(&fun_name, &ret_type, params, env)?;
        env.insert_function(&fun_name, fun_type, function);

        Ok(Some(AnyValueEnum::FunctionValue(function)))
    }

    pub fn make_function_name_in_impl(class_name: &str, fun_name: &str) -> String {
        format!("${}${}", class_name, fun_name)
    }

    pub fn gen_define_struct<'b, 'c>(
        &self,
        name: &Option<String>,
        fields: &StructDefinition,
        env: &mut Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        _continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<Option<AnyTypeEnum<'ctx>>, Box<dyn Error>> {

        let (struct_type, index_map) = Self::struct_from_struct_definition(name, fields, self.context, pos)?;
        if let Some(id) = name {
            env.insert_struct(id, &struct_type, index_map, pos)?;
        }

        Ok(Some(struct_type.as_any_type_enum()))
    }

    pub fn struct_from_struct_definition(name: &Option<String>, struct_def: &StructDefinition, ctx: &'ctx Context, pos: &Position) -> Result<(StructType<'ctx>, HashMap<String, usize>), Box<dyn Error>> {
        let mut list: Vec<BasicTypeEnum<'ctx>> = Vec::new();
        let mut packed = false;
        let mut index_map: HashMap<String, usize> = HashMap::new();
        let mut index = 0;

        if let Some(fields) = struct_def.get_fields() {
            for field in fields {
                match field {
                    StructField::NormalField { name: _, sq: _, typ } => {
                        let t = TypeUtil::to_basic_type_enum(typ, ctx, pos)?;
                        list.push(t);

                        if let Some(id) = name {
                            index_map.insert(id.clone(), index);
                        }
                    },
                    StructField::BitField { name, sq: _, typ, bit_size } => {
                        packed = true;

                        Self::bit_size_check(typ, *bit_size, pos)?;
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
        _continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<Option<AnyTypeEnum<'ctx>>, Box<dyn Error>> {

        let (type_list, index_map, max_size, max_size_type) = Self::union_from_struct_definition(name, fields, self.context, pos)?;
        if let Some(id) = name {
            env.insert_union(id, type_list, index_map, max_size, max_size_type, pos)?;
        }

        if let Some(t) = max_size_type {
            Ok(Some(t.as_any_type_enum()))
        }else{
            Ok(None)
        }
    }

    pub fn union_from_struct_definition(_name: &Option<String>, struct_def: &StructDefinition, ctx: &'ctx Context, pos: &Position)
      -> Result<(Vec<(Type, BasicTypeEnum<'ctx>)>, HashMap<String, usize>, u64, Option<BasicTypeEnum<'ctx>>), Box<dyn Error>>
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
                        let t = TypeUtil::to_basic_type_enum(typ, ctx, pos)?;
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
                        Self::bit_size_check(&typ, *bit_size, pos)?;
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
        _continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<(), Box<dyn Error>> {

        let (enumerator_list, index_map) = self.enum_from_enum_definition(enum_def, env)?;
        if let Some(id) = name {
            let i32_type = self.context.i32_type();
            env.insert_enum(id, &i32_type, enumerator_list, index_map, pos)?;
        }

        Ok(())
    }

    pub fn enum_from_enum_definition(&self, enum_def: &EnumDefinition, _env: &mut Env<'ctx>) -> Result<(Vec<(String, IntValue<'ctx>)>, HashMap<String, usize>), CodeGenError> {
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

    fn size_of(basic_type: &BasicTypeEnum) -> Result<u64, Box<dyn Error>> {
        if let Some(size) = basic_type.size_of() {
            if let Some(s) = size.get_zero_extended_constant() {
                return Ok(s);
            }
        }

        let size = Self::calc_type_size(basic_type)?;
        Ok(size)
     }

    fn calc_type_size(basic_type: &dyn BasicType) -> Result<u64, Box<dyn Error>> {
        use inkwell::execution_engine::JitFunction;
        type FuncVoidU64 = unsafe extern "C" fn() -> u64;

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
        let array_ptr = builder.build_alloca(array_type, "array")?;
        // let zero = i32_type.const_int(0, false);
        // let array_ptr = zero.const_to_pointer(array_ptr_type);
        let const_one = i32_type.const_int(1, false);
        let ptr = unsafe { builder.build_in_bounds_gep(array_ptr, &[const_one], "gep for array")? };
    
        let base = array_ptr.const_to_int(i32_type);
        let index1 = ptr.const_to_int(i32_type);
        let size = builder.build_int_sub(index1, base, "sub")?;
        let _tmp = builder.build_return(Some(&size));

        let f = unsafe { engine.get_function(fn_name) };
        let f: JitFunction<FuncVoidU64> = f.ok().unwrap();
        let result: u64 = unsafe { f.call() };

        Ok(result)
    }

    fn bit_size_check(opt_type: &Option<Type>, size: usize, pos: &Position) -> Result<(), CodeGenError> {
        if let Some(Type::Number(typ)) = opt_type {
            match typ {
                NumberType::_Bool => {
                    if size > 1 {
                        return Err(CodeGenError::illegal_bit_size(typ, size, pos.clone()));
                    }
                },
                NumberType::Char => {
                    if size > 8 {
                        return Err(CodeGenError::illegal_bit_size(typ, size, pos.clone()));
                    }
                },
                NumberType::Short => {
                    if size > 16 {
                        return Err(CodeGenError::illegal_bit_size(typ, size, pos.clone()));
                    }
                },
                NumberType::Int => {
                    if size > 32 {
                        return Err(CodeGenError::illegal_bit_size(typ, size, pos.clone()));
                    }
                },
                NumberType::Long => {
                    if size > 64 {
                        return Err(CodeGenError::illegal_bit_size(typ, size, pos.clone()));
                    }
                },
                NumberType::LongLong => {
                    if size > 128 {
                        return Err(CodeGenError::illegal_bit_size(typ, size, pos.clone()));
                    }
                },
                NumberType::UnsignedChar => {
                    if size > 8 {
                        return Err(CodeGenError::illegal_bit_size(typ, size, pos.clone()));
                    }
                },
                NumberType::UnsignedShort => {
                    if size > 16 {
                        return Err(CodeGenError::illegal_bit_size(typ, size, pos.clone()));
                    }
                },
                NumberType::UnsignedInt => {
                    if size > 32 {
                        return Err(CodeGenError::illegal_bit_size(typ, size, pos.clone()));
                    }
                },
                NumberType::UnsignedLong => {
                    if size > 64 {
                        return Err(CodeGenError::illegal_bit_size(typ, size, pos.clone()));
                    }
                },
                NumberType::UnsignedLongLong => {
                    if size > 128 {
                        return Err(CodeGenError::illegal_bit_size(typ, size, pos.clone()));
                    }
                },
                NumberType::Float | NumberType::Double => {
                    return Err(CodeGenError::cannot_use_float_for_bitsize(pos.clone()));
                }
            }
        }

        Ok(())
    }

    fn last_is_jump_statement(&self, block: BasicBlock) -> bool {
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
        _continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let (_fun_type, func) = env.get_current_function().ok_or(CodeGenError::no_current_function(pos.clone()))?;
        let func = func.clone();
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
        self.builder.build_unconditional_branch(pre_condition_block)?;
        self.builder.position_at_end(pre_condition_block);

        if let Some(cond) = pre_condition {
            let cond = self.gen_expr(cond, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::condition_is_not_number(cond, cond.get_position().clone()))?;
            let comparison = cond.get_value().into_int_value();
            self.builder.build_conditional_branch(comparison, start_block, end_block)?;
        }else{
            self.builder.build_unconditional_branch(start_block)?;
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
        self.builder.build_unconditional_branch(update_block)?;
        self.builder.position_at_end(update_block);
        if let Some(expr) = update_expr {
            self.gen_expr(expr, env, break_catcher, continue_catcher)?;
        }

        //
        // check post condtition
        //
        if let Some(cond) = post_condition {
            let cond = self.gen_expr(cond, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::condition_is_not_number(cond, cond.get_position().clone()))?;
            let comparison = cond.get_value().into_int_value();
            self.builder.build_conditional_branch(comparison, pre_condition_block, end_block)?;
        }else{
            self.builder.build_unconditional_branch(pre_condition_block)?;
        }

        // loop end
        self.builder.position_at_end(end_block);

        Ok(None)
    }

    fn bin_expr_implicit_cast(&self, left: CompiledValue<'ctx>, right: CompiledValue<'ctx>) -> Result<(CompiledValue<'ctx>, CompiledValue<'ctx>), Box<dyn Error>> {
        if let (Type::Number(left_type), Type::Number(right_type)) = (left.get_type(), right.get_type()) {
            if left_type == right_type {
                Ok((left, right))
            }else if left_type < right_type {
                unimplemented!()
            }else{  // left_type > right_type
//                 let value = self.gen_cast(&right.get_value(), &left_type.to_basic_type_enum(self.context)?)?;
//                 Ok((left, CompiledValue::new(Type::Number(right_type.clone()), value)))
                unimplemented!()
            }
        }else{
            Ok((left, right))
        }
    }

    fn gen_bin_expr<'b, 'c>(&self,
         op: &BinOp,
         left_arg: &ExprAST,
         right_arg: &ExprAST,
         env: &mut Env<'ctx>,
         break_catcher: Option<&'b BreakCatcher>,
         continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {
        match op {
            BinOp::Add => {
                let left = self.gen_expr(&left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    let result = self.builder.build_int_add(left_value.into_int_value(), right_value.into_int_value(), "add_int")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_add(left_value.into_float_value(), right_value.into_float_value(), "add_float")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CodeGenError::cannot_add_value(left_type, right.get_type(), left_arg.get_position().clone())))
                }
            },
            BinOp::Sub => {
                let left = self.gen_expr(&left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    let result = self.builder.build_int_sub(left_value.into_int_value(), right_value.into_int_value(), "sub_int")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_sub(left_value.into_float_value(), right_value.into_float_value(), "sub_float")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CodeGenError::cannot_sub_value(left_type, right.get_type(), left_arg.get_position().clone())))
                }
            },
            BinOp::Mul => {
                let left = self.gen_expr(&left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    let result = self.builder.build_int_mul(left_value.into_int_value(), right_value.into_int_value(), "mul_int")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_mul(left_value.into_float_value(), right_value.into_float_value(), "mul_float")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CodeGenError::cannot_mul_value(left_type, right.get_type(), left_arg.get_position().clone())))
                }
            },
            BinOp::Div => {
                let left = self.gen_expr(&left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    if left_type.is_signed()? {
                        let result = self.builder.build_int_signed_div(left_value.into_int_value(), right_value.into_int_value(), "div_int")?;
                        Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }else{
                        let result = self.builder.build_int_unsigned_div(left_value.into_int_value(), right_value.into_int_value(), "div_int")?;
                        Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_div(left_value.into_float_value(), right_value.into_float_value(), "div_float")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CodeGenError::cannot_div_value(left_type, right.get_type(), left_arg.get_position().clone())))
                }
            },
            BinOp::Mod => {
                let left = self.gen_expr(&left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    if left_type.is_signed()? {
                        let result = self.builder.build_int_signed_rem(left_value.into_int_value(), right_value.into_int_value(), "mod_int")?;
                        Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }else{
                        let result = self.builder.build_int_unsigned_rem(left_value.into_int_value(), right_value.into_int_value(), "mod_int")?;
                        Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_rem(left_value.into_float_value(), right_value.into_float_value(), "mod_float")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CodeGenError::cannot_mod_value(left_type, right.get_type(), left_arg.get_position().clone())))
                }
            },
            BinOp::Equal => {
                let left = self.gen_expr(&left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    let result = self.builder.build_int_compare(IntPredicate::EQ, left_value.into_int_value(), right_value.into_int_value(), "eq_int")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_compare(FloatPredicate::OEQ, left_value.into_float_value(), right_value.into_float_value(), "eq_float")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CodeGenError::cannot_compare_value(left_type, left_arg.get_position().clone())))
                }
            },
            BinOp::NotEqual => {
                let left = self.gen_expr(&left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    let result = self.builder.build_int_compare(IntPredicate::NE, left_value.into_int_value(), right_value.into_int_value(), "not_eq_int")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_compare(FloatPredicate::ONE, left_value.into_float_value(), right_value.into_float_value(), "not_eq_float")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CodeGenError::cannot_compare_value(left_type, left_arg.get_position().clone())))
                }
            },
            BinOp::Less => {
                let left = self.gen_expr(&left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    if left_type.is_signed()? {
                         let result = self.builder.build_int_compare(IntPredicate::SLT, left_value.into_int_value(), right_value.into_int_value(), "less_int")?;
                        Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }else{
                        let result = self.builder.build_int_compare(IntPredicate::ULT, left_value.into_int_value(), right_value.into_int_value(), "less_int")?;
                         Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_compare(FloatPredicate::OLT, left_value.into_float_value(), right_value.into_float_value(), "less_float")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CodeGenError::cannot_compare_value(left_type, left_arg.get_position().clone())))
                }
            },
            BinOp::LessEqual => {
                let left = self.gen_expr(&left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    if left_type.is_signed()? {
                         let result = self.builder.build_int_compare(IntPredicate::SLE, left_value.into_int_value(), right_value.into_int_value(), "less_eq_int")?;
                        Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }else{
                        let result = self.builder.build_int_compare(IntPredicate::ULE, left_value.into_int_value(), right_value.into_int_value(), "less_eq_int")?;
                         Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_compare(FloatPredicate::OLE, left_value.into_float_value(), right_value.into_float_value(), "less_eq_float")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CodeGenError::cannot_compare_value(left_type, left_arg.get_position().clone())))
                }
            },
            BinOp::Greater => {
                let left = self.gen_expr(&left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    if left_type.is_signed()? {
                         let result = self.builder.build_int_compare(IntPredicate::SGT, left_value.into_int_value(), right_value.into_int_value(), "greater_int")?;
                        Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }else{
                        let result = self.builder.build_int_compare(IntPredicate::UGT, left_value.into_int_value(), right_value.into_int_value(), "greater_int")?;
                         Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_compare(FloatPredicate::OGT, left_value.into_float_value(), right_value.into_float_value(), "greater_float")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CodeGenError::cannot_compare_value(left_type, left_arg.get_position().clone())))
                }
            },
            BinOp::GreaterEqual => {
                let left = self.gen_expr(&left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    if left_type.is_signed()? {
                         let result = self.builder.build_int_compare(IntPredicate::SGE, left_value.into_int_value(), right_value.into_int_value(), "greater_eq_int")?;
                        Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }else{
                        let result = self.builder.build_int_compare(IntPredicate::UGE, left_value.into_int_value(), right_value.into_int_value(), "greater_eq_int")?;
                         Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                    }
                }else if left_value.is_float_value() {
                    let result = self.builder.build_float_compare(FloatPredicate::OGE, left_value.into_float_value(), right_value.into_float_value(), "greater_eq_float")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CodeGenError::cannot_compare_value(left_type, left_arg.get_position().clone())))
                }
            },
            BinOp::And => {
                let left = self.gen_expr(&left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    let result = self.builder.build_and(left_value.into_int_value(), right_value.into_int_value(), "and_int")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CodeGenError::cannot_apply_logical_op_value(left_type, left_arg.get_position().clone())))
                }
            },
            BinOp::Or => {
                let left = self.gen_expr(&left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let (left, right) = self.bin_expr_implicit_cast(left, right)?;
                let left_type = left.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if left_value.is_int_value() {
                    let result = self.builder.build_or(left_value.into_int_value(), right_value.into_int_value(), "or_int")?;
                    Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
                }else{
                    Err(Box::new(CodeGenError::cannot_apply_logical_op_value(left_type, left_arg.get_position().clone())))
                }
            },
            BinOp::Comma => {
                let _left = self.gen_expr(&*left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&*right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;

                Ok(Some(right))
            },
            BinOp::ShiftLeft => {
                let left = self.gen_expr(&*left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&*right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let left_type = left.get_type();
                let right_type = right.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if ! left_value.is_int_value() {
                    return Err(Box::new(CodeGenError::not_int_in_shift(&left_type, left_arg.get_position().clone())));
                }
                if ! right_value.is_int_value() {
                    return Err(Box::new(CodeGenError::not_int_in_shift(&right_type, right_arg.get_position().clone())));
                }

                let result = self.builder.build_left_shift(left_value.into_int_value(), right_value.into_int_value(), "ShiftLeft")?;
                Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
            },
            BinOp::ShiftRight => {
                let left = self.gen_expr(&*left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&*right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let left_type = left.get_type();
                let right_type = right.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if ! left_value.is_int_value() {
                    return Err(Box::new(CodeGenError::not_int_in_shift(&left_type, left_arg.get_position().clone())));
                }
                if ! right_value.is_int_value() {
                    return Err(Box::new(CodeGenError::not_int_in_shift(&right_type, right_arg.get_position().clone())));
                }

                let result = self.builder.build_right_shift(left_value.into_int_value(), right_value.into_int_value(), left_type.is_signed()?, "ShiftRight")?;
                Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
            },
            BinOp::BitAnd => {
                let left = self.gen_expr(&*left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&*right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let left_type = left.get_type();
                let right_type = right.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if ! left_value.is_int_value() {
                    return Err(Box::new(CodeGenError::not_int_bit_and(&left_type, left_arg.get_position().clone())));
                }
                if ! right_value.is_int_value() {
                    return Err(Box::new(CodeGenError::not_int_bit_and(&right_type, right_arg.get_position().clone())));
                }

                let result = self.builder.build_and(left_value.into_int_value(), right_value.into_int_value(), "BitAnd")?;
                Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
            },
            BinOp::BitOr => {
                let left = self.gen_expr(&*left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&*right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let left_type = left.get_type();
                let right_type = right.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if ! left_value.is_int_value() {
                    return Err(Box::new(CodeGenError::not_int_bit_or(&left_type, left_arg.get_position().clone())));
                }
                if ! right_value.is_int_value() {
                    return Err(Box::new(CodeGenError::not_int_bit_or(&right_type, right_arg.get_position().clone())));
                }

                let result = self.builder.build_and(left_value.into_int_value(), right_value.into_int_value(), "BitOr")?;
                Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
            },
            BinOp::BitXor => {
                let left = self.gen_expr(&*left_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(left_arg.get_position().clone()))?;
                let right = self.gen_expr(&*right_arg, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(right_arg.get_position().clone()))?;
                let left_type = left.get_type();
                let right_type = right.get_type();
                let left_value = left.get_value();
                let right_value = right.get_value();

                if ! left_value.is_int_value() {
                    return Err(Box::new(CodeGenError::not_int_bit_xor(&left_type, left_arg.get_position().clone())));
                }
                if ! right_value.is_int_value() {
                    return Err(Box::new(CodeGenError::not_int_bit_xor(&right_type, right_arg.get_position().clone())));
                }

                let result = self.builder.build_xor(left_value.into_int_value(), right_value.into_int_value(), "BitXor")?;
                Ok(Some(CompiledValue::new(left_type.clone(), result.as_any_value_enum())))
            },
        }
    }

    fn gen_assign<'b, 'c>(&self,
        l_value: &ExprAST,
        r_value: &ExprAST,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {
        let compiled_value = self.gen_expr(r_value, env, break_catcher, continue_catcher)?.ok_or(Box::new(CodeGenError::assign_illegal_value(r_value, r_value.get_position().clone())))?;
        let value = self.try_as_basic_value(&compiled_value.get_value(), l_value.get_position())?;
        // let from_type = compiled_value.get_type();
        let (_to_type, ptr) = self.get_l_value(l_value, env, break_catcher, continue_catcher)?;

        self.builder.build_store(ptr, value)?;
        Ok(Some(compiled_value))
    }

    fn gen_op_assign<'b, 'c>(&self,
        op: &BinOp,
        l_value: &ExprAST,
        r_value: &ExprAST,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {
        let compiled_value = self.gen_bin_expr(op, l_value, r_value, env, break_catcher, continue_catcher)?.ok_or(Box::new(CodeGenError::cannot_calculate(l_value.get_position().clone())))?;
        let value = self.try_as_basic_value(&compiled_value.get_value(), l_value.get_position())?;
        // let from_type = compiled_value.get_type();
        let (_to_type, ptr) = self.get_l_value(l_value, env, break_catcher, continue_catcher)?;

        self.builder.build_store(ptr, value)?;
        Ok(Some(compiled_value))
    }

    fn get_l_value<'b, 'c>(&self,
        ast: &ExprAST,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<(Type, PointerValue<'ctx>), Box<dyn Error>> {
        match ast {
            ExprAST::Symbol(name, pos) => {
                let (typ, ptr) = env.get_ptr(&name).ok_or(Box::new(CodeGenError::no_such_a_variable(&name, pos.clone())))?;
                Ok((typ.clone(), ptr))
            },
            ExprAST::UnaryPointerAccess(boxed_ast, pos) => {  // *pointer
                let ast = &**boxed_ast;
                let (typ, ptr_to_ptr) = self.get_l_value(ast, env, break_catcher, continue_catcher)?;

                if let Some(type2) = typ.peel_off_pointer() {
                    let ptr = self.builder.build_load(ptr_to_ptr, "load_ptr")?;

                    Ok((Type::new_pointer_type(type2.clone(), false, false), ptr.into_pointer_value()))
                }else{
                    Err(Box::new(CodeGenError::not_pointer(&typ, pos.clone())))
                }
            },
            ExprAST::MemberAccess(expr, member_name, pos) => {  // struct_or_union.member
                let ast = &**expr;
                let (_typ, ptr) = self.get_l_value(ast, env, break_catcher, continue_catcher)?;
                let typ = TypeUtil::get_type(ast, env)?;

                match &typ {
                    Type::Struct {name, fields} => {
                        let index = fields.get_index(member_name).ok_or(CodeGenError::no_such_a_member(name, member_name, pos.clone()))?;
                        let elem_type = fields.get_type(member_name).unwrap();
                        let msg = if let Some(id) = &name {
                            format!("struct_{}.{}", id, member_name)
                        }else{
                            format!("struct?.{}", member_name)
                        };

                        let elem_ptr = self.builder.build_struct_gep(ptr, index as u32, &msg);
                        if let Ok(p) = elem_ptr {
                            Ok((elem_type.clone(), p))
                        }else{
                            return Err(Box::new(CodeGenError::cannot_access_struct_member(&member_name, pos.clone())));
                        }
                    },
                    Type::Union { name, fields } => {
                        let typ = fields.get_type(member_name).ok_or(CodeGenError::no_such_a_member(name, member_name, pos.clone()))?;
                        let to_type = TypeUtil::to_basic_type_enum(typ, self.context, pos)?;
                        let ptr_type = to_type.ptr_type(AddressSpace::default());
                        let p = ptr.const_cast(ptr_type);
                        Ok((typ.clone(), p))

                        // if let Some(id) = name {
                        //     let type_or_union = env.get_type(&id).ok_or(CodeGenError::no_such_a_member(name, member_name, pos.clone()))?;
                        //     match type_or_union {
                        //         TypeOrUnion::Union { type_list, index_map, max_size: _, max_size_type: _ } => {
                        //             let idx = index_map[member_name];
                        //             let (typ, to_type) = &type_list[idx];
                        //             let ptr_type = to_type.ptr_type(AddressSpace::default());

                        //             let p = ptr.const_cast(ptr_type);
                        //             Ok((typ.clone(), p))
                        //         },
                        //         _ => return Err(Box::new(CodeGenError::not_union(&id, pos.clone()))),
                        //     }
                        // }else{
                        //     let typ = fields.get_type(member_name).ok_or(CodeGenError::no_such_a_member(name, member_name, pos.clone()))?;
                        //     let to_type = TypeUtil::to_basic_type_enum(typ, self.context, pos)?;
                        //     let ptr_type = to_type.ptr_type(AddressSpace::default());
                        //     let p = ptr.const_cast(ptr_type);
                        //     Ok((typ.clone(), p))
                        // }
                    },
                    Type::Pointer(_, _elem_type) => {
                        match ast {
                            ExprAST::ArrayAccess(expr, index_list, pos) => {
                                let ast = &**expr;
                                let (expr_type, base_ptr) = self.get_l_value(ast, env, break_catcher, continue_catcher)?;
                                let index_len = index_list.len();

                                if index_len > 1 {
                                    return Err(Box::new(CodeGenError::array_index_is_too_long(pos.clone())));
                                }
            
                                let value = self.gen_expr(&index_list[0], env, break_catcher, continue_catcher)?.ok_or(CodeGenError::no_index_value_while_access_array(pos.clone()))?;
                                let index_val = value.get_value().into_int_value();
                                let index_list = [index_val];
                                let ptr = self.builder.build_load(base_ptr, "load_ptr")?.into_pointer_value();
                                let ptr = unsafe { ptr.const_in_bounds_gep(&index_list) };

                                let typ = if let Some(type2) = expr_type.peel_off_pointer() {
                                    type2
                                }else{
                                    return Err(Box::new(CodeGenError::not_pointer(&expr_type, pos.clone())));
                                };
                                match &typ {
                                    Type::Struct {name, fields} => {
                                        let index = fields.get_index(member_name).ok_or(CodeGenError::no_such_a_member(name, member_name, pos.clone()))?;
                                        let elem_type = fields.get_type(member_name).unwrap();
                                        let msg = if let Some(id) = &name {
                                            format!("struct_{}.{}", id, member_name)
                                        }else{
                                            format!("struct?.{}", member_name)
                                        };
                
                                        let elem_ptr = self.builder.build_struct_gep(ptr, index as u32, &msg);
                                        if let Ok(p) = elem_ptr {
                                            Ok((elem_type.clone(), p))
                                        }else{
                                            return Err(Box::new(CodeGenError::cannot_access_struct_member(&member_name, pos.clone())));
                                        }
                                    },
                                    Type::Union { name, fields } => {
                                        let typ = fields.get_type(member_name).ok_or(CodeGenError::no_such_a_member(name, member_name, pos.clone()))?;
                                        let to_type = TypeUtil::to_basic_type_enum(typ, self.context, pos)?;
                                        let ptr_type = to_type.ptr_type(AddressSpace::default());
                                        let p = ptr.const_cast(ptr_type);
                                        Ok((typ.clone(), p))

                                        // if let Some(id) = name {
                                        //     let type_or_union = env.get_type(&id).ok_or(CodeGenError::no_such_a_member(name, member_name, pos.clone()))?;
                                        //     match type_or_union {
                                        //         TypeOrUnion::Union { type_list, index_map, max_size: _, max_size_type: _ } => {
                                        //             let idx = index_map[member_name];
                                        //             let (typ, to_type) = &type_list[idx];
                                        //             let ptr_type = to_type.ptr_type(AddressSpace::default());
                
                                        //             let p = ptr.const_cast(ptr_type);
                                        //             Ok((typ.clone(), p))
                                        //         },
                                        //         _ => return Err(Box::new(CodeGenError::not_union(&id, pos.clone()))),
                                        //     }
                                        // }else{
                                        //     let typ = fields.get_type(member_name).ok_or(CodeGenError::no_such_a_member(name, member_name, pos.clone()))?;
                                        //     let to_type = TypeUtil::to_basic_type_enum(typ, self.context, pos)?;
                                        //     let ptr_type = to_type.ptr_type(AddressSpace::default());
                                        //     let p = ptr.const_cast(ptr_type);
                                        //     Ok((typ.clone(), p))
                                        // }
                                    },
                                    _ => {
                                        Err(Box::new(CodeGenError::has_not_member(typ.to_string(), member_name.to_string(), pos.clone())))
                                    }
                                }

                            },
                            _ => {
                                Err(Box::new(CodeGenError::has_not_member(typ.to_string(), member_name.to_string(), pos.clone())))
                            },
                        }
                    },
                    _ => {
                        Err(Box::new(CodeGenError::has_not_member(typ.to_string(), member_name.to_string(), pos.clone())))
                    }
                }
            },
            ExprAST::PointerAccess(expr, member_name, pos) => {  // ptr->member
                let ast = &**expr;
                let (_typ, ptr) = self.get_l_value(ast, env, break_catcher, continue_catcher)?;
                let ptr = self.builder.build_load(ptr, "get_pointer")?.into_pointer_value();
                let typ = TypeUtil::get_type(ast, env)?;
                let pointed_type = typ.get_pointed_type(pos)?;

                match pointed_type {
                    Type::Struct {fields, name} => {
                        let index = fields.get_index(member_name).ok_or(CodeGenError::no_such_a_member(name, member_name, pos.clone()))?;
                        let elem_ptr = self.builder.build_struct_gep(ptr, index as u32, "struct_member_access");
                        if let Ok(p) = elem_ptr {
                            let typ = fields.get_type(member_name).unwrap();
                            Ok((typ.clone(), p))
                        }else{
                            return Err(Box::new(CodeGenError::cannot_access_struct_member(&member_name, pos.clone())));
                        }
                    },
                    Type::Union { name, fields } => {
                        let typ = fields.get_type(member_name).ok_or(CodeGenError::no_such_a_member(name, member_name, pos.clone()))?;
                        let to_type = TypeUtil::to_basic_type_enum(typ, self.context, pos)?;
                        let ptr_type = to_type.ptr_type(AddressSpace::default());
                        let p = ptr.const_cast(ptr_type);
                        Ok((typ.clone(), p))

                        // if let Some(id) = name {
                        //     let type_or_union = env.get_type(&id).ok_or(CodeGenError::no_such_a_member(name, member_name, pos.clone()))?;
                        //     match type_or_union {
                        //         TypeOrUnion::Union { type_list, index_map, max_size: _, max_size_type: _ } => {
                        //             let idx = index_map[member_name];
                        //             let (typ, to_type) = &type_list[idx];
                        //             let ptr_type = to_type.ptr_type(AddressSpace::default());

                        //             let p = ptr.const_cast(ptr_type);
                        //             Ok((typ.clone(), p))
                        //         },
                        //         _ => return Err(Box::new(CodeGenError::not_union(&id, pos.clone()))),
                        //     }
                        // }else{
                        //     let typ = fields.get_type(member_name).ok_or(CodeGenError::no_such_a_member(name, member_name, pos.clone()))?;
                        //     let to_type = TypeUtil::to_basic_type_enum(typ, self.context, pos)?;
                        //     let ptr_type = to_type.ptr_type(AddressSpace::default());
                        //     let p = ptr.const_cast(ptr_type);
                        //     Ok((typ.clone(), p))
                        // }
                    },
                    _ => {
                        Err(Box::new(CodeGenError::has_not_member(pointed_type.to_string(), member_name.to_string(), pos.clone())))
                    },
                }
            },
            ExprAST::ArrayAccess(expr, index_list, pos) => {
                let ast = &**expr;
                let (expr_type, base_ptr) = self.get_l_value(ast, env, break_catcher, continue_catcher)?;
                let index_len = index_list.len();

                //
                // when Pointer
                //
                if let Type::Pointer(_, elem_type) = &expr_type {
                    if index_len > 1 {
                        return Err(Box::new(CodeGenError::array_index_is_too_long(pos.clone())));
                    }

                    let value = self.gen_expr(&index_list[0], env, break_catcher, continue_catcher)?.ok_or(CodeGenError::no_index_value_while_access_array(pos.clone()))?;
                    let index_val = value.get_value().into_int_value();
                    let index_list = [index_val];
                    let ptr = self.builder.build_load(base_ptr, "load_ptr")?.into_pointer_value();
                    let ptr = unsafe { ptr.const_in_bounds_gep(&index_list) };

                    return Ok((*elem_type.clone(), ptr));
                }

                //
                // when Array
                //
                if ! expr_type.is_array() {
                    return Err(Box::new(CodeGenError::not_array(ast.clone(), pos.clone())));
                }
                let array_dim = expr_type.get_array_dimension();
                let array_dim_len = array_dim.len();

                if index_len > array_dim_len {
                    return Err(Box::new(CodeGenError::array_index_is_too_long(pos.clone())));
                }

                let item_type = expr_type.get_array_item_type();
                let result_type;
                if index_len == array_dim_len {
                    result_type = item_type.clone();
                }else{  // index_len < array_dim_len
                    let vec = array_dim[index_len..].to_vec();
                    result_type = Type::Array{
                        name: expr_type.get_array_name().clone(),
                        typ: Box::new(item_type.clone()),
                        size_list: vec,
                    };
                }

                let mut ptr = base_ptr;
                for index in index_list {
                    let value = self.gen_expr(index, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::no_index_value_while_access_array(pos.clone()))?;
                    let index_val = value.get_value().into_int_value();

                    let i32_type = self.context.i32_type();
                    let const_zero = i32_type.const_zero();
                    let index_list = [const_zero, index_val];
                    ptr = unsafe { ptr.const_in_bounds_gep(&index_list) };
                }

                Ok((result_type, ptr))
            },
            ExprAST::_self(pos) => {
                let (typ, ptr) = env.get_ptr("self").ok_or(Box::new(CodeGenError::no_such_a_variable("self", pos.clone())))?;
                Ok((typ.clone(), ptr))
            },
            ExprAST::_Self(pos) => {
                Err(Box::new(CodeGenError::self_has_not_l_value(pos.clone())))
            },
            _ => {
                Err(Box::new(CodeGenError::has_not_l_value(format!("{:?}", ast), ast.get_position().clone())))
            }
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

    fn make_fun_type(name: &str, ret_type: &Type, params: &Params, _env: &Env) -> Result<CustFunctionType, CodeGenError> {
        Ok(CustFunctionType::new(
            Some(name.to_string()),
            ret_type.clone(),
            params.get_params_type(),
            params.has_variadic()
        ))
    }

    fn to_function(&self, any_val: &AnyValueEnum<'ctx>, pos: &Position) -> Result<FunctionValue<'ctx>, CodeGenError> {
        match any_val {
            AnyValueEnum::FunctionValue(fun) => Ok(*fun),
            _ => Err(CodeGenError::cannot_call_not_function(any_val, pos.clone())),
        }
    }

    fn try_as_basic_metadata_value(&self, any_val: &AnyValueEnum<'ctx>, pos: &Position) -> Result<BasicMetadataValueEnum<'ctx>, CodeGenError> {
        match any_val {
            AnyValueEnum::ArrayValue(val) => Ok(BasicMetadataValueEnum::ArrayValue(*val)),
            AnyValueEnum::IntValue(val) => Ok(BasicMetadataValueEnum::IntValue(*val)),
            AnyValueEnum::FloatValue(val) => Ok(BasicMetadataValueEnum::FloatValue(*val)),
            AnyValueEnum::PointerValue(val) => Ok(BasicMetadataValueEnum::PointerValue(*val)),
            AnyValueEnum::StructValue(val) => Ok(BasicMetadataValueEnum::StructValue(*val)),
            AnyValueEnum::VectorValue(val) => Ok(BasicMetadataValueEnum::VectorValue(*val)),
            // _ => Err(CodeGenError::cannot_convert_anyvalueenum_to_basicmetadatavalueenum(None, any_val)),
            _ => Err(CodeGenError::cannot_convert_anyvalueenum_to_basicmetadatavalueenum(pos.clone())),
        }
    }

    fn try_as_basic_value(&self, any_val: &AnyValueEnum<'ctx>, pos: &Position) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        match any_val {
            AnyValueEnum::ArrayValue(val) => Ok(BasicValueEnum::ArrayValue(*val)),
            AnyValueEnum::IntValue(val) => Ok(BasicValueEnum::IntValue(*val)),
            AnyValueEnum::FloatValue(val) => Ok(BasicValueEnum::FloatValue(*val)),
            AnyValueEnum::PointerValue(val) => Ok(BasicValueEnum::PointerValue(*val)),
            AnyValueEnum::StructValue(val) => Ok(BasicValueEnum::StructValue(*val)),
            AnyValueEnum::VectorValue(val) => Ok(BasicValueEnum::VectorValue(*val)),
            // _ => Err(CodeGenError::cannot_convert_anyvalueenum_to_basicvalueenum(None, any_val)),
            _ => Err(CodeGenError::cannot_convert_anyvalueenum_to_basicvalueenum(pos.clone())),
        }
    }

    fn try_as_basic_type(&self, any_type: &AnyTypeEnum<'ctx>, pos: &Position) -> Result<BasicTypeEnum<'ctx>, CodeGenError> {
        match any_type {
            AnyTypeEnum::ArrayType(val) => Ok(BasicTypeEnum::ArrayType(*val)),
            AnyTypeEnum::IntType(val) => Ok(BasicTypeEnum::IntType(*val)),
            AnyTypeEnum::FloatType(val) => Ok(BasicTypeEnum::FloatType(*val)),
            AnyTypeEnum::PointerType(val) => Ok(BasicTypeEnum::PointerType(*val)),
            AnyTypeEnum::StructType(val) => Ok(BasicTypeEnum::StructType(*val)),
            AnyTypeEnum::VectorType(val) => Ok(BasicTypeEnum::VectorType(*val)),
            // _ => Err(CodeGenError::cannot_convert_anytypeenum_to_basictypeenum(None, any_type)),
            _ => Err(CodeGenError::cannot_convert_anytypeenum_to_basictypeenum(pos.clone())),
        }
    }

    fn gen_code_function_prototype(&self,
        fn_name: &str,
        ret_type: &Type,
        params: &Params,
        env: &mut Env<'ctx>,
        pos: &Position
    ) -> Result<FunctionValue<'ctx>, Box<dyn Error>> {
        let fn_type = self.make_function_type(&ret_type, params, env, pos)?;
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
        continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<FunctionValue<'ctx>, Box<dyn Error>> {

        let fn_type = self.make_function_type(&ret_type, params, env, pos)?;
        let has_variadic = false;
        let cust_fn_type = CustFunctionType::new(Some(fn_name.to_string()), ret_type.clone(), params.get_params_type(), has_variadic);
        // TODO: 同名のプロトタイプが存在しないか確認する。
        //       存在した場合、プロトタイプと関数の型が一致するかチェックする。

        // prologue
        let function = self.module.add_function(fn_name, fn_type, None);
        env.set_current_function(&cust_fn_type, function);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        env.add_new_function_local();
        // let result = self.gen_code_function_sub(&fn_type, &function, params, body, labels, env, break_catcher, continue_catcher);
        let result = self.gen_code_function_sub(&cust_fn_type, &function, params, body, labels, env, break_catcher, continue_catcher);
        env.remove_function_local();

        if let Err(err) = result {
            Err(err)
        }else{
            Ok(function)
        }
    }

    fn gen_code_function_sub<'b, 'c>(&self,
        // fn_type: &FunctionType,
        fn_type: &CustFunctionType,
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
            let ptr = self.builder.build_alloca(TypeUtil::to_basic_type_enum(&typ, self.context, param.get_position())?, name)?;
            let value = function.get_nth_param(i as u32).unwrap();
            self.builder.build_store(ptr, value)?;
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

        let ret_type = fn_type.get_return_type();
        if ret_type.is_void() {
            // 戻り値の型がvoidで、かつ、最後の行がreturn文でないときに、build_returnしておく。
            match last_stmt {
                Some(AST::Return(None, _)) => Ok(()),  // do nothing
                Some(AST::Return(Some(expr), pos)) => {
                    let typ: Type = TypeUtil::get_type(expr, env)?;
                    return Err(Box::new(CodeGenError::return_type_mismatch(fn_type.get_return_type().clone(), typ, pos.clone())));
                },
                _ => {
                    self.builder.build_return(None)?;
                    Ok(())
                },
            }
        }else{
            // TODO: 関数が返す型をチェックする。

            // 最後の文の型をチェックする。
            match last_stmt {
                Some(AST::Return(None, pos)) => {
                    return Err(Box::new(CodeGenError::no_return_for_type(&fn_type.get_return_type(), pos.clone())));
                },
                Some(AST::Return(Some(_expr), _pos)) => {
                    // let expr_type = expr.get_type(env)?.to_basic_type_enum(self.context)?;
                    // if ret_type != expr_type {
                        // return Err(Box::new(CompileError::return_type_mismatch(None, ret_type.clone(), expr_type.clone())));
                    // }

                    Ok(())
                },
                Some(AST::If(_cond, _then, _else, pos)) => {
                    let typ = self.calc_ret_type(last_stmt.unwrap(), env)?;
                    if typ.is_void() {
                        Err(Box::new(CodeGenError::return_type_mismatch(ret_type.clone(), Type::Void, pos.clone())))
                    }else{
                        if *ret_type == typ {
                            // 最後の文が、ifのとき、ラベルif.endの後にコードが生成されないのでセグフォが起きることへのケア
                            self.builder.build_return(None)?;

                            Ok(())
                        }else{
                            Err(Box::new(CodeGenError::return_type_mismatch(ret_type.clone(), typ, pos.clone())))
                        }

                    }
                },
                Some(stmt) => {  // TODO: if文などの時に内部でreturnしているかもしれないので、その処理。
                    // if let Some(typ) = self.calc_ret_type(stmt, env)? {
                    let typ = self.calc_ret_type(stmt, env)?;
                    if typ.is_void() {
                        Err(Box::new(CodeGenError::return_type_mismatch(ret_type.clone(), Type::Void, stmt.get_position().clone())))
                    }else{
                        if *ret_type == typ {
                            Ok(())
                        }else{
                            Err(Box::new(CodeGenError::return_type_mismatch(ret_type.clone(), typ, stmt.get_position().clone())))
                        }
                    }

                },
                _ => {
                    // return Err(Box::new(CompileError::no_return_for_type(None, &ret_type)))
                    Ok(())
                },
            }
        }
    }

    // fn calc_ret_type(&self, stmt: &AST, env: &Env) -> Result<Option<BasicTypeEnum>, Box<dyn Error>> {
    fn calc_ret_type(&self, stmt: &AST, env: &Env) -> Result<Type, Box<dyn Error>> {
        match stmt {
            AST::Return(None, _pos) => {
                Ok(Type::Void)
            },
            AST::Return(Some(expr), _pos) => {
                let typ = TypeUtil::get_type(&**expr, env)?;
                // let expr_type = TypeUtil::to_basic_type_enum(&typ, self.context)?;
                Ok(typ)
            },
            AST::If(_cond, if_then, if_else, pos) => {
                let then_type = self.calc_ret_type(if_then, env)?;

                if then_type.is_void() {
                    if let Some(else_expr) = if_else {
                        let else_type = self.calc_ret_type(else_expr, env)?;

                        if else_type.is_void() {
                            Ok(Type::Void)
                        }else{
                            Err(Box::new(CodeGenError::mismatch_type_in_if(pos.clone(), then_type, else_type)))
                        }

                    }else{
                        Ok(Type::Void)
                    }

                }else{
                    if let Some(else_expr) = if_else {
                        let else_type = self.calc_ret_type(else_expr, env)?;
                        if else_type.is_void() {
                            Err(Box::new(CodeGenError::mismatch_type_in_if(pos.clone(), then_type, else_type)))
                        }else{
                            Ok(else_type)
                        }

                    }else{
                        Err(Box::new(CodeGenError::mismatch_type_in_if(pos.clone(), then_type, Type::Void)))
                    }
                }
            },
            AST::Block(blk, _pos) => {
                let mut typ = Type::Void;

                for e in &blk.body {
                    typ = self.calc_ret_type(e, env)?;

                    match e {
                        AST::Return(_, _) => {
                            break;
                        },
                        _ => (),  // do nothing
                    }
                }

                Ok(typ)
            },
            AST::Switch(..) => {
                Ok(Type::Void)
            },
            _ => {
                Ok(Type::Void)
            },
        }
    }

    fn make_function_type(&self, ret_type: &Type, params: &Params, _env: &Env<'ctx>, pos: &Position) -> Result<FunctionType<'ctx>, Box<dyn Error>> {
        let ret_t = TypeUtil::to_llvm_any_type(ret_type, self.context, pos)?;
        let has_variadic = params.has_variadic();

        let mut arg_type_vec = Vec::new();

        if let Some(cust_self) = params.get_self() {
            let typ = cust_self.get_type();
            let typ = Type::new_pointer_type(typ.clone(), false, false);
            let t = TypeUtil::to_llvm_type(&typ, self.context, pos)?;

            arg_type_vec.push(t);
        }

        for ds in params.get_params() {
            let typ = ds.get_type();

            let t = TypeUtil::to_llvm_type(&typ, self.context, pos)?;
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
            _ => Err(Box::new(CodeGenError::cannot_make_fn_type(pos.clone()))),
        }
    }
}

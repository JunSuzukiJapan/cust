use crate::parser::{ExprAST, BinOp, Type, Pointer, NumberType};
use crate::parser::{Initializer, EnumLiteral};
use super::{CompiledValue, CodeGenError};
use super::Env;
use super::env::{BreakCatcher, ContinueCatcher, TypeOrUnion};
use super::caster::Caster;
use super::type_util::TypeUtil;
use crate::CodeGen;

use inkwell::values::{AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue};
use inkwell::types::{AsTypeRef,  BasicType, BasicTypeEnum, PointerType};
use inkwell::basic_block::BasicBlock;
use inkwell::{IntPredicate, FloatPredicate};
use inkwell::AddressSpace;
use inkwell::types::AnyType;
use parser::Position;
use std::error::Error;
use std::rc::Rc;

impl<'ctx> CodeGen<'ctx> {

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
                Ok(Some(CompiledValue::new(Type::Number(NumberType::Char).into(), result.as_any_value_enum())))
            },
            ExprAST::Int(num, _pos) => {
                let i32_type = self.context.i32_type();
                let result = i32_type.const_int(*num as u64, true);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::Int).into(), result.as_any_value_enum())))
            },
            ExprAST::Short(num, _pos) => {
                let i16_type = self.context.i16_type();
                let result = i16_type.const_int(*num as u64, true);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::Short).into(), result.as_any_value_enum())))
            },
            ExprAST::Long(num, _pos) => {
                let i64_type = self.context.i64_type();
                let result = i64_type.const_int(*num as u64, true);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::Long).into(), result.as_any_value_enum())))
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
                Ok(Some(CompiledValue::new(Type::Number(NumberType::UnsignedChar).into(), result.as_any_value_enum())))
            },
            ExprAST::UInt(num, _pos) => {
                let i32_type = self.context.i32_type();
                let result = i32_type.const_int(*num as u64, false);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::UnsignedInt).into(), result.as_any_value_enum())))
            },
            ExprAST::UShort(num, _pos) => {
                let i16_type = self.context.i16_type();
                let result = i16_type.const_int(*num as u64, false);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::UnsignedShort).into(), result.as_any_value_enum())))
            },
            ExprAST::ULong(num, _pos) => {
                let i64_type = self.context.i64_type();
                let result = i64_type.const_int(*num as u64, false);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::UnsignedLong).into(), result.as_any_value_enum())))
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
                Ok(Some(CompiledValue::new(Type::Number(NumberType::Double).into(), result.as_any_value_enum())))
            },
            ExprAST::Double(num, _pos) => {
                let f64_type = self.context.f64_type();
                let result = f64_type.const_float(*num);
                Ok(Some(CompiledValue::new(Type::Number(NumberType::Double).into(), result.as_any_value_enum())))
            },
            ExprAST::StringLiteral(s, _pos) => {
                // let result = self.context.const_string(s.as_bytes(), false);
                let result = self.builder.build_global_string_ptr(s, &format!("global_str_{}", s))?;
                let pointer = Pointer::new(false, false);
                Ok(Some(CompiledValue::new(Type::Pointer(pointer, Box::new(Type::Number(NumberType::Char).into())).into(), result.as_any_value_enum())))
            },
            ExprAST::PreInc(name, sym_pos, pos) => {
                if let Some((typ, _sq, ptr)) = env.get_ptr(name) {
                    let basic_val = self.builder.build_load(ptr, name)?;
                    let any_val = basic_val.as_any_value_enum();
                    let one = TypeUtil::to_llvm_int_type(typ, self.context, pos)?.const_int(1, false);
                    let added = self.builder.build_int_add(any_val.into_int_value(), one, "pre_increment")?;
                    let (_ptr_type, _sq, ptr) = env.get_ptr(&name).ok_or(Box::new(CodeGenError::no_such_a_variable(&name, sym_pos.clone())))?;
                    let _result = self.builder.build_store(ptr, added);

                    Ok(Some(CompiledValue::new(typ.clone(), added.as_any_value_enum())))
    
                }else{
                    Err(Box::new(CodeGenError::no_such_a_variable(name, sym_pos.clone())))
                }
            },
            ExprAST::PreDec(name, sym_pos, pos) => {
                if let Some((typ, _sq, ptr)) = env.get_ptr(name) {
                    let basic_val = self.builder.build_load(ptr, name)?;
                    let any_val = basic_val.as_any_value_enum();
                    let one = TypeUtil::to_llvm_int_type(typ, self.context, pos)?.const_int(1, false);
                    let subed = self.builder.build_int_sub(any_val.into_int_value(), one, "pre_decrement")?;
                    let (_ptr_type, _sq, ptr) = env.get_ptr(&name).ok_or(Box::new(CodeGenError::no_such_a_variable(&name, sym_pos.clone())))?;
                    let _result = self.builder.build_store(ptr, subed);

                    Ok(Some(CompiledValue::new(typ.clone(), subed.as_any_value_enum())))
    
                }else{
                    Err(Box::new(CodeGenError::no_such_a_variable(name, sym_pos.clone())))
                }
            },
            ExprAST::PostInc(name, sym_pos, pos) => {
                if let Some((typ, _sq, ptr)) = env.get_ptr(name) {
                    let basic_val = self.builder.build_load(ptr, name)?;
                    let pre_val = basic_val.as_any_value_enum();
                    let one = TypeUtil::to_llvm_int_type(typ, self.context, pos)?.const_int(1, false);
                    let added = self.builder.build_int_add(pre_val.into_int_value(), one, "post_increment")?;
                    let (_ptr_type, _sq, ptr) = env.get_ptr(&name).ok_or(Box::new(CodeGenError::no_such_a_variable(&name, sym_pos.clone())))?;
                    let _result = self.builder.build_store(ptr, added);

                    Ok(Some(CompiledValue::new(typ.clone(), pre_val)))
    
                }else{
                    Err(Box::new(CodeGenError::no_such_a_variable(name, sym_pos.clone())))
                }
            },
            ExprAST::PostDec(name, sym_pos, pos) => {
                if let Some((typ, _sq, ptr)) = env.get_ptr(name) {
                    let basic_val = self.builder.build_load(ptr, name)?;
                    let pre_val = basic_val.as_any_value_enum();
                    let one = TypeUtil::to_llvm_int_type(typ, self.context, pos)?.const_int(1, false);
                    let subed = self.builder.build_int_sub(pre_val.into_int_value(), one, "post_decrement")?;
                    let (_ptr_type, _sq, ptr) = env.get_ptr(&name).ok_or(Box::new(CodeGenError::no_such_a_variable(&name, sym_pos.clone())))?;
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

                Ok(Some(CompiledValue::new(Rc::new(typ), ptr.into())))
            },
            ExprAST::UnaryPointerAccess(boxed_ast, pos) => {  // *pointer
                let ast = &**boxed_ast;
                let ptr = self.gen_expr(&ast, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::not_pointer(&TypeUtil::get_type(&ast, env)?.as_ref().clone(), pos.clone()))?;
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
                        self.gen_call_function(name, &v, env, break_catcher, continue_catcher, pos)
                    },
                    ExprAST::MemberAccess(ast, fun_name, _pos2) => {
                        let typ = TypeUtil::get_type(ast, env)?;
                        let class_name = typ.get_type_name();
                        let (_t, obj) = self.get_l_value(&**ast, env, break_catcher, continue_catcher)?;

                        let mut args: Vec<BasicMetadataValueEnum> = Vec::new();
                        args.push(obj.into());

                        args.append(&mut v);

                        self.gen_call_member_function(&class_name, &fun_name, &args, env, break_catcher, continue_catcher, pos)

                    },
                    ExprAST::StructStaticSymbol(class_name, method_name, _pos2) => {
                        self.gen_call_class_function(&class_name, &method_name, &v, env, break_catcher, continue_catcher, pos)
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
                if let Type::Array { typ, .. } = typ.as_ref() {
                    let any_val = ptr.as_any_value_enum();
                    let t = Type::new_pointer_type(*typ.clone(), false, false);
                    Ok(Some(CompiledValue::new(t.into(), any_val)))

                }else{
                    let basic_val = self.builder.build_load(ptr, "get_value_from_array")?;
                    let any_val = basic_val.as_any_value_enum();
                    Ok(Some(CompiledValue::new(typ, any_val)))
                }
            },
            ExprAST::Symbol(name, pos) => {
                if let Some((typ, _sq, ptr)) = env.get_ptr(name) {
                    // let basic_val = self.builder.build_load(ptr, name)?;
                    // let any_val = basic_val.as_any_value_enum();
                    // Ok(Some(CompiledValue::new(typ.clone(), any_val)))

                    if let Type::Array { typ, .. } = typ.as_ref() {
                        let any_val = ptr.as_any_value_enum();
                        let t = Type::new_pointer_type(*typ.clone(), false, false);
                        Ok(Some(CompiledValue::new(Rc::new(t), any_val)))
    
                    }else{
                        let basic_val = self.builder.build_load(ptr, name)?;
                        let any_val = basic_val.as_any_value_enum();
                        Ok(Some(CompiledValue::new(typ.clone(), any_val)))
                    }

                }else if let Some((typ, _sq, val)) = env.get_value(name) {
                    if let Type::Array { typ, .. } = typ.as_ref() {
                        let any_val = val.as_any_value_enum();
                        let t = Type::new_pointer_type(*typ.clone(), false, false);
                        Ok(Some(CompiledValue::new(Rc::new(t), any_val)))
    
                    }else{
                        Ok(Some(CompiledValue::new(typ.clone(), val.as_any_value_enum())))
                    }
    
                }else{
                    Err(Box::new(CodeGenError::no_such_a_variable(name, pos.clone())))
                }
            },
            ExprAST::_self(pos) => {
                if let Some((typ, _sq, ptr)) = env.get_self_ptr() {
                    let basic_val = self.builder.build_load(ptr, "get_self")?;
                    let any_val = basic_val.as_any_value_enum();

                    Ok(Some(CompiledValue::new(Rc::clone(typ), any_val)))
                }else{
                    Err(Box::new(CodeGenError::no_such_a_variable("self", pos.clone())))
                }
            },
            ExprAST::UnarySizeOfExpr(expr, pos) => {
                let typ = TypeUtil::get_type(&**expr, env)?;
                let llvm_type = TypeUtil::to_llvm_any_type(&typ, self.context, pos)?;
                let size = llvm_type.size_of().ok_or(CodeGenError::cannot_get_size_of(typ.as_ref().clone(), pos.clone()))?;

                Ok(Some(CompiledValue::new(Type::Number(NumberType::Int).into(), size.as_any_value_enum())))
            },
            ExprAST::UnarySizeOfTypeName(typ, pos) => {
                let llvm_type = TypeUtil::to_llvm_any_type(typ, self.context, pos)?;
                let size = llvm_type.size_of().ok_or(CodeGenError::cannot_get_size_of(typ.as_ref().clone(), pos.clone()))?;

                Ok(Some(CompiledValue::new(Type::Number(NumberType::Int).into(), size.as_any_value_enum())))
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
            ExprAST::StructStaticSymbol(struct_name, var_name, pos) => {  // struct_name::var_name
                if let Some(type_or_union) = env.get_type(struct_name) {
                    match type_or_union {
                        TypeOrUnion::StandardEnum { i32_type: _, enumerator_list, index_map } => {
                            let index = index_map.get(var_name).ok_or(CodeGenError::no_such_a_enum_member(struct_name.to_string(), var_name.to_string(), pos.clone()))?;
                            let (_name, value) = &enumerator_list[*index];
                            return Ok(Some(CompiledValue::new(Type::Number(NumberType::Int).into(), value.as_any_value_enum())))
                        },
                        _ => {
                            ()
                        },
                    }
                }

                let (typ, ptr) = self.get_l_value(expr_ast, env, break_catcher, continue_catcher)?;
                let basic_val = self.builder.build_load(ptr, &format!("access_to_field_{}_in_class_{}", var_name, struct_name))?;
                let any_val = basic_val.as_any_value_enum();
                Ok(Some(CompiledValue::new(typ.clone(), any_val)))

            },
            ExprAST::SelfStaticSymbol(var_name, _pos) => {  // _Self::Symbol
                let (typ, ptr) = self.get_l_value(expr_ast, env, break_catcher, continue_catcher)?;
                let basic_val = self.builder.build_load(ptr, &format!("access_to_field_{}_in_Self", var_name))?;
                let any_val = basic_val.as_any_value_enum();
                Ok(Some(CompiledValue::new(typ.clone(), any_val)))
            },
            ExprAST::StructLiteral(struct_literal) => {
                let typ = struct_literal.get_type();
                let pos = struct_literal.get_position();
                let basic_type = env.basic_type_enum_from_type(&typ, self.context, pos)?;
                let struct_ptr = self.builder.build_alloca(basic_type, "struct_literal")?;

                self.gen_struct_literal(struct_literal, struct_ptr, env, break_catcher, continue_catcher)
            },
            ExprAST::UnionLiteral(typ, list, pos) => {
                let basic_type = env.basic_type_enum_from_type(&typ, self.context, pos)?;
                let union_ptr = self.builder.build_alloca(basic_type, "union_literal")?;
                let union_name = typ.get_type_name();

                for (_field_name, expr_ast2) in list {
                    let any_value = self.gen_expr(expr_ast2, env, break_catcher, continue_catcher)?.unwrap().get_value();
                    let basic_value = BasicValueEnum::try_from(any_value).map_err(|_e| CodeGenError::system_error(pos.clone()))?;
                    let _result = self.builder.build_store(union_ptr, basic_value);                        
                }

                let basic_val = self.builder.build_load(union_ptr, &format!("load_union_{}_literal", union_name))?;
                let any_val = basic_val.as_any_value_enum();
                Ok(Some(CompiledValue::new(typ.clone(), any_val)))
            },
            ExprAST::UnionConstLiteral(typ, const_list, pos) => {
                let basic_type = env.basic_type_enum_from_type(&typ, self.context, pos)?;
                let union_ptr = self.builder.build_alloca(basic_type, "union_literal")?;
                let union_name = typ.get_type_name();

                for (_field_name, const_expr) in const_list {
                    let basic_value = self.const_expr_to_basic_value_enum(const_expr, self.context);
                    let _result = self.builder.build_store(union_ptr, basic_value);                        
                }

                let basic_val = self.builder.build_load(union_ptr, &format!("load_union_{}_literal", union_name))?;
                let any_val = basic_val.as_any_value_enum();
                Ok(Some(CompiledValue::new(typ.clone(), any_val)))
            },
            ExprAST::EnumLiteral(_typ, tag, literal, _pos) => {
                match literal {
                    EnumLiteral::Struct(struct_literal) => {
                        let typ = struct_literal.get_type();
                        let pos = struct_literal.get_position();

                        let tag_type = self.enum_tag_llvm_type;
                        let basic_type = env.basic_type_enum_from_type(&typ, self.context, pos)?;
                        let vec: Vec<BasicTypeEnum> = vec!(tag_type.into(), basic_type);
                        let tagged_type = self.context.struct_type(&vec, false);

                        let tagged_ptr = self.builder.build_alloca(tagged_type, "enum_literal")?;
                        let tag_ptr = self.builder.build_struct_gep(tagged_ptr, 0, "struct_gep_in_tagged_enum")?;
                        let struct_ptr = self.builder.build_struct_gep(tagged_ptr, 1, "struct_gep_in_tagged_enum")?;

                        let tag_value = tag_type.const_int(*tag as u64, false);
                        let _ = self.builder.build_store(tag_ptr, tag_value);

                        let _ = self.gen_struct_literal(struct_literal, struct_ptr, env, break_catcher, continue_catcher)?;

                        let basic_val = self.builder.build_load(tagged_ptr, &format!("load_enum_literal"))?;
                        let any_val = basic_val.as_any_value_enum();
                        Ok(Some(CompiledValue::new(Rc::clone(typ), any_val)))
                    },
                    EnumLiteral::Tuple(literal) => {







                        unimplemented!()


                    },
                }
            },
            ExprAST::TupleLiteral(list, pos) => {
                //
                // codegen each element
                //
                let mut expr_list = Vec::new();
                let mut type_list = Vec::new();
                let mut cust_type_list = Vec::new();

                for expr in list {
                    let expr = self.gen_expr(expr, env, break_catcher, continue_catcher)?.unwrap();
                    let typ = expr.get_type();
                    let t = TypeUtil::to_basic_type_enum(&typ, &self.context, pos)?;

                    expr_list.push(expr.get_value());
                    type_list.push(t);
                    cust_type_list.push(typ.clone());
                }

                let any_type = self.context.struct_type(&type_list, false);
                let basic_type = BasicTypeEnum::try_from(any_type).unwrap();
                let tuple_ptr = self.builder.build_alloca(basic_type, "tuple_literal")?;

                //
                // store each element to tuple structure
                //
                let i32_type = self.context.i32_type();
                let const_zero = i32_type.const_int(0, false);
                let mut index = 0;
                for any_value in expr_list {
                    let basic_value = BasicValueEnum::try_from(any_value).map_err(|_e| CodeGenError::system_error(pos.clone()))?;
                    let const_index = i32_type.const_int(index, false);
                    let indexes = vec![const_zero, const_index];
                    let ptr = unsafe { self.builder.build_in_bounds_gep(tuple_ptr, &indexes, "gep_for_tuple_element")? };
                    let _result = self.builder.build_store(ptr, basic_value);

                    index += 1;
                }

                //
                // return result
                //
                let basic_val = self.builder.build_load(tuple_ptr, &format!("load_tuple_literal"))?;
                let any_val = basic_val.as_any_value_enum();
                let tuple_type = Type::Tuple(cust_type_list);
                Ok(Some(CompiledValue::new(Rc::new(tuple_type), any_val)))
            },
            ExprAST::TupleMemberAccess(tpl, index, pos) => {
                let (elem_type, ptr) = self.get_indexed_tuple_ptr_and_type(tpl, *index, pos, env, break_catcher, continue_catcher)?;
                let basic_val = self.builder.build_load(ptr, "load_tuple_element")?;
                let any_val = basic_val.as_any_value_enum();

                Ok(Some(CompiledValue::new(Rc::clone(&elem_type), any_val)))
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
            Initializer::Array(_list, typ, _pos) => {  // maybe not reached
                // let any_value = self.gen_initializer(init, env, break_catcher, continue_catcher)?;
                // let compiled_value = CompiledValue::new(Rc::clone(typ), any_value);
                // Ok(Some(compiled_value))
                panic!("This process should be done in a function gen_def_var")
            },
            Initializer::Struct(_list, typ, _pos) => {  // maybe not reached
                // let any_value = self.gen_initializer(init, env, break_catcher, continue_catcher)?;
                // let compiled_value = CompiledValue::new(Rc::clone(typ), any_value);
                // Ok(Some(compiled_value))
                panic!("This process should be done in a function gen_def_var")
            }
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
        let (ptr_type, ptr) = self.get_l_value(l_value, env, break_catcher, continue_catcher)?;

        let value_type = value.get_type().as_any_type_enum();
        let ptr_elem_type = ptr.get_type().get_element_type();
        if value_type == ptr_elem_type {
            self.builder.build_store(ptr, value)?;
            Ok(Some(compiled_value))

        }else{
            if ptr_type.is_tagged_enum() {
                // let ptr_type = unsafe { PointerType::new(value_type.as_type_ref()) };
                let t = value_type.as_type_ref();
                let address_space = 0;
                let t = unsafe { llvm_sys::core::LLVMPointerType(t, address_space) };
                let ptr_type = unsafe { PointerType::new(t) };
                let ptr = self.builder.build_pointer_cast(ptr, ptr_type, "cast_ptr_to_value_typ_ptr")?;
                self.builder.build_store(ptr, value)?;
                Ok(Some(compiled_value))

            }else{
                let casted = self.gen_implicit_cast(&value.as_any_value_enum(), &compiled_value.get_type(), &ptr_type, r_value.get_position())?;
                let compiled_value2 = CompiledValue::new(ptr_type.clone(), casted);
                let value2 = self.try_as_basic_value(&compiled_value2.get_value(), l_value.get_position())?;
                self.builder.build_store(ptr, value2)?;

                Ok(Some(compiled_value2))
            }
        }
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

    pub fn get_l_value<'b, 'c>(&self,
        ast: &ExprAST,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<(Rc<Type>, PointerValue<'ctx>), Box<dyn Error>> {
        match ast {
            ExprAST::Symbol(name, pos) => {
                let (typ, sq, ptr) = env.get_ptr(&name).ok_or(Box::new(CodeGenError::no_such_a_variable(&name, pos.clone())))?;
                if sq.is_const() {
                    return Err(Box::new(CodeGenError::cannot_assign_constant(pos.clone())));
                }

                Ok((Rc::clone(typ), ptr))
            },
            ExprAST::UnaryPointerAccess(boxed_ast, pos) => {  // *pointer
                let ast = &**boxed_ast;
                let (typ, ptr_to_ptr) = self.get_l_value(ast, env, break_catcher, continue_catcher)?;

                if let Some(type2) = typ.peel_off_pointer() {
                    let ptr = self.builder.build_load(ptr_to_ptr, "load_ptr")?;

                    Ok((Type::new_pointer_type(type2.clone(), false, false).into(), ptr.into_pointer_value()))
                }else{
                    Err(Box::new(CodeGenError::not_pointer(&typ, pos.clone())))
                }
            },
            ExprAST::MemberAccess(expr, member_name, pos) => {  // struct_or_union.member
                let ast = &**expr;
                let (_typ, ptr) = self.get_l_value(ast, env, break_catcher, continue_catcher)?;
                let typ = TypeUtil::get_type(ast, env)?;

                match typ.as_ref() {
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
                                match typ.as_ref() {
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
                if let Type::Pointer(_, elem_type) = expr_type.as_ref() {
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
                        typ: Box::new(Rc::clone(item_type)),
                        size_list: vec,
                    }.into();
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
                let (typ, _sq, ptr) = env.get_ptr("self").ok_or(Box::new(CodeGenError::no_such_a_variable("self", pos.clone())))?;
                Ok((typ.clone(), ptr))
            },
            ExprAST::SelfStaticSymbol(var_name, pos) => {
                let cls = env.get_current_class().ok_or(CodeGenError::no_current_class(pos.clone()))?;

                if let Some((typ, _sq, ptr)) = cls.get_class_var(var_name) {
                    Ok((typ.clone(), ptr.as_pointer_value()))
                }else{
                    return Err(Box::new(CodeGenError::no_such_a_class_var(cls.get_name().to_string(), var_name.clone(), pos.clone())));
                }
            },
            ExprAST::StructStaticSymbol(struct_name, var_name, pos) => {
                if let Some((typ, _sq, ptr)) = env.get_class_var(struct_name, var_name) {
                    Ok((typ.clone(), ptr.as_pointer_value()))
                }else{
                    return Err(Box::new(CodeGenError::no_such_a_class_var(struct_name.clone(), var_name.clone(), pos.clone())));
                }
            },
            ExprAST::TupleMemberAccess(tpl, index, pos) => {
                self.get_indexed_tuple_ptr_and_type(tpl, *index, pos, env, break_catcher, continue_catcher)
            },
            _ => {
                Err(Box::new(CodeGenError::has_not_l_value(format!("{:?}", ast), ast.get_position().clone())))
            }
        }
    }

    fn get_indexed_tuple_ptr_and_type<'b, 'c>(
        &self,
        tpl: &ExprAST,
        index: usize,
        pos: &Position,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<(Rc<Type>, PointerValue<'ctx>), Box<dyn Error>> {
        //
        // is expr tuple?
        //
        let tuple_type = TypeUtil::get_type(&tpl, env)?;
        if ! tuple_type.is_tuple() {
            return Err(Box::new(CodeGenError::not_tuple_in_tuple_access_by_index(tpl.clone(), pos.clone())));
        }

        //
        // check size
        //
        let size = tuple_type.get_tuple_size().unwrap();
        if size <= index {
            return Err(Box::new(CodeGenError::tuple_index_too_big(size, index, pos.clone())));
        }

        //
        // code gen
        //
        let i32_type = self.context.i32_type();
        let const_zero = i32_type.const_int(0, false);

        let const_index = i32_type.const_int(index as u64, false);
        let indexes = vec![const_zero, const_index];

        let (_typ, tuple_ptr) = self.get_l_value(&tpl, env, break_catcher, continue_catcher)?;
        let ptr = unsafe { self.builder.build_in_bounds_gep(tuple_ptr, &indexes, "gep_for_tuple_element")? };

        let elem_type = {
            match &*tuple_type {
                Type::Tuple(list) => {
                    &list[index]
                },
                _ => panic!(),
            }
        };

        Ok((Rc::clone(elem_type), ptr))
    }

    pub fn bin_expr_implicit_cast(&self, left: CompiledValue<'ctx>, right: CompiledValue<'ctx>) -> Result<(CompiledValue<'ctx>, CompiledValue<'ctx>), Box<dyn Error>> {
        if let (Type::Number(left_type), Type::Number(right_type)) = (left.get_type().as_ref(), right.get_type().as_ref()) {
            if left_type == right_type {
                Ok((left, right))
            }else if left_type < right_type {



                unimplemented!()
            }else{  // left_type > right_type



                unimplemented!()
            }
        }else{
            Ok((left, right))
        }
    }
}

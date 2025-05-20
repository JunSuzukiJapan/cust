#![allow(dead_code)]

use crate::global::global;
use crate::parser::{AST, ToplevelAST, Type, Block, Params, NumberType, Function, FunProto, FunOrProt, Pattern};
#[allow(unused_imports)]
use crate::parser::Pointer;
use crate::parser::{Declaration, DeclarationSpecifier, CustFunctionType, Initializer, ConstInitializer, ImplElement, SpecifierQualifier};
use crate::env::Class;
use super::{CompiledValue, CodeGenError};
use super::Env;
use super::env::{BreakCatcher, ContinueCatcher};
use super::caster::Caster;
use super::type_util::TypeUtil;
use crate::parser::Declarator;
use crate::parser::{ConstExpr, ArrayInitializer};
use crate::{Position};

use inkwell::{OptimizationLevel};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::values::{AnyValue, AnyValueEnum, ArrayValue, AsValueRef, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, GlobalValue, PointerValue, StructValue};
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, IntType};
use inkwell::AddressSpace;
use parser::{EnumPattern, ExprAST};
use std::error::Error;
use std::rc::Rc;

#[cfg(test)] use crate::parser::{DirectDeclarator, Defines, Param};

type EnumTagType = u32;

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
    pub enum_tag_type: Rc<Type>,
    pub enum_tag_llvm_type: IntType<'ctx>,
    pub enum_only_tag_type: Rc<Type>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn try_new(context: &'ctx Context, module_name: &str) -> Result<CodeGen<'ctx>, Box<dyn Error>> {
        let module = context.create_module(module_name);
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
        let enum_tag_type = Rc::new(Type::Number(global().enum_tag_type.clone()));
        let enum_tag_llvm_type = context.i64_type();
        let t = Type::Array {
            name: None,
            typ: Box::new(Rc::new(Type::Number(NumberType::Long))),
            size_list: vec![],
        };
        let enum_only_tag_type = Rc::new(t);

        Ok(CodeGen {
            context: &context,
            module,
            builder: context.create_builder(),
            execution_engine,
            enum_tag_type,
            enum_tag_llvm_type,
            enum_only_tag_type,
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
                let sq = specifiers.get_specifier_qualifier();

                for decl in declaration {
                    let declarator = decl.get_declarator();
                    let name = declarator.get_name();

                    self.gen_global_def_var_sub(name, base_type, sq, decl, declarator, env, break_catcher, continue_catcher, pos)?;
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
            ToplevelAST::DefineStruct{name, fields, type_variables, pos} => {
                self.gen_define_struct(name, fields, type_variables, env, break_catcher, continue_catcher, pos)?;
                Ok(None)
            },
            ToplevelAST::DefineUnion { name, fields, type_variables, pos } => {
                self.gen_define_union(name, fields, type_variables, env, break_catcher, continue_catcher, pos)?;
                Ok(None)
            },
            ToplevelAST::DefineEnum { name, fields, type_variables, pos } => {
                self.gen_define_enum(name, fields, type_variables, env, break_catcher, continue_catcher, pos)?;
                Ok(None)
            },
            ToplevelAST::Impl { name, typ, for_type, defines, pos } => {
                self.gen_impl(name, typ, for_type, defines, env, break_catcher, continue_catcher, pos)?;

                Ok(None)
            },
        }
    }

    pub fn gen_def_var<'b, 'c>(&self,
        specifiers: &DeclarationSpecifier,
        declarations: &Vec<Declaration>,
        env: &mut Env<'ctx>,
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
                    match typ.as_ref() {
                        Type::Struct { fields, .. } => {
                            self.gen_struct_init(&typ, &fields, ptr, &*const_expr, env, break_catcher, continue_catcher)?;
                        },
                        Type::Array { name: _, typ, size_list } => {
                            self.gen_array_init(&size_list, ptr, typ, &*const_expr, env, break_catcher, continue_catcher)?;
                        },
                        _ => {
                            let mut init_value = self.gen_initializer(&*const_expr, env, break_catcher, continue_catcher)?;
                            let init_type = TypeUtil::get_initializer_type(const_expr, env)?;

                            if *typ != *init_type {
                                init_value = self.gen_implicit_cast(&init_value, &init_type, &typ, (*const_expr).get_position())?;
                            }

                            let basic_value = self.try_as_basic_value(&init_value, (*const_expr).get_position())?;
                            self.builder.build_store(ptr, basic_value)?;
                        }
                    }
                },
                None => (),  // do nothing
            };

            let sq = specifiers.get_specifier_qualifier();
            env.insert_local(name, typ.clone(), sq.clone(), ptr);
        }

        Ok(())
    }

    pub fn const_expr_to_basic_value_enum(&self, const_expr: &ConstExpr) -> BasicValueEnum<'ctx> {
        match const_expr {
            ConstExpr::Int(n, _) => {
                let i32_type = self.context.i32_type();
                i32_type.const_int(*n as u64, false).as_basic_value_enum()
            },
            ConstExpr::Unsigned(n, _) => {
                let i32_type = self.context.i32_type();
                i32_type.const_int(*n as u64, true).as_basic_value_enum()
            },
            ConstExpr::LongLong(n, _) => {
                let i64_type = self.context.i64_type();
                i64_type.const_int(*n as u64, false).as_basic_value_enum()
            },
            ConstExpr::ULongLong(n, _) => {
                let i64_type = self.context.i64_type();
                i64_type.const_int(*n as u64, true).as_basic_value_enum()
            },
            ConstExpr::Double(f, _) => {
                let f64_type = self.context.f64_type();
                f64_type.const_float(*f).as_basic_value_enum()
            },
        }
    }

    fn make_array_type(&self, size_list: &[u32], elem_type: &Type, pos: &Position) -> Result<BasicTypeEnum<'ctx>, Box<dyn Error>> {
        let mut typ = TypeUtil::to_basic_type_enum(elem_type, &self.context, pos)?;
        let len = size_list.len();

        for i in 0..len {
            let index = len - i - 1;
            let size = size_list[index];

            typ = typ.array_type(size).as_basic_type_enum();
        }

        Ok(typ)
    }

    fn make_array_init_value<'b, 'c>(&self,
        size_list: &[u32],
        elem_type: &Type,
        init_value_list: &Vec<Box<ArrayInitializer>>,
        env: &Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<(ArrayValue<'ctx>, BasicTypeEnum<'ctx>), Box<dyn Error>> {

        let init_len = init_value_list.len();

        if init_len == 0 {
            let array_type = self.make_array_type(size_list, elem_type, pos)?;
            let null_array: Vec<ArrayValue> = Vec::new();
            let value = unsafe { ArrayValue::new_const_array(&array_type, &null_array) };
            return Ok((value, array_type));
        }

        let dim = size_list.len();
        if dim == 1 {
            //
            // init_len > 0
            //
            let mut vec: Vec<ArrayValue<'ctx>> = Vec::new();
            for i in 0..init_len {
                let init_value = &*init_value_list[i];
                let init_type = TypeUtil::get_initializer_type_of_array_initializer(init_value, env)?;
                if elem_type != init_type.as_ref() {
                    return Err(Box::new(CodeGenError::mismatch_initializer_type(elem_type, &init_type, init_value.get_position().clone())));
                }

                let const_value = init_value.get_const().unwrap();
                let any_val = self.gen_const_initializer(const_value, env, break_catcher, continue_catcher)?;
                let value = unsafe { ArrayValue::new(any_val.as_value_ref()) };

                vec.push(value);
            }
    
            //
            // make array
            //
            let basic_type = TypeUtil::to_basic_type_enum(elem_type, &self.context, pos)?;
            let values = unsafe { ArrayValue::new_const_array(&basic_type, &vec) };
            let array_type = self.make_array_type(size_list, elem_type, pos)?.into_array_type();

            Ok((values, array_type.as_basic_type_enum()))

        }else{  // init_len > 1

            let size_list_sub = &size_list[1..];
            let mut vec = Vec::new();

            for i in 0..init_len {
                let init_value_list = init_value_list[i].get_array_list().unwrap();
                let (value, _typ) = self.make_array_init_value(size_list_sub, elem_type, init_value_list, env, break_catcher, continue_catcher, pos)?;

                vec.push(value);
            }

            if vec.len() != 0 {
                let array_type = self.make_array_type(size_list_sub, elem_type, pos)?.into_array_type();
                let values = array_type.const_array(&vec);

                Ok((values, array_type.as_basic_type_enum()))

            }else{  // null array
                panic!();  // not reached
            }
        }
    }

    pub fn gen_global_array_init<'b, 'c>(&self,
        size_list: &Vec<u32>,
        target_array_ptr: GlobalValue<'ctx>,
        elem_type: &Type,
        init: &Initializer,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let pos: &Position;
        let init_value_list = if let Initializer::Array(list, _typ, pos2) = init {
            pos = pos2;
            list
        }else{
            return Err(Box::new(CodeGenError::initializer_is_not_array(init.get_position().clone())));
        };

        let init_len = init_value_list.len();
        if init_len == 0 {
            return Ok(None);
        }

        let (values, _typ) = self.make_array_init_value(size_list, elem_type, init_value_list, env, break_catcher, continue_catcher, pos)?;
        target_array_ptr.set_initializer(&values.as_basic_value_enum());

        Ok(None)
    }

    pub fn gen_array_init<'b, 'c>(&self,
        size_list: &Vec<u32>,
        target_array_ptr: PointerValue<'ctx>,
        elem_type: &Type,
        init: &Initializer,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let pos: &Position;
        let init_value_list = if let Initializer::Array(list, _typ, pos2) = init {
            pos = pos2;
            list
        }else{
            return Err(Box::new(CodeGenError::initializer_is_not_array(init.get_position().clone())));
        };

        let init_len = init_value_list.len();
        if init_len == 0 {
            return Ok(None);
        }

        let (values, _typ) = self.make_array_init_value(size_list, elem_type, init_value_list, env, break_catcher, continue_catcher, pos)?;
        let _result = self.builder.build_store(target_array_ptr, values.as_basic_value_enum());

        Ok(None)
    }

    pub fn gen_global_tuple_init<'b, 'c>(&self,
        type_list: &Vec<Rc<Type>>,
        target_tuple_ptr: GlobalValue<'ctx>,
        init: &Initializer,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let init_value_list = if let Initializer::Simple(ExprAST::TupleLiteral(expr_list, _pos), _pos2) = init {
            expr_list
        }else{
            return Err(Box::new(CodeGenError::initializer_is_not_tuple(init.get_position().clone())));
        };

        let target_len = type_list.len();
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

        let values = self.make_tuple_init_value(type_list, init.get_position(), init_value_list, env, break_catcher, continue_catcher)?;
        target_tuple_ptr.set_initializer(&values.as_basic_value_enum());

        Ok(None)
    }

    pub fn make_tuple_init_value<'b, 'c>(&self,
        type_list: &Vec<Rc<Type>>,
        init_pos: &Position,
        init_value_list: &Vec<Box<ExprAST>>,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<StructValue<'ctx>, Box<dyn Error>> {

        let target_len = type_list.len();
        let init_len = init_value_list.len();
        if target_len < init_len {
            return  Err(Box::new(CodeGenError::initial_list_is_too_long(init_pos.clone())));
        }

        let mut vec = Vec::new();
        for i in 0..target_len {
            let field_type = &type_list[i];

            if i < init_len {
                let init_value = &init_value_list[i];
                let init_type = TypeUtil::get_type(&init_value, env)?;
                if **field_type != *init_type {
                    return Err(Box::new(CodeGenError::mismatch_initializer_type(&field_type, &init_type, init_value.get_position().clone())));
                }

                let compiled_val = self.gen_expr(&**init_value, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::mismatch_initializer_type(&field_type, &init_type, init_value.get_position().clone()))?;
                let value = self.try_as_basic_value(&compiled_val.get_value(), init_value.get_position())?;
                vec.push(value);

            }else{  // zero clear
                let zero_value = self.const_zero(&field_type, init_pos)?;
                vec.push(zero_value);
            }
        }

        let values = self.context.const_struct(&vec, false);
        Ok(values)
    }

    pub fn gen_initializer<'b, 'c>(&self,
        init: &Initializer,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<AnyValueEnum<'ctx>, Box<dyn Error>> {
        match init {
            Initializer::Simple(expr, pos) => {
                if let Some(v) = self.gen_expr(expr, env, break_catcher, continue_catcher)? {
                    Ok(v.get_value())
                }else{
                    Err(Box::new(CodeGenError::initializer_is_none(pos.clone())))
                }
            },
            Initializer::Array(vec_init, typ, pos) => {
                // let mut list = Vec::new();

                self.gen_array_initializer(vec_init, typ, pos, env, break_catcher, continue_catcher)
            },
            Initializer::Struct(vec_init, _typ, _pos) => {
                let mut list = Vec::new();
                for init_value in vec_init {
                    let compiled_val = self.gen_initializer(init_value, env, break_catcher, continue_catcher)?;
                    let value = self.try_as_basic_value(&compiled_val, init_value.get_position())?;

                    list.push(value);
                }

                let values = self.context.const_struct(&list, false);
                let any_val = values.as_any_value_enum();
                Ok(any_val)
            }
        }
    }

    pub fn gen_const_initializer<'b, 'c>(&self,
        init: &ConstInitializer,
        env: &Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<AnyValueEnum<'ctx>, Box<dyn Error>> {
        match init {
            ConstInitializer::Simple(expr, _pos) => {
                let basic_value = self.const_expr_to_basic_value_enum(expr);
                let any_value = basic_value.as_any_value_enum();
                Ok(any_value)
            },
            ConstInitializer::Array(vec_init, typ, pos) => {
                self.gen_array_initializer(vec_init, typ, pos, env, break_catcher, continue_catcher)
            },
            ConstInitializer::Struct(vec_init, _typ, _pos) => {
                let mut list = Vec::new();
                for init_value in vec_init {
                    let compiled_val = self.gen_const_initializer(init_value, env, break_catcher, continue_catcher)?;
                    let value = self.try_as_basic_value(&compiled_val, init_value.get_position())?;

                    list.push(value);
                }

                let values = self.context.const_struct(&list, false);
                let any_val = values.as_any_value_enum();
                Ok(any_val)
            }
        }
    }

    fn gen_array_initializer<'b, 'c>(&self,
        vec_init: &Vec<Box<ArrayInitializer>>,
        typ: &Rc<Type>,
        pos: &Position,
        env: &Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<AnyValueEnum<'ctx>, Box<dyn Error>> {

        let llvm_type = TypeUtil::to_basic_type_enum(typ, &self.context, pos)?;
        let array_type = llvm_type.array_type(vec_init.len() as u32);

        let mut list = Vec::new();
        for init_value in vec_init {
            let const_value = init_value.get_const().unwrap();
            let compiled_val = self.gen_const_initializer(const_value, env, break_catcher, continue_catcher)?;
            let value = self.try_as_basic_value(&compiled_val, init_value.get_position())?;
            let array_value = unsafe { ArrayValue::new(value.as_value_ref()) };

            list.push(array_value);
        }

        let any_val = array_type.const_array(list.as_slice());
        Ok(any_val.as_any_value_enum())
    }

    pub fn const_zero(&self, typ: &Type, pos: &Position) -> Result<BasicValueEnum, Box<dyn Error>> {
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
                let struct_type = t.into_struct_type();
                Ok(struct_type.const_zero().as_basic_value_enum())
            },
            BasicMetadataTypeEnum::VectorType(_) => {
                let v_type = t.into_vector_type();
                Ok(v_type.const_zero().as_basic_value_enum())
            },
            BasicMetadataTypeEnum::ArrayType(_) => {
                let ary_type = t.into_array_type();
                Ok(ary_type.const_zero().as_basic_value_enum())
            },
            _ => {
                Err(Box::new(CodeGenError::cannot_get_zero_value(pos.clone())))
            },
        }
    }

    pub fn gen_call_function<'b, 'c>(&self,
        name: &str,
        args: &Vec<BasicMetadataValueEnum<'ctx>>,
        env: &Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        _continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
   ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {
        if let Some((fn_typ, function)) = env.get_function(name) {
            let call_site_value = self.builder.build_call(*function, args, &format!("call_function_{}", name))?;

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

    pub fn gen_call_member_function<'b, 'c>(&self,
        class_name: &str,
        name: &str,
        args: &Vec<BasicMetadataValueEnum<'ctx>>,
        env: &Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        _continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {
        if let Some((fn_typ, function)) = env.get_member_function(class_name, name) {
            let call_site_value = self.builder.build_call(*function, args, &format!("call_member_function_{name}_in_{class_name}"))?;

            let tried = call_site_value.try_as_basic_value();
            if tried.is_left() {  // BasicValueEnum
                let any_val = tried.left().unwrap().as_any_value_enum();
                Ok(Some(CompiledValue::new(fn_typ.get_return_type().clone(), any_val)))

            }else{                 // InstructionValue
                Ok(Some(CompiledValue::new(fn_typ.get_return_type().clone(), AnyValueEnum::InstructionValue(tried.right().unwrap()))))
            }

        }else{
            return Err(Box::new(CodeGenError::no_such_a_member_function(class_name.to_string(), name.to_string(), pos.clone())));
        }
    }

    pub fn gen_call_class_function<'b, 'c>(&self,
        class_name: &str,
        name: &str,
        args: &Vec<BasicMetadataValueEnum<'ctx>>,
        env: &Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        _continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {

        if let Some((fn_typ, function)) = env.get_class_function(class_name, &name) {
            let call_site_value = self.builder.build_call(*function, &args, &format!("call_class_function_{name}_in_{class_name}"))?;

            let tried = call_site_value.try_as_basic_value();
            if tried.is_left() {  // BasicValueEnum
                let any_val = tried.left().unwrap().as_any_value_enum();
                Ok(Some(CompiledValue::new(fn_typ.get_return_type().clone(), any_val)))

            }else{                 // InstructionValue
                Ok(Some(CompiledValue::new(fn_typ.get_return_type().clone(), AnyValueEnum::InstructionValue(tried.right().unwrap()))))
            }

        }else{
            return Err(Box::new(CodeGenError::no_such_a_class_function(class_name.to_string(), name.to_string(), pos.clone())));
        }
    }

    fn str_to_basic_metadata_value_enum(&self, s: &str, pos: &Position) -> Result<BasicMetadataValueEnum<'ctx>, Box<dyn Error>> {
        let global_str = self.builder.build_global_string_ptr(s, &format!("global_str_{}", s))?.as_any_value_enum();
        Ok(self.try_as_basic_metadata_value(&global_str, pos)?)
    }

    #[inline]
    pub fn gen_implicit_cast(&self, value: &AnyValueEnum<'ctx>, from_type: &Type, to_type: &Type, pos: &Position) -> Result<AnyValueEnum<'ctx>, Box<dyn Error>> {
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
            let ret_type = Rc::new(Type::Number(NumberType::Int));

            let sq = SpecifierQualifier::new();
            let ds = DeclarationSpecifier { typ: Type::Number(NumberType::Char).into(), specifier_qualifier: sq };

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
        defines: &'ctx Vec<ImplElement>,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<(), Box<dyn Error>> {

        let class = env.intern_class(class_name) as *const Class;

        env.set_current_class(class);

        if for_type.is_some() {
            self.gen_impl_for(class_name, typ, for_type, defines, env, break_catcher, continue_catcher)?
        }else{
            self.gen_impl_no_for(class_name, typ, defines, env, break_catcher, continue_catcher, pos)?
        }

        env.remove_current_class();

        Ok(())
    }

    fn gen_impl_no_for<'b, 'c>(
        &self,
        class_name: &str,
        typ: &Type,
        // functions: &'ctx Vec<FunOrProt>,
        defines: &'ctx Vec<ImplElement>,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<(), Box<dyn Error>> {

        for def in defines {
            match def {
                ImplElement::FunOrProt(f_or_p) => {
                    self.gen_impl_function(class_name, typ, f_or_p, env, break_catcher, continue_catcher, pos)?;
                },
                ImplElement::DefVar { specifiers, declaration } => {
                    self.gen_impl_def_var(class_name, typ, specifiers, declaration, env, break_catcher, continue_catcher, pos)?;
                },
            }
        }

        Ok(())
    }

    #[allow(unused_variables)]
    fn gen_impl_for<'b, 'c>(
        &self,
        class_name: &str,
        typ: &Type,
        for_type: &Option<String>,
        // functions: &Vec<FunOrProt>,
        defines: &'ctx Vec<ImplElement>,
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
        fun_or_proto: &'ctx FunOrProt,
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
        let fun_name = if params.has_self() {
            Self::make_function_name_in_impl(class_name, decl_name)
        }else{
            Self::make_class_function_name_in_impl(class_name, decl_name)
        };
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

        if params.has_self() {
            env.insert_member_function(class_name, &decl_name, fun_type, function, pos)?;
        }else{
            env.insert_class_function(class_name, &decl_name, fun_type, function, pos)?;
        }

        Ok(Some(AnyValueEnum::FunctionValue(function)))
    }

    fn gen_impl_def_var<'b, 'c>(
        &self,
        class_name: &str,
        _typ: &Type,
        specifier: &DeclarationSpecifier,
        declarations: &Vec<Declaration>,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<(), Box<dyn Error>> {

        let base_type = specifier.get_type();
        let sq = specifier.get_specifier_qualifier();

        for decl in declarations {
            let declarator = decl.get_declarator();
            let var_name = declarator.get_name();
            let global_name = Self::make_var_name_in_impl(class_name, var_name);

            let typ = declarator.make_type(base_type);
            let basic_type = TypeUtil::to_basic_type_enum(&typ, self.context, pos)?;
            let ptr = self.module.add_global(basic_type, Some(AddressSpace::default()), &global_name);

            match decl.get_init_expr() {
                Some(initializer) => {
                    match typ.as_ref() {
                        Type::Struct { fields, .. } => {
                            self.gen_global_struct_init(&fields, ptr, &*initializer, env, break_catcher, continue_catcher)?;
                        },
                        Type::Array { name: _, typ, size_list } => {
                            self.gen_global_array_init(&size_list, ptr, &*typ, &*initializer, env, break_catcher, continue_catcher)?;
                        },
                        _ => {
                            let init = self.gen_initializer(initializer, env, break_catcher, continue_catcher)?;
                            let basic_value = self.try_as_basic_value(&init, initializer.get_position())?;
    
                            ptr.set_initializer(&basic_value);
                        }
                    }
                },
                None => (),  // do nothing
            };

            env.insert_class_var(class_name, var_name, typ.clone(), sq.clone(), ptr, pos)?;
        }

        Ok(())
    }

    fn gen_global_def_var_sub<'b, 'c>(
        &self,
        name: &str,
        base_type: &Rc<Type>,
        sq: &SpecifierQualifier,
        decl: &Declaration,
        declarator: &Declarator,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<(), Box<dyn Error>> {

        let typ = declarator.make_type(base_type);
        let basic_type = TypeUtil::to_basic_type_enum(&typ, self.context, pos)?;
        let ptr = self.module.add_global(basic_type, Some(AddressSpace::default()), &name);

        match decl.get_init_expr() {
            Some(initializer) => {
                match typ.as_ref() {
                    Type::Struct { fields, type_variables, .. } => {
                        if let Some(variables) = type_variables {
                            self.gen_global_init_type_variables(&variables, ptr, &*initializer, env, break_catcher, continue_catcher)?;
                        }
                        self.gen_global_struct_init(&fields, ptr, &*initializer, env, break_catcher, continue_catcher)?;
                    },
                    Type::Array { typ, size_list, .. } => {
                        self.gen_global_array_init(&size_list, ptr, &*typ, &*initializer, env, break_catcher, continue_catcher)?;
                    },
                    Type::Enum { name: _, enum_def: _, type_variables } => {
                        if let Some(variables) = type_variables {
                            self.gen_global_init_type_variables(&variables, ptr, &*initializer, env, break_catcher, continue_catcher)?;
                        }

                        unimplemented!()
                    },
                    Type::Tuple(type_list) => {
                        self.gen_global_tuple_init(&type_list, ptr, &*initializer, env, break_catcher, continue_catcher)?;
                    },
                    _ => {
                        let init = self.gen_initializer(initializer, env, break_catcher, continue_catcher)?;
                        let basic_value = self.try_as_basic_value(&init, initializer.get_position())?;

                        ptr.set_initializer(&basic_value);
                    }
                }
            },
            None => (),  // do nothing
        };

        env.insert_global_var(&name, typ.clone(), sq.clone(), ptr);

        Ok(())
    }

    fn gen_global_init_type_variables<'b, 'c>(
        &self,
        _variables: &Vec<String>,
        _target_ptr: GlobalValue<'ctx>,
        _init: &Initializer,
        _env: &mut Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        _continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<(), Box<dyn Error>> {

        // let init_value_list = if let Initializer::Simple(ExprAST::TupleLiteral(expr_list, _pos), _pos2) = init {
        //     expr_list
        // }else{
        //     return Err(Box::new(CodeGenError::initializer_is_not_tuple(init.get_position().clone())));
        // };

        // let target_len = fields.len();
        // let init_len = init_value_list.len();
        // if target_len < init_len {
        //     return  Err(Box::new(CodeGenError::initial_list_is_too_long(init.get_position().clone())));
        // }

        // let mut vec = Vec::new();
        // for i in 0..target_len {
        //     let field = &fields[i];
        //     let typ = &field.get_type();
        //     let var = &variables[i];

        //     if i < init_len {
        //         let init_value = &init_value_list[i];
        //         let init_type = TypeUtil::get_type(&init_value, env)?;
        //         if **typ != *init_type {
        //             return Err(Box::new(CodeGenError::mismatch_initializer_type(&typ, &init_type, init_value.get_position().clone())));
        //         }

        //         let compiled_val = self.gen_expr(&**init_value, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::mismatch_initializer_type(&typ, &init_type, init_value.get_position().clone()))?;
        //         let value = self.try_as_basic_value(&compiled_val.get_value(), init_value.get_position())?;
        //         vec.push(value);

        //     }else{  // zero clear
        //         let zero_value = self.const_zero(&typ, init.get_position())?;
        //         vec.push(zero_value);
        //     }
        // }

        // let values = self.context.const_struct(&vec, false);
        // target_ptr.set_initializer(&values.as_basic_value_enum());

        // Ok(())

        unimplemented!("gen_global_init_type_variables")
    }

    pub fn make_function_name_in_impl(class_name: &str, fun_name: &str) -> String {
        format!("${}.{}", class_name, fun_name)
    }

    pub fn make_class_function_name_in_impl(class_name: &str, fun_name: &str) -> String {
        format!("${}::{}", class_name, fun_name)
    }

    pub fn make_var_name_in_impl(class_name: &str, var_name: &str) -> String {
        format!("${}@@{}", class_name, var_name)
    }

    pub fn size_of(basic_type: &BasicTypeEnum) -> Result<u64, Box<dyn Error>> {
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
        let const_one = i32_type.const_int(1, false);
        let ptr = unsafe { builder.build_in_bounds_gep(array_type, array_ptr, &[const_one], "gep for array")? };
    
        let base = array_ptr.const_to_int(i32_type);
        let index1 = ptr.const_to_int(i32_type);
        let size = builder.build_int_sub(index1, base, "sub")?;
        let _tmp = builder.build_return(Some(&size));

        let f = unsafe { engine.get_function(fn_name) };
        let f: JitFunction<FuncVoidU64> = f.ok().unwrap();
        let result: u64 = unsafe { f.call() };

        Ok(result)
    }

    pub fn bit_size_check(opt_type: &Option<Rc<Type>>, size: usize, pos: &Position) -> Result<(), CodeGenError> {
        if let Some(rc_type) = opt_type {
            if let Type::Number(typ) = rc_type.as_ref() {
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
        }

        Ok(())
    }

    fn make_fun_type(name: &str, ret_type: &Rc<Type>, params: &Params, _env: &Env) -> Result<CustFunctionType, CodeGenError> {
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

    pub fn try_as_basic_metadata_value(&self, any_val: &AnyValueEnum<'ctx>, pos: &Position) -> Result<BasicMetadataValueEnum<'ctx>, CodeGenError> {
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

    pub fn try_as_basic_value(&self, any_val: &AnyValueEnum<'ctx>, pos: &Position) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
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
        ret_type: &Rc<Type>,
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
        let cust_fn_type = CustFunctionType::new(Some(fn_name.to_string()), Rc::clone(ret_type), params.get_params_type(), has_variadic);
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
            let sq = SpecifierQualifier::default();
            env.insert_local(name, typ.clone(), sq, ptr);
        }

        // 引数の処理
        for (i, param) in params.get_params().iter().enumerate() {
            let typ = param.get_type();
            let name = param.get_name();
            let sq = param.get_declaration_specifier().get_specifier_qualifier();
            let ptr = self.builder.build_alloca(TypeUtil::to_basic_type_enum(&typ, self.context, param.get_position())?, name)?;
            let value = function.get_nth_param(i as u32).unwrap();
            self.builder.build_store(ptr, value)?;
            env.insert_local(name, typ, sq.clone(), ptr);
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
                    let typ: Rc<Type> = TypeUtil::get_type(expr, env)?;
                    return Err(Box::new(CodeGenError::return_type_mismatch(fn_type.get_return_type().as_ref().clone(), typ.as_ref().clone(), pos.clone())));
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
                        Err(Box::new(CodeGenError::return_type_mismatch(ret_type.as_ref().clone(), Type::Void, pos.clone())))
                    }else{
                        if *ret_type == typ {
                            // 最後の文が、ifのとき、ラベルif.endの後にコードが生成されないのでセグフォが起きることへのケア
                            // self.builder.build_return(None)?;
                            self.builder.build_unreachable()?;

                            Ok(())
                        }else{
                            Err(Box::new(CodeGenError::return_type_mismatch(ret_type.as_ref().clone(), typ.as_ref().clone(), pos.clone())))
                        }

                    }
                },
                Some(AST::IfLet { pattern_list, pattern_name, expr, then, else_, pos }) => {  // if let pattern_list @ pattern_name
                    let typ = self.calc_ret_type_in_if_let(then, else_, pattern_list, pattern_name, expr, pos, env)?;
                    if typ.is_void() {
                        Err(Box::new(CodeGenError::return_type_mismatch(ret_type.as_ref().clone(), Type::Void, pos.clone())))
                    }else{
                        if *ret_type == typ {
                            // 最後の文が、ifのとき、ラベルif.endの後にコードが生成されないのでセグフォが起きることへのケア
                            // self.builder.build_return(None)?;
                            self.builder.build_unreachable()?;

                            Ok(())
                        }else{
                            Err(Box::new(CodeGenError::return_type_mismatch(ret_type.as_ref().clone(), typ.as_ref().clone(), pos.clone())))
                        }

                    }
                },
                Some(AST::Switch(_switch, pos)) => {
                    let typ = self.calc_ret_type(last_stmt.unwrap(), env)?;

                    if *ret_type == typ {
                        Ok(())
                    }else{
                        Err(Box::new(CodeGenError::return_type_mismatch(ret_type.as_ref().clone(), typ.as_ref().clone(), pos.clone())))
                    }
                },
                Some(stmt) => {  // TODO: if文などの時に内部でreturnしているかもしれないので、その処理。
                    let typ = self.calc_ret_type(stmt, env)?;
                    if typ.is_void() {
                        Err(Box::new(CodeGenError::return_type_mismatch(ret_type.as_ref().clone(), Type::Void, stmt.get_position().clone())))
                    }else{
                        if *ret_type == typ {
                            Ok(())
                        }else{
                            Err(Box::new(CodeGenError::return_type_mismatch(ret_type.as_ref().clone(), typ.as_ref().clone(), stmt.get_position().clone())))
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

    fn calc_ret_type(&self, stmt: &AST, env: &mut Env<'ctx>) -> Result<Rc<Type>, Box<dyn Error>> {
        match stmt {
            AST::Return(None, _pos) => {
                Ok(Rc::new(Type::Void))
            },
            AST::Return(Some(expr), _pos) => {
                let typ = TypeUtil::get_type(&**expr, env)?;
                Ok(typ)
            },
            AST::If(_cond, if_then, if_else, pos) => {
                self.calc_ret_type_in_if(if_then, if_else, pos, env)
            },
            AST::IfLet { pattern_list, pattern_name, expr, then, else_, pos } => {
                self.calc_ret_type_in_if_let(then, else_, pattern_list, pattern_name, expr, pos, env)
            }
            AST::Block(blk, _pos) => {
                let mut typ = Rc::new(Type::Void);

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
            AST::Switch(switch, _pos1) => {
                let opt_stmt = switch.get_stmt();
                if let Some(stmt) = opt_stmt {
                    if let AST::Block(blk, _pos2) = stmt {
                        // let mut typ = Rc::new(Type::Void);
                        let mut typ = None;

                        for e in &blk.body {
                            match e {
                                AST::Return(Some(boxed_ast), _) => {
                                    let t = TypeUtil::get_type(&**boxed_ast, env)?;

                                    if typ.is_none() {
                                        typ = Some(t);
                                    }else{
                                        if typ.as_ref().unwrap() != &t {
                                            return Err(Box::new(CodeGenError::return_type_mismatch(typ.as_ref().unwrap().as_ref().clone(), t.as_ref().clone(), boxed_ast.get_position().clone())));
                                        }
                                    }
                                },
                                AST::Case(case, _pos3) => {
                                    let stmt = case.get_stmt();
                                    let typ2 = self.calc_ret_type(stmt, env)?;

                                    if typ.is_none() {
                                        typ = Some(typ2);
                                    }else{
                                        if typ.as_ref().unwrap() != &typ2 {
                                            return Err(Box::new(CodeGenError::return_type_mismatch(typ.as_ref().unwrap().as_ref().clone(), typ2.as_ref().clone(), case.get_position().clone())));
                                        }
                                    }
                                },
                                _ => (),  // do nothing
                            }
                        }

                        if let Some(typ) = typ {
                            return Ok(typ);
                        }else{
                            return Ok(Rc::new(Type::Void));
                        }
                    }
                }

                Ok(Rc::new(Type::Void))
            },
            _ => {
                Ok(Rc::new(Type::Void))
            },
        }
    }

    fn calc_ret_type_in_if(&self, if_then: &AST, if_else: &Option<Box<AST>>, pos: &Position, env: &mut Env<'ctx>) -> Result<Rc<Type>, Box<dyn Error>> {
        let then_type = self.calc_ret_type(if_then, env)?;

        if then_type.is_void() {
            if let Some(else_expr) = if_else {
                let else_type = self.calc_ret_type(else_expr, env)?;
                if else_type.is_void() {
                    Ok(Rc::new(Type::Void))
                }else{
                    Err(Box::new(CodeGenError::mismatch_type_in_if(pos.clone(), then_type.as_ref().clone(), else_type.as_ref().clone())))
                }

            }else{
                Ok(Rc::new(Type::Void))
            }

        }else{
            if let Some(else_expr) = if_else {
                let else_type = self.calc_ret_type(else_expr, env)?;
                if else_type.is_void() {
                    Err(Box::new(CodeGenError::mismatch_type_in_if(pos.clone(), then_type.as_ref().clone(), else_type.as_ref().clone())))
                }else{
                    Ok(else_type)
                }

            }else{
                Err(Box::new(CodeGenError::mismatch_type_in_if(pos.clone(), then_type.as_ref().clone(), Type::Void)))
            }
        }
    }

    fn calc_ret_type_in_if_let(&self, if_then: &AST, if_else: &Option<Box<AST>>, pattern_list: &Vec<(Box<Pattern>, Position)>, pattern_name: &Option<String>, expr: &ExprAST, pos: &Position, env: &mut Env<'ctx>) -> Result<Rc<Type>, Box<dyn Error>> {
        let result;

        env.add_new_local_types();

        for (pattern, _pos) in pattern_list {
            let expr_type = TypeUtil::get_type(expr, env)?;
            self.insert_pat_type(pattern, pattern_name, &expr_type, env)?;
        }

        let then_type = self.calc_ret_type(if_then, env)?;

        if then_type.is_void() {
            if let Some(else_expr) = if_else {
                let else_type = self.calc_ret_type(else_expr, env)?;
                if else_type.is_void() {
                    result = Ok(Rc::new(Type::Void));
                }else{
                    result = Err(Box::new(CodeGenError::mismatch_type_in_if(pos.clone(), then_type.as_ref().clone(), else_type.as_ref().clone())));
                }

            }else{
                result = Ok(Rc::new(Type::Void));
            }

        }else{
            if let Some(else_expr) = if_else {
                let else_type = self.calc_ret_type(else_expr, env)?;
                if else_type.is_void() {
                    result = Err(Box::new(CodeGenError::mismatch_type_in_if(pos.clone(), then_type.as_ref().clone(), else_type.as_ref().clone())));
                }else{
                    result = Ok(else_type);
                }

            }else{
                result = Err(Box::new(CodeGenError::mismatch_type_in_if(pos.clone(), then_type.as_ref().clone(), Type::Void)));
            }
        }

        env.remove_local_types();

        Ok(result?)
    }

    fn insert_pat_type(&self, pattern: &Box<Pattern>, pattern_name: &Option<String>, expr_type: &Rc<Type>, env: &mut Env<'ctx>) -> Result<(), Box<dyn Error + 'static>> {
        match &**pattern {
            Pattern::Var(name) => {
                env.insert_local_type(name, Rc::clone(&expr_type));
    
                if let Some(pat_name) = pattern_name {
                    env.insert_local_type(&pat_name, Rc::clone(expr_type));
                }
            },
            Pattern::Char(_) | Pattern::CharRange(_, _) => {
                let typ = Type::new_number_type(NumberType::Char);
                if let Some(pat_name) = pattern_name {
                    env.insert_local_type(&pat_name, Rc::new(typ));
                }
            },
            Pattern::Number(_) | Pattern::NumberRange(_, _) => {
                if let Some(pat_name) = pattern_name {
                    env.insert_local_type(&pat_name, Rc::clone(expr_type));
                }
            },
            Pattern::Str(_) => {
                let typ = Type::new_pointer_type(Rc::new(Type::new_number_type(NumberType::Char)), false, false);
                if let Some(pat_name) = pattern_name {
                    env.insert_local_type(&pat_name, Rc::new(typ));
                }
            },
            Pattern::Enum(enum_pat) => {
                match enum_pat {
                    // Name::SubName
                    EnumPattern::Simple(_typ, _name, _sub_name) => {
                        // do nothing
                    },
                    // Name::SubName(pattern1 @ pat_name, pattern2, ...)
                    EnumPattern::Tuple(_typ, _name, _sub_name, _pat_list) => {
                        // do nothing
                    },
                    // Name::SubName { field1: struct_pattern1, field2: struct_pattern2, ... }
                    EnumPattern::Struct(_typ, _name, sub_name, struct_pat) => {
                        let e_type = expr_type.get_enum_sub_type_by_name(sub_name).unwrap();
                        self.insert_struct_pat_type(e_type, env, struct_pat)?;
                    },
                }
            },
            Pattern::Struct(struct_pat) => {
                self.insert_struct_pat_type(expr_type, env, struct_pat)?;
            },
            Pattern::Tuple(pattern_list) => {
                for i in 0..pattern_list.len() {
                    let (pat_list, opt_name) = &pattern_list[i];
                    let (pat, _pos) = &pat_list[0];
                    let e_type = expr_type.get_tuple_type_at_index(i).unwrap();

                    match &**pat {
                        Pattern::Var(name) => {
                            env.insert_local_type(name, Rc::clone(e_type));
                        },
                        _ => {



                            unimplemented!()
                        },
                    }
        
                    self.insert_pat_type(pat, opt_name, e_type, env)?;
                }
            }
        }

        Ok(())
    }

    fn insert_struct_pat_type(&self, expr_type: &Rc<Type>, env: &mut Env<'ctx>, struct_pat: &parser::StructPattern) -> Result<(), Box<dyn Error + 'static>> {
        let pattern_map = struct_pat.get_map();
        let key_list = struct_pat.get_keys();

        for key in key_list {
            let pat = pattern_map.get(key).unwrap();
    
            if let Some((pat_list, opt_name)) = pat {
                let (pat, _pos) = &pat_list[0];
                let e_type = expr_type.get_field_type_by_name(&key).unwrap();

                match &**pat {
                    Pattern::Var(name) => {
                        env.insert_local_type(name, Rc::clone(e_type));
                    },
                    _ => {



                        unimplemented!()
                    },
                }
    
                self.insert_pat_type(pat, opt_name, e_type, env)?;
            }
        }

        Ok(())
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

use crate::parser::{ExprAST, Type, StructDefinition, StructField};
use crate::parser::{Initializer, StructLiteral};
use super::{CompiledValue, CodeGenError};
use super::Env;
use super::env::{BreakCatcher, ContinueCatcher};
use super::type_util::TypeUtil;
use crate::Position;
use crate::CodeGen;

use inkwell::context::Context;
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, GlobalValue, PointerValue, StructValue};
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum};
use inkwell::types::AnyType;
use inkwell::types::StructType;
use inkwell::AddressSpace;
use std::error::Error;
use std::collections::HashMap;
use std::rc::Rc;

impl<'ctx> CodeGen<'ctx> {
    pub fn gen_struct_literal<'b, 'c>(&self,
        struct_literal: &StructLiteral,
        struct_ptr: PointerValue<'ctx>,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {
        match struct_literal {
            StructLiteral::NormalLiteral(typ, map, pos) => {
                let struct_name = typ.get_type_name();

                if let Some(fields) = typ.get_struct_fields() {
                    let i32_type = self.context.i32_type();
                    let const_zero = i32_type.const_int(0, false);

                    let mut index = 0;
                    for field in fields {
                        let name = field.get_name().as_ref().unwrap();
                        let expr_ast = map.get(name).unwrap();
                        let any_value = self.gen_expr(expr_ast, env, break_catcher, continue_catcher)?.unwrap().get_value();
                        let basic_value = BasicValueEnum::try_from(any_value).map_err(|_e| CodeGenError::system_error(pos.clone()))?;

                        let const_index = i32_type.const_int(index, false);
                        let indexes = vec![const_zero, const_index];
                        let (struct_type, _index_map) = Self::struct_from_fields(&None, fields, &self.context, pos)?;
                        let struct_ptr_ty = struct_type.ptr_type(AddressSpace::default());
                        let ptr = unsafe { self.builder.build_in_bounds_gep(struct_ptr_ty, struct_ptr, &indexes, "gep_for_struct_field")? };
                        let _result = self.builder.build_store(ptr, basic_value);

                        index += 1;
                    }
                }

                let struct_ptr_ty = TypeUtil::to_basic_type_enum(typ, &self.context, pos)?;
                let basic_val = self.builder.build_load(struct_ptr_ty, struct_ptr, &format!("load_struct_{}_literal", struct_name))?;
                let any_val = basic_val.as_any_value_enum();
                Ok(Some(CompiledValue::new(typ.clone(), any_val)))
            },
            StructLiteral::ConstLiteral(typ, const_map, pos) => {
                let struct_name = typ.get_type_name();

                let mut vec = Vec::new();
                let struct_ptr_ty;
                if let Some(fields) = typ.get_struct_fields() {
                    for field in fields {
                        let name = field.get_name().as_ref().unwrap();
                        let const_expr = const_map.get(name).unwrap();
                        let basic_value = self.const_expr_to_basic_value_enum(const_expr, self.context);
                        vec.push(basic_value);
                    }

                    let values = self.context.const_struct(&vec, false);
                    let _result = self.builder.build_store(struct_ptr, values.as_basic_value_enum());

                    let (struct_type, _index_map) = Self::struct_from_fields(&None, fields, &self.context, pos)?;
                    struct_ptr_ty = struct_type.ptr_type(AddressSpace::default());
                }else{
                    panic!()
                }

                let basic_val = self.builder.build_load(struct_ptr_ty, struct_ptr, &format!("load_struct_{}_literal", struct_name))?;
                let any_val = basic_val.as_any_value_enum();
                Ok(Some(CompiledValue::new(typ.clone(), any_val)))
            },
        }        
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
                    return Err(Box::new(CodeGenError::mismatch_initializer_type(field_type, &init_type, init_value.get_position().clone())));
                }

                let compiled_val = self.gen_const_expr(init_value, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::mismatch_initializer_type(field_type, &init_type, init_value.get_position().clone()))?;
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
            return Err(Box::new(CodeGenError::initializer_is_not_struct(init.get_position().clone())));
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
        typ: &Type,
        target_fields: &StructDefinition,
        target_struct_ptr: PointerValue<'ctx>,
        init: &Initializer,
        env: &mut Env<'ctx>,
        break_catcher: Option<&'b BreakCatcher>,
        continue_catcher: Option<&'c ContinueCatcher>
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        match init {
            Initializer::Struct(init_value_list, _typ, _pos) => {
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
            },
            Initializer::Simple(expr, _pos) => {
                match expr {
                    ExprAST::CallFunction(fun, args, pos2) => {
                        let mut v: Vec<BasicMetadataValueEnum> = Vec::new();
                        for expr in args {
                            let result = self.gen_expr(expr, env, break_catcher, continue_catcher)?.ok_or(CodeGenError::illegal_end_of_input(pos2.clone()))?;
                            v.push(self.try_as_basic_metadata_value(&result.get_value(), pos2)?);
                        }
        
                        let compiled_value = match &**fun {
                            ExprAST::Symbol(name, _pos2) => {
                                if let Some((fn_typ, _function)) = env.get_function(name) {
                                    let ret_type = fn_typ.get_return_type();
                                    if typ != ret_type.as_ref() {
                                        return Err(Box::new(CodeGenError::initializer_is_not_struct(init.get_position().clone())));
                                    }
                                }else{
                                    return Err(Box::new(CodeGenError::no_such_a_function(&name, pos2.clone())));
                                }

                                self.gen_call_function(name, &v, env, break_catcher, continue_catcher, pos2)?.unwrap()
                            },
                            ExprAST::MemberAccess(ast, fun_name, pos2) => {
                                let typ2 = TypeUtil::get_type(ast, env)?;
                                let class_name = typ2.get_type_name();
                                if let Some((fn_typ, _function)) = env.get_member_function(&class_name, fun_name) {
                                    let ret_type = fn_typ.get_return_type();
                                    if typ != ret_type.as_ref() {
                                        return Err(Box::new(CodeGenError::initializer_is_not_struct(init.get_position().clone())));
                                    }
                                }else{
                                    return Err(Box::new(CodeGenError::no_such_a_member_function(class_name, fun_name.to_string(), pos2.clone())));
                                }

                                let (_t, obj) = self.get_l_value(&**ast, env, break_catcher, continue_catcher)?;

                                let mut args: Vec<BasicMetadataValueEnum> = Vec::new();
                                args.push(obj.into());

                                args.append(&mut v);

                                self.gen_call_member_function(&class_name, &fun_name, &args, env, break_catcher, continue_catcher, pos2)?.unwrap()
                            },
                            ExprAST::StructStaticSymbol(class_name, method_name, pos2) => {
                                if let Some((fn_typ, _function)) = env.get_class_function(class_name, &method_name) {
                                    let ret_type = fn_typ.get_return_type();
                                    if typ != ret_type.as_ref() {
                                        return Err(Box::new(CodeGenError::initializer_is_not_struct(init.get_position().clone())));
                                    }
                                }else{
                                    return Err(Box::new(CodeGenError::no_such_a_class_function(class_name.to_string(), method_name.to_string(), pos2.clone())));
                                }

                                self.gen_call_class_function(&class_name, &method_name, &v, env, break_catcher, continue_catcher, pos2)?.unwrap()
                            },
                            _ => {
                                return Err(Box::new(CodeGenError::not_function(&format!("{:?}", fun), pos2.clone())));
                            }
                        };

                        let _result = self.builder.build_store(target_struct_ptr, compiled_value.get_value().into_struct_value().as_basic_value_enum());
                    },
                    ExprAST::StructLiteral(struct_literal) => {
                        match struct_literal {
                            StructLiteral::NormalLiteral(typ2, map, pos2) => {
                                if typ != typ2.as_ref() {
                                    return Err(Box::new(CodeGenError::mismatch_initializer_type(typ, typ2, pos2.clone())));
                                }
        
                                if let Some(fields) = typ.get_struct_fields() {
                                    let i32_type = self.context.i32_type();
                                    let const_zero = i32_type.const_int(0, false);
        
                                    let mut index = 0;
        
                                    for field in fields {
                                        let name = field.get_name().as_ref().unwrap();
                                        let expr_ast = map.get(name).unwrap();
                                        let any_value = self.gen_expr(expr_ast, env, break_catcher, continue_catcher)?.unwrap().get_value();
                                        let basic_value = BasicValueEnum::try_from(any_value).map_err(|_e| CodeGenError::system_error(pos2.clone()))?;
        
                                        let const_index = i32_type.const_int(index, false);
                                        let indexes = vec![const_zero, const_index];

                                        let (struct_type, _index_map) = Self::struct_from_fields(&None, fields, &self.context, pos2)?;
                                        let struct_ptr_ty = struct_type.ptr_type(AddressSpace::default());
                                        let ptr = unsafe { self.builder.build_in_bounds_gep(struct_ptr_ty, target_struct_ptr, &indexes, "gep_for_struct_field")? };
                                        let _result = self.builder.build_store(ptr, basic_value);
        
                                        index += 1;
                                    }
                                }
                            },
                            StructLiteral::ConstLiteral(typ2, const_map, pos2) => {
                                if typ != typ2.as_ref() {
                                    return Err(Box::new(CodeGenError::mismatch_initializer_type(typ, typ2, pos2.clone())));
                                }
        
                                let mut vec = Vec::new();
                                if let Some(fields) = typ.get_struct_fields() {
                                    for field in fields {
                                        let name = field.get_name().as_ref().unwrap();
                                        let const_expr = const_map.get(name).unwrap();
                                        let basic_value = self.const_expr_to_basic_value_enum(const_expr, self.context);
                                        vec.push(basic_value);
                                    }
        
                                    let values = self.context.const_struct(&vec, false);
                                    let _result = self.builder.build_store(target_struct_ptr, values.as_basic_value_enum());
                                }
                            },
                        }
                    },
                    _ => {
                        return Err(Box::new(CodeGenError::initializer_is_not_struct(init.get_position().clone())));
                    }
                }
            },
            _ => {
                return Err(Box::new(CodeGenError::initializer_is_not_struct(init.get_position().clone())));
            },
        };

        Ok(None)
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
            return Self::struct_from_fields(name, fields, ctx, pos);
        }

        Ok((ctx.struct_type(&list, packed), index_map))
    }

    fn struct_from_fields(name: &Option<String>, fields: &Vec<StructField>, ctx: &'ctx Context, pos: &Position) -> Result<(StructType<'ctx>, HashMap<String, usize>), Box<dyn Error>> {
        let mut list: Vec<BasicTypeEnum<'ctx>> = Vec::new();
        let mut packed = false;
        let mut index_map: HashMap<String, usize> = HashMap::new();
        let mut index = 0;

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
      -> Result<(Vec<(Rc<Type>, BasicTypeEnum<'ctx>)>, HashMap<String, usize>, u64, Option<BasicTypeEnum<'ctx>>), Box<dyn Error>>
    {
        let mut list: Vec<(Rc<Type>, BasicTypeEnum<'ctx>)> = Vec::new();
        let mut index_map: HashMap<String, usize> = HashMap::new();
        let mut index = 0;
        let mut max_size = 0;
        let mut max_size_type: Option<BasicTypeEnum> = None;
 
        if let Some(fields) = struct_def.get_fields() {
            for field in fields {
                match field {
                    StructField::NormalField { name: field_name, sq: _, typ } => {
                        let t = TypeUtil::to_basic_type_enum(typ, ctx, pos)?;
                        list.push((Rc::clone(typ), t.clone()));

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
                            list.push((Rc::clone(typ2), t.clone()));
                        }else{
                            list.push((Rc::new(Type::BitField), t.clone()))
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

}

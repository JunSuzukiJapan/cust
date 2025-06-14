use crate::parser::{Type, EnumDefinition, Enumerator, EnumLiteral, Initializer, ExprAST};
use super::{CodeGenError, CompiledValue};
use super::Env;
use super::env::{BreakCatcher, ContinueCatcher};
use super::type_util::TypeUtil;
use crate::Position;
use crate::CodeGen;

use inkwell::context::Context;
use inkwell::values::{AnyValue, AnyValueEnum, BasicValue, GlobalValue, IntValue, StructValue};
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::types::AnyType;
use inkwell::AddressSpace;
use std::error::Error;
use std::collections::HashMap;
use std::rc::Rc;


impl<'ctx> CodeGen<'ctx> {
    pub fn gen_define_enum<'b, 'c>(
        &self,
        enum_name: &str,
        enum_def: &EnumDefinition,
        type_variables: &Option<Vec<String>>,
        env: &mut Env<'ctx>,
        _break_catcher: Option<&'b BreakCatcher>,
        _continue_catcher: Option<&'c ContinueCatcher>,
        pos: &Position
    ) -> Result<Option<AnyTypeEnum<'ctx>>, Box<dyn Error>> {

        match enum_def {
            EnumDefinition::StandardEnum { fields, .. } => {
                let (enumerator_list, index_map) = self.enum_from_enum_standard(fields, type_variables, env)?;
                let i32_type = self.context.i32_type();
                env.insert_const_enum(enum_name.into(), &i32_type, enumerator_list, index_map, pos)?;

                Ok(None)
            },
            EnumDefinition::TaggedEnum { fields, .. } => {
                let (type_list, index_map, max_size, max_size_type) = Self::tagged_enum_from_enum_definition(enum_name, fields, &self.enum_tag_type, type_variables, self.context, pos)?;
                env.insert_tagged_enum(enum_name.into(), type_list, index_map, max_size, max_size_type, pos)?;
        
                if let Some(t) = max_size_type {
                    Ok(Some(t.as_any_type_enum()))
                }else{
                    Ok(None)
                }
            },
        }
    }

    pub fn tagged_enum_from_enum_definition<'b, 'c>(
        _enum_name: &str,
        fields: &Vec<Enumerator>,
        tag_type: &Rc<Type>,
        _type_variables: &Option<Vec<String>>,
        ctx: &'ctx Context,
        pos: &Position
    ) -> Result<(Vec<(Rc<Type>, BasicTypeEnum<'ctx>)>, HashMap<String, usize>, u64, Option<BasicTypeEnum<'ctx>>), Box<dyn Error>> {

        let mut list: Vec<(Rc<Type>, BasicTypeEnum<'ctx>)> = Vec::new();
        let mut index_map: HashMap<String, usize> = HashMap::new();
        let mut index = 0;
        let mut max_size = 0;
        let mut max_size_type: Option<BasicTypeEnum> = None;
 
        let tag_basic_type = TypeUtil::to_basic_type_enum(&tag_type, ctx, pos)?;
        let rc_tag_type = Rc::clone(tag_type);
        let tag_size = Self::size_of(&tag_basic_type)?;

        for field in fields {
            match field {
                Enumerator::Const { name, .. } => {
                    list.push((Rc::clone(&rc_tag_type), tag_basic_type.clone()));

                    let size = tag_size;
                    if size > max_size {
                        max_size = size;
                        max_size_type = Some(tag_basic_type);
                    }

                    if max_size_type.is_none() {
                        max_size = size;
                        max_size_type = Some(tag_basic_type.clone())
                    }

                    index_map.insert(name.clone(), index);
                },
                Enumerator::TypeStruct { name, struct_type } => {
                    let def = struct_type.get_struct_definition().unwrap();
                    let type_variables = struct_type.get_type_variables();

                    let typ = Type::struct_from_struct_definition(Some(name.to_string()), def.clone(), type_variables.clone());
                    let t = TypeUtil::to_basic_type_enum(&typ, ctx, pos)?;
                    let t = Self::add_tag_type(tag_basic_type, t, ctx);
                    list.push((Rc::new(typ), t.clone()));

                    let size = Self::size_of(&t)?;
                    if size > max_size {
                        max_size = size;
                        max_size_type = Some(t);
                    }

                    if max_size_type.is_none() {
                        max_size = size;
                        max_size_type = Some(t.clone())
                    }

                    index_map.insert(name.clone(), index);
                },
                Enumerator::TypeTuple { name, tuple_type } => {
                    let typ = TypeUtil::to_basic_type_enum(&tuple_type, ctx, pos)?;
                    let t = Self::add_tag_type(tag_basic_type, typ, ctx);
                    list.push((Rc::clone(tuple_type), t.clone()));

                    let size = Self::size_of(&t)?;
                    if size > max_size {
                        max_size = size;
                        max_size_type = Some(t);
                    }

                    if max_size_type.is_none() {
                        max_size = size;
                        max_size_type = Some(t.clone())
                    }

                    index_map.insert(name.clone(), index);
                },
            }

            index += 1;
        }

        Ok((list, index_map, max_size, max_size_type))
    }

    fn add_tag_type(tag_type: BasicTypeEnum,  typ: BasicTypeEnum, ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        let mut vec = Vec::new();

        vec.push(tag_type);
        vec.push(typ);

        let struct_type = ctx.struct_type(&vec, false);
        BasicTypeEnum::StructType(struct_type)
    }

    pub fn enum_from_enum_standard(&self, fields: &Vec<Enumerator>, _type_variables: &Option<Vec<String>>, _env: &mut Env<'ctx>) -> Result<(Vec<(String, IntValue<'ctx>)>, HashMap<String, usize>), CodeGenError> {
        let mut enumerator_list: Vec<(String, IntValue<'ctx>)> = Vec::new();
        let mut index_map: HashMap<String, usize> = HashMap::new();
        let mut index: usize = 0;

        for field in fields {
            match field {
                Enumerator::Const { name, const_value } => {
                    let i32_type = self.context.i32_type();
                    let i32_value = i32_type.const_int(*const_value as u64, false);
                    enumerator_list.push((name.to_string(), i32_value));

                    index_map.insert(name.clone(), index);
                },
                Enumerator::TypeTuple { name: _, tuple_type: _ } => {
                    panic!("standard enum do not have TypeTuple");
                },
                Enumerator::TypeStruct { name: _, struct_type: _ } => {
                    panic!("standard enum do not have TypeStruct");
                },
            }

            index += 1;
        }

        Ok((enumerator_list, index_map))
    }

    pub fn gen_enum_literal(
        &self,
        literal: &EnumLiteral,
        tag: &u64,
        env: &Env<'ctx>,
        break_catcher: Option<&BreakCatcher>,
        continue_catcher: Option<&ContinueCatcher>,
        _pos: &Position
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {

        match literal {
            EnumLiteral::Symbol(_elem_name, _index) => {
                let tag_type = self.enum_tag_llvm_type;
                let vec: Vec<BasicTypeEnum> = vec!(tag_type.into());
                let tagged_type = self.context.struct_type(&vec, false);
                let tagged_ptr = self.builder.build_alloca(tagged_type, "enum_literal")?;
                let tag_ptr = self.builder.build_struct_gep(tagged_type, tagged_ptr, 0, "struct_gep_in_tagged_enum")?;

                let tag_value = tag_type.const_int(*tag as u64, false);
                let _ = self.builder.build_store(tag_ptr, tag_value);

                let basic_val = self.builder.build_load(tagged_type, tagged_ptr, &format!("load_enum_literal"))?;
                let any_val = basic_val.as_any_value_enum();
                let typ = &self.enum_only_tag_type;
                Ok(Some(CompiledValue::new(Rc::clone(typ), any_val)))
            },
            EnumLiteral::Struct(struct_literal, _tag) => {
                let typ = struct_literal.get_type();
                let pos = struct_literal.get_position();

                let tag_type = self.enum_tag_llvm_type;
                let raw_type = env.basic_type_enum_from_type(&typ, self.context, pos)?;
                let vec: Vec<BasicTypeEnum> = vec!(tag_type.into(), raw_type);
                let tagged_type = self.context.struct_type(&vec, false);
                let tagged_ptr = self.builder.build_alloca(tagged_type, "enum_literal")?;
                let tag_ptr = self.builder.build_struct_gep(tagged_type, tagged_ptr, 0, "struct_gep_in_tagged_enum")?;
                let struct_ptr = self.builder.build_struct_gep(tagged_type, tagged_ptr, 1, "struct_gep_in_tagged_enum")?;

                let tag_value = tag_type.const_int(*tag as u64, false);
                let _ = self.builder.build_store(tag_ptr, tag_value);

                let initialized_literal = self.gen_struct_literal(struct_literal, struct_ptr, env, break_catcher, continue_catcher)?;
                let _initialized_literal = initialized_literal.ok_or(CodeGenError::cannot_init_enum(literal.get_type().get_type_name(), pos.clone()))?;

                let basic_val = self.builder.build_load(tagged_type, tagged_ptr, &format!("load_enum_literal"))?;
                let any_val = basic_val.as_any_value_enum();
                Ok(Some(CompiledValue::new(Rc::clone(typ), any_val)))
            },
            EnumLiteral::Tuple(tuple_literal, _tag) => {
                let typ = tuple_literal.get_type();
                let pos = tuple_literal.get_position();

                let tag_type = self.enum_tag_llvm_type;
                let raw_type = env.basic_type_enum_from_type(&typ, self.context, pos)?;
                let vec: Vec<BasicTypeEnum> = vec!(tag_type.into(), raw_type);
                let tagged_type = self.context.struct_type(&vec, false);
                let tagged_ptr = self.builder.build_alloca(tagged_type, "enum_literal");
                let tagged_ptr = self.builder.build_alloca(tagged_type, "enum_literal")?;
                let tag_ptr = self.builder.build_struct_gep(tagged_type, tagged_ptr, 0, "struct_gep_for_tag_in_tagged_enum")?;
                let tuple_ptr = self.builder.build_struct_gep(tagged_type, tagged_ptr, 1, "struct_gep_for_tuple_in_tagged_enum")?;
                let tag_value = tag_type.const_int(*tag as u64, false);
                let _ = self.builder.build_store(tag_ptr, tag_value);

                if tuple_literal.is_const() {
                    let const_list = tuple_literal.get_const_list();
                    let initialized_literal = self.gen_tuple_literal_from_const_expr_list(const_list, tuple_ptr, typ, pos)?;
                    let _initialized_literal = initialized_literal.ok_or(CodeGenError::cannot_init_enum(literal.get_type().get_type_name(), pos.clone()))?;

                }else{
                    let expr_list = tuple_literal.get_expr_list();
                    let initialized_literal = self.gen_tuple_literal(expr_list, Some(tuple_ptr), env, break_catcher, continue_catcher, pos)?;
                    let _initialized_literal = initialized_literal.ok_or(CodeGenError::cannot_init_enum(literal.get_type().get_type_name(), pos.clone()))?;
                }

                let basic_val = self.builder.build_load(tagged_type, tagged_ptr, &format!("load_enum_literal"))?;
                let any_val = basic_val.as_any_value_enum();
                Ok(Some(CompiledValue::new(Rc::clone(typ), any_val)))
            },
        }
    }

    pub fn gen_enum_const_literal(
        &self,
        literal: &EnumLiteral,
        pos: &Position
    ) -> Result<Option<CompiledValue<'ctx>>, Box<dyn Error>> {

        match literal {
            EnumLiteral::Symbol(_elem_name, tag) => {
                let tag_type = self.enum_tag_llvm_type;
                let tag_value = tag_type.const_int(*tag as u64, false);

                let mut tagged_values = Vec::new();
                tagged_values.push(tag_value.as_basic_value_enum());
                let tagged_struct = self.context.const_struct(tagged_values.as_slice(), false);
                let any_val = tagged_struct.as_any_value_enum();

                Ok(Some(CompiledValue::new(Rc::new(Type::Void), any_val)))
            },
            EnumLiteral::Struct(struct_literal, tag) => {
                if ! struct_literal.is_const() {
                    return Err(CodeGenError::not_const_initializer(pos.clone()).into());
                }

                let typ = struct_literal.get_type();
                let const_map = struct_literal.get_const_map().unwrap();
                let mut vec = Vec::new();

                let fields = typ.get_struct_fields().unwrap();
                for field in fields {
                    let name = field.get_name().as_ref().unwrap();
                    let const_expr = const_map.get(name).unwrap();
                    let basic_value = self.const_expr_to_basic_value_enum(const_expr);
                    vec.push(basic_value);
                }

                let struct_values = self.context.const_struct(&vec, false);

                let tag_type = self.enum_tag_llvm_type;
                let tag_value = tag_type.const_int(*tag as u64, false);

                let mut tagged_values = Vec::new();
                tagged_values.push(tag_value.as_basic_value_enum());
                tagged_values.push(struct_values.as_basic_value_enum());
                let tagged_struct = self.context.const_struct(tagged_values.as_slice(), false);
                let any_val = tagged_struct.as_any_value_enum();

                Ok(Some(CompiledValue::new(Rc::clone(typ), any_val)))
            },
            EnumLiteral::Tuple(tuple_literal, tag) => {
                if ! tuple_literal.is_const() {
                    return Err(CodeGenError::not_const_initializer(pos.clone()).into());
                }

                //
                // make const values
                //
                let const_exprs = tuple_literal.get_const_list();
                let mut vec = Vec::new();
                for const_expr in const_exprs {
                    let basic_value = self.const_expr_to_basic_value_enum(const_expr);
                    vec.push(basic_value);
                }
                let tuple_values = self.context.const_struct(&vec, false);

                let tag_type = self.enum_tag_llvm_type;
                let tag_value = tag_type.const_int(*tag as u64, false);

                let mut tagged_values = Vec::new();
                tagged_values.push(tag_value.as_basic_value_enum());
                tagged_values.push(tuple_values.as_basic_value_enum());
                let tagged_struct = self.context.const_struct(tagged_values.as_slice(), false);
                let any_val = tagged_struct.as_any_value_enum();

                //
                // make type
                //
                let typ = tuple_literal.get_type();
                // let tag_type = self.enum_tag_llvm_type;
                // let raw_type = env.basic_type_enum_from_type(&typ, self.context, pos)?;
                // let vec: Vec<BasicTypeEnum> = vec!(tag_type.into(), raw_type);
                // let tagged_type = self.context.struct_type(&vec, false);

                Ok(Some(CompiledValue::new(Rc::clone(typ), any_val)))
            },
        }
    }

    pub fn gen_global_enum_init<'b, 'c>(&self,
        // enum_def: &EnumDefinition,
        target_enum_ptr: GlobalValue<'ctx>,
        init: &Initializer,
    ) -> Result<Option<AnyValueEnum<'ctx>>, Box<dyn Error>> {

        let (init_value, pos) = if let Initializer::Simple(ExprAST::EnumLiteral(_typ, _tag, enum_literal, _pos), pos2) = init {
            (enum_literal, pos2)
        }else{
            return Err(Box::new(CodeGenError::initializer_is_not_enum(init.get_position().clone())));
        };

        let literal = self.gen_enum_const_literal(init_value, pos)?.unwrap();
        let mut basic_value = self.try_as_basic_value(&literal.get_value(), pos)?;

        let t1 = target_enum_ptr.get_value_type();
        let t2 = basic_value.get_type().as_any_type_enum();
        if t1 == t2 {
            target_enum_ptr.set_initializer(&basic_value);

        }else{
            // let literal_type = BasicTypeEnum::try_from(t2).or_else(|_| Err(CodeGenError::cannot_convert_anytypeenum_to_basictypeenum(pos.clone())))?;
            let target_ptr = target_enum_ptr.as_pointer_value();
            let _ = self.builder.build_store(target_ptr, basic_value)?;
        }

        Ok(None)
    }

/*
    pub fn make_tuple_type_enum_init_value<'b, 'c>(&self,
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

    #[allow(dead_code)]
    fn tuple_type_from_type_list(list: &Vec<Rc<Type>>, ctx: &'ctx Context, pos: &Position) -> Result<StructType<'ctx>, Box<dyn Error>> {
        let mut vec = Vec::new();

        for typ in list {
            let basic_type = TypeUtil::to_basic_type_enum(typ, ctx, pos)?;
            vec.push(basic_type);
        }

        let struct_type = ctx.struct_type(&vec, false);
        Ok(struct_type)
    }
*/
}

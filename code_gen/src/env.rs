#![allow(dead_code)]

use std::collections::HashMap;
use std::env::var;
use std::error::Error;
use std::hash::Hash;
use std::os::macos::raw;
// use inkwell::debug_info::DIFlagsConstants;
use inkwell::values::{PointerValue, FunctionValue, GlobalValue, AnyValueEnum, IntValue, BasicValueEnum, BasicValue};
use inkwell::types::{StructType, AnyTypeEnum, AnyType, BasicTypeEnum, IntType, BasicType};
use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use parser::FunProto;
use tokenizer::Position;
use crate::parser::{Type, ConstExpr, NumberType, ExprAST, CustFunctionType};
use crate::{code_gen, CodeGenError};
use super::type_util::TypeUtil;

use super::CodeGen;

#[derive(Debug)]
pub enum SomeValue<'ctx> {
    Global(&'ctx GlobalValue<'ctx>),
    Local(&'ctx PointerValue<'ctx>),
}

impl<'ctx> SomeValue<'ctx> {
    pub fn as_local_pointer(&self, pos: &Position) -> Result<&PointerValue<'ctx>, CodeGenError> {
        match self {
            SomeValue::Local(ptr) => Ok(*ptr),
            _ => Err(CodeGenError::cannot_convert_to_local_pointer(pos.clone())),
        }
    }

    pub fn as_global(&self, pos: &Position) -> Result<&GlobalValue<'ctx>, CodeGenError> {
        match self {
            SomeValue::Global(val) => Ok(*val),
            _ => Err(CodeGenError::cannot_convert_to_global_value(pos.clone())),
        }
    }

}

#[derive(Debug, Clone)]
pub struct BreakCatcher<'a> (&'a BasicBlock<'a>);

impl<'a> BreakCatcher<'a> {
    pub fn new(block: &'a BasicBlock) -> BreakCatcher<'a> {
        BreakCatcher(block)
    }

    pub fn get_block(&self) -> &'a BasicBlock<'a> {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct ContinueCatcher<'a> (&'a BasicBlock<'a>);


impl<'a> ContinueCatcher<'a> {
    pub fn new(block: &'a BasicBlock) -> ContinueCatcher<'a> {
        ContinueCatcher(block)
    }

    pub fn get_block(&self) -> &'a BasicBlock<'a> {
        self.0
    }
}

#[derive(Debug, Clone)]
pub enum CompiledCase<'a> {
    Case {
        cond: &'a ConstExpr,
        block: BasicBlock<'a>,
        code: Option<AnyValueEnum<'a>>,
        insert_block: Option<BasicBlock<'a>>,
        pos: Position
    },
    Default {
        block: BasicBlock<'a>,
        code: Option<AnyValueEnum<'a>>,
        insert_block: Option<BasicBlock<'a>>,
        pos: Position
    }
}

impl<'a> CompiledCase<'a> {
    pub fn new(cond: &'a ConstExpr, block: BasicBlock<'a>, code: Option<AnyValueEnum<'a>>, insert_block: Option<BasicBlock<'a>>, pos: Position) -> CompiledCase<'a> {
        CompiledCase::Case {
            cond,
            block,
            code,
            insert_block,
            pos,
        }
    }

    pub fn new_default(block: BasicBlock<'a>, code: Option<AnyValueEnum<'a>>, insert_block: Option<BasicBlock<'a>>, pos: Position) -> CompiledCase<'a> {
        CompiledCase::Default {
            block,
            code,
            insert_block,
            pos,
        }
    }

    pub fn is_case(&self) -> bool {
        match self {
            CompiledCase::Case { .. } => true,
            _ => false,
        }
    }

    pub fn is_default(&self) -> bool {
        match self {
            CompiledCase::Default { .. } => true,
            _ => false,
        }
    }

    pub fn get_block(&self) -> &BasicBlock<'a> {
        match self {
            CompiledCase::Case { block, .. } => block,
            CompiledCase::Default { block, .. } => block,
        }
    }

    pub fn get_code(&self) -> &Option<AnyValueEnum<'a>> {
        match self {
            CompiledCase::Case { code, .. } => code,
            CompiledCase::Default { code, .. } => code,
        }
    }

    pub fn get_cond(&self) -> Option<&'a ConstExpr> {
        match self {
            CompiledCase::Case { cond, .. } => Some(cond),
            CompiledCase::Default { .. } => None,
        }
    }

    pub fn get_insert_block(&self) -> &Option<BasicBlock<'a>> {
        match self {
            CompiledCase::Case { insert_block, .. } => insert_block,
            CompiledCase::Default { insert_block, .. } => insert_block,
        }
    }

    pub fn get_position(&self) -> &Position {
        match self {
            Self::Case { pos, .. } => pos,
            Self::Default { pos, .. } => pos,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeOrUnion<'ctx> {
    Type(AnyTypeEnum<'ctx>),
    Union{
        type_list: Vec<(Type, BasicTypeEnum<'ctx>)>,
        index_map: HashMap<String, usize>,
        max_size: u64,
        max_size_type: Option<BasicTypeEnum<'ctx>>,
    },
    StandardEnum {
        i32_type: IntType<'ctx>,
        enumerator_list: Vec<(String, IntValue<'ctx>)>,
        index_map: HashMap<String,usize>,
    },
    TypeDefStruct(String, *const TypeOrUnion<'ctx>),
    TypeDefUnion(String, *const TypeOrUnion<'ctx>),
}

impl<'ctx> TypeOrUnion<'ctx> {
    pub fn is_struct_type(&self) -> bool {
        match self {
            TypeOrUnion::Type(t) => t.is_struct_type(),
            TypeOrUnion::TypeDefStruct(_, _) => true,
            _ => false,
        }
    }

    pub fn is_union_type(&self) -> bool{
        match self {
            TypeOrUnion::Union {..} => true,
            TypeOrUnion::TypeDefUnion(_, _) => true,
            _ => false,
        }
    }

    pub fn as_any_type_enum(&self) -> AnyTypeEnum<'ctx> {
        match self {
            TypeOrUnion::Type(t) => t.as_any_type_enum(),
            TypeOrUnion::Union {max_size_type, ..} => {
                if let Some(t) = max_size_type {
                    t.as_any_type_enum()
                }else{
                    panic!("no type in union");
                }
            },
            TypeOrUnion::StandardEnum {i32_type, ..} => AnyTypeEnum::IntType(*i32_type),
            TypeOrUnion::TypeDefStruct(_struct_name, raw_ptr) => {
                let t = unsafe {
                    raw_ptr.as_ref().unwrap()
                };
                t.as_any_type_enum()
            },
            TypeOrUnion::TypeDefUnion(_name, raw_ptr) => {
                let t = unsafe {
                    raw_ptr.as_ref().unwrap()
                };
                t.as_any_type_enum()
            },
        }
    }
}

#[derive(Debug)]
enum ConstOrGlobalValue<'ctx> {
    Const{value: BasicValueEnum<'ctx>},
    GlobalValue{global: GlobalValue<'ctx>},
}

#[derive(Debug)]
pub struct Interface {
    name: String,
    function_prototypes: HashMap<String, FunProto>,
}

#[derive(Debug)]
pub struct Class<'ctx> {
    name: String,
    vars:HashMap<String, (Type, GlobalValue<'ctx>)>,
    class_functions: HashMap<String, (CustFunctionType, FunctionValue<'ctx>)>,
    functions: HashMap<String, (CustFunctionType, FunctionValue<'ctx>)>,
    interfaces: HashMap<String, (Interface,                                   // interface
                                 HashMap<String, &'ctx (CustFunctionType, FunctionValue<'ctx>)>)      // interfaceに対する実際の関数実装
                       >,
}

impl<'ctx> Class<'ctx> {
    pub fn new(name: &str) -> Self {
        Class {
            name: name.to_string(),
            vars: HashMap::new(),
            class_functions: HashMap::new(),
            functions: HashMap::new(),
            interfaces: HashMap::new(),
        }
    }

    #[inline]
    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn add_class_function(&mut self, func_name: &str, typ: CustFunctionType, function: FunctionValue<'ctx>, pos: &Position) -> Result<(), CodeGenError> {
        if self.class_functions.contains_key(func_name) {
            return  Err(CodeGenError::already_class_function_defined(self.name.clone(), func_name.to_string(), pos.clone()));
        }

        self.class_functions.insert(func_name.to_string(), (typ, function));

        Ok(())
    }

    pub fn add_member_function(&mut self, func_name: &str, typ: CustFunctionType, function: FunctionValue<'ctx>, pos: &Position) -> Result<(), CodeGenError> {
        if self.functions.contains_key(func_name) {
            return Err(CodeGenError::already_member_function_defined(self.name.clone(), func_name.to_string(), pos.clone()));
        }

        self.functions.insert(func_name.to_string(), (typ, function));

        Ok(())
    }

    #[inline]
    pub fn get_class_function(&self, func_name: &str) -> Option<&(CustFunctionType, FunctionValue<'ctx>)> {
        self.class_functions.get(func_name)
    }

    #[inline]
    pub fn get_member_function(&self, func_name: &str) -> Option<&(CustFunctionType, FunctionValue<'ctx>)> {
        self.functions.get(func_name)
    }

    pub fn add_class_var(&mut self, var_name: &str, typ: Type, ptr: GlobalValue<'ctx>, pos: &Position) -> Result<(), CodeGenError> {
        if self.vars.contains_key(var_name) {
            return Err(CodeGenError::already_class_var_defined(self.name.clone(), var_name.to_string(), pos.clone()));
        }

        self.vars.insert(var_name.to_string(), (typ, ptr));

        Ok(())
    }

    #[inline]
    pub fn get_class_var(&self, var_name: &str) -> Option<&(Type, GlobalValue<'ctx>)> {
        self.vars.get(var_name)
    }
}

#[derive(Debug)]
pub struct Env<'ctx> {
    global_def: HashMap<String, (Type, ConstOrGlobalValue<'ctx>)>,
    global_functions: HashMap<String, (CustFunctionType, FunctionValue<'ctx>)>,

    classes: HashMap<String, Class<'ctx>>,

    locals: Vec<Vec<HashMap<String, (Type, PointerValue<'ctx>)>>>,
    local_functions: Vec<Vec<HashMap<String, (CustFunctionType, FunctionValue<'ctx>)>>>,
    local_labels: Vec<Vec<HashMap<String, BasicBlock<'ctx>>>>,
    local_cases: Vec<Vec<Vec<CompiledCase<'ctx>>>>,

    current_function: Option<(CustFunctionType, FunctionValue<'ctx>)>,
    types: HashMap<String, (TypeOrUnion<'ctx>, Option<HashMap<String, usize>>)>,

    current_class: Option<*const Class<'ctx>>,
}

impl<'ctx> Env<'ctx> {
    pub fn new() -> Env<'ctx> {
        Env {
            global_def: HashMap::new(),
            global_functions: HashMap::new(),

            classes: HashMap::new(),

            locals: Vec::new(),
            local_functions: Vec::new(),
            local_labels: Vec::new(),
            local_cases: Vec::new(),

            current_function: None,
            types: HashMap::new(),

            current_class: None,
        }
    }

    pub fn add_new_function_local(&mut self) {
        self.locals.push(vec![HashMap::new()]);
        self.local_functions.push(vec![HashMap::new()]);
        self.local_labels.push(vec![HashMap::new()]);
        self.local_cases.push(vec![Vec::new()]);
    }

    pub fn remove_function_local(&mut self) {
        self.locals.pop();
        self.local_functions.pop();
        self.local_labels.pop();
        self.local_cases.pop();
    }

    pub fn add_new_local(&mut self) {
        self.locals.last_mut().unwrap().push(HashMap::new());
        self.local_functions.last_mut().unwrap().push(HashMap::new());
        self.local_labels.last_mut().unwrap().push(HashMap::new());
    }

    pub fn remove_local(&mut self) {
        self.locals.last_mut().unwrap().pop();
        self.local_functions.last_mut().unwrap().pop();
        self.local_labels.last_mut().unwrap().pop();
    }

    pub fn add_new_switch_case(&mut self) {
        self.local_cases.last_mut().unwrap().push(Vec::new());
    }

    pub fn remove_switch_case(&mut self) {
        self.local_cases.last_mut().unwrap().pop();
    }

    pub fn get_case_list(&self) -> &Vec<CompiledCase> {
        self.local_cases.last().unwrap().last().unwrap()
    }

    pub fn insert_case(&mut self, cond: &'ctx ConstExpr, block: BasicBlock<'ctx>, code: Option<AnyValueEnum<'ctx>>, insert_block: Option<BasicBlock<'ctx>>, pos: Position) {
        self.local_cases.last_mut().unwrap().last_mut().unwrap().push(CompiledCase::new(cond, block, code, insert_block, pos));
    }

    pub fn insert_default(&mut self, block: BasicBlock<'ctx>, code: Option<AnyValueEnum<'ctx>>, insert_block: Option<BasicBlock<'ctx>>, pos: Position) {
        self.local_cases.last_mut().unwrap().last_mut().unwrap().push(CompiledCase::new_default(block, code, insert_block, pos));
    }

    pub fn insert_global_var(&mut self, key: &str, typ: Type, ptr: GlobalValue<'ctx>) {
        self.global_def.insert(key.to_string(), (typ, ConstOrGlobalValue::GlobalValue { global: ptr }));
    }

    fn get_real_class_name(&self, class_name: &str) -> String {
        if let Some((type_or_union, _)) = self.types.get(class_name) {
            match type_or_union {
                TypeOrUnion::TypeDefStruct(struct_naem, _raw_ptr) => {
                    struct_naem.to_string()
                },
                TypeOrUnion::TypeDefUnion(name, _raw_ptr) => {
                    name.to_string()
                },
                _ => {
                    class_name.to_string()
                },
            }
        }else{
            class_name.to_string()
        }
    }

    pub fn insert_class_var(&mut self, class_name: &str, var_name: &str, typ: Type, ptr: GlobalValue<'ctx>, pos: &Position) -> Result<(), CodeGenError> {
        let class_name = &self.get_real_class_name(class_name);

        if ! self.classes.contains_key(class_name) {
            self.classes.insert(class_name.to_string(), Class::new(class_name));
        }

        self.classes.get_mut(class_name).unwrap().add_class_var(var_name, typ, ptr, pos)?;

        Ok(())
    }

    pub fn get_class_var(&self, class_name: &str, var_name: &str) -> Option<&(Type, GlobalValue<'ctx>)> {
        let class_name = &self.get_real_class_name(class_name);

        if ! self.classes.contains_key(class_name) {
            return None;
        }

        self.classes.get(class_name).unwrap().get_class_var(var_name)
    }

    pub fn insert_global_enumerator(&mut self, key: &str, typ: Type, value: IntValue<'ctx>) {
        self.global_def.insert(key.to_string(), (typ, ConstOrGlobalValue::Const { value: value.as_basic_value_enum() }));
    }

    pub fn insert_local(&mut self, key: &str, typ: Type, ptr: PointerValue<'ctx>) {
        self.locals.last_mut().unwrap().last_mut().unwrap().insert(key.to_string(), (typ, ptr));
    }

    pub fn insert_label(&mut self, key: &str, block: BasicBlock<'ctx>) -> Result<(), CodeGenError> {
        self.local_labels.last_mut().unwrap().last_mut().unwrap().insert(key.to_string(), block);
        Ok(())
    }

    pub fn insert_function(&mut self, key: &str, typ: CustFunctionType, function: FunctionValue<'ctx>) {
        self.global_functions.insert(key.to_string(), (typ, function));
    }

    pub fn insert_class_function(&mut self, class_name: &str, func_name: &str, typ: CustFunctionType, function: FunctionValue<'ctx>, pos: &Position) -> Result<(), CodeGenError> {
        if ! self.classes.contains_key(class_name) {
            self.classes.insert(class_name.to_string(), Class::new(class_name));
        }

        self.classes.get_mut(class_name).unwrap().add_class_function(func_name, typ, function, pos)?;

        Ok(())
    }

    pub fn insert_member_function(&mut self, class_name: &str, func_name: &str, typ: CustFunctionType, function: FunctionValue<'ctx>, pos: &Position) -> Result<(), CodeGenError> {
        if ! self.classes.contains_key(class_name) {
            self.classes.insert(class_name.to_string(), Class::new(class_name));
        }

        self.classes.get_mut(class_name).unwrap().add_member_function(func_name, typ, function, pos)?;

        Ok(())
    }

    pub fn get_function(&self, key: &str) -> Option<&(CustFunctionType, FunctionValue<'ctx>)> {
        self.global_functions.get(key)
    }

    pub fn get_class_function(&self, class_name: &str, func_name: &str) -> Option<&(CustFunctionType, FunctionValue<'ctx>)> {
        if ! self.classes.contains_key(class_name) {
            return  None;
        }

        self.classes.get(class_name).unwrap().get_class_function(func_name)
    }

    pub fn get_member_function(&self, class_name: &str, func_name: &str) -> Option<&(CustFunctionType, FunctionValue<'ctx>)> {
        if ! self.classes.contains_key(class_name) {
            return  None;
        }

        self.classes.get(class_name).unwrap().get_member_function(func_name)
    }

    pub fn insert_struct(&mut self, key: &str, struct_type: &StructType<'ctx>, index_map: HashMap<String, usize>, pos: &Position) -> Result<(), CodeGenError> {
        if let Some(_any_type_enum) = self.types.get(key) {
            return Err(CodeGenError::already_type_defined_in_struct(key, pos.clone()));
        }

        self.types.insert(key.to_string(), (TypeOrUnion::Type(struct_type.as_any_type_enum()), Some(index_map)));

        Ok(())
    }

    pub fn insert_union(
        &mut self,
        key: &str,
        type_list: Vec<(Type, BasicTypeEnum<'ctx>)>,
        index_map: HashMap<String, usize>,
        max_size: u64, max_size_type: Option<BasicTypeEnum<'ctx>>,
        pos: &Position
      ) -> Result<(), CodeGenError>
    {
        if let Some(_any_type_enum) = self.types.get(key) {
            return Err(CodeGenError::already_type_defined_in_union(key, pos.clone()));
        }

        let type_or_union = TypeOrUnion::Union { type_list, index_map: index_map.clone(), max_size, max_size_type };
        self.types.insert(key.to_string(), (type_or_union, Some(index_map)));

        Ok(())
    }

    pub fn insert_typedef(&mut self, key: &str, typ: &Type, ctx: &'ctx Context, pos: &Position) -> Result<(), Box<dyn Error>> {
        if let Some(_any_type_enum) = self.types.get(key) {
            return Err(Box::new(CodeGenError::already_type_defined_in_typedef(typ, key, pos.clone())));
        }

        match typ {
            Type::Symbol(name) => {
                let t = self.get_type(name).ok_or(CodeGenError::no_such_a_type(name, pos.clone()))?.clone();
                self.types.insert(key.to_string(), (t.clone(), None));
            },
            Type::Struct { name, fields } => {
                if let Some(struct_name) = name {
                    if let Some(t) = self.get_type(struct_name) {
                        if ! t.is_struct_type() {
                            return  Err(Box::new(CodeGenError::already_type_defined_in_typedef(typ, struct_name, pos.clone())));
                        }

                        self.types.insert(key.to_string(), (t.clone(), None));
                        return Ok(());
                    }
                }

                let (struct_type, index_map) = CodeGen::struct_from_struct_definition(name, fields, ctx, pos)?;
                if let Some(struct_name) = name {
                    self.insert_struct(struct_name, &struct_type, index_map, pos)?;
                    let raw_ptr = self.get_type(&struct_name).unwrap() as *const TypeOrUnion;
                    let t = TypeOrUnion::TypeDefStruct(struct_name.to_string(), raw_ptr);
                    self.types.insert(key.to_string(), (t, None));
                }else{
                    self.insert_struct(key, &struct_type, index_map, pos)?;
                };
            },
            Type::Union { name, fields } => {
                if let Some(id) = name {
                    if let Some(t) = self.get_type(id) {
                        if ! t.is_union_type() {
                            return  Err(Box::new(CodeGenError::already_type_defined_in_typedef(typ, id, pos.clone())));
                        }

                        self.types.insert(key.to_string(), (t.clone(), None));
                        return  Ok(());
                    }
                }

                let (type_list, index_map, max_size, max_size_type) = CodeGen::union_from_struct_definition(name, fields, ctx, pos)?;
                if let Some(union_name) = name {
                    self.insert_union(&union_name, type_list, index_map, max_size, max_size_type, pos)?;

                    let raw_ptr = self.get_type(&union_name).unwrap() as *const TypeOrUnion;
                    let t = TypeOrUnion::TypeDefUnion(union_name.to_string(), raw_ptr);
                    self.types.insert(key.to_string(), (t.clone(), None));

                }else{
                    self.insert_union(key, type_list, index_map, max_size, max_size_type, pos)?;
                }

            },
            _ => {
                let type_or_union = TypeOrUnion::Type(TypeUtil::to_llvm_any_type(typ, ctx, pos)?);
                self.types.insert(key.to_string(), (type_or_union, None));
            },
        };
        // self.types.insert(key.to_string(), (type_or_union, None));

        Ok(())
    }

    pub fn insert_enum(&mut self, key: &str, enum_type: &IntType<'ctx>, enumerator_list: Vec<(String, IntValue<'ctx>)>, index_map: HashMap<String, usize>, pos: &Position) -> Result<(), CodeGenError> {
        if let Some(_any_type_enum) = self.types.get(key) {
            return Err(CodeGenError::already_type_defined_in_struct(key, pos.clone()));
        }

        for (id, val) in &enumerator_list {
            self.insert_global_enumerator(id, Type::Number(NumberType::Int), *val);
        }

        let type_or_union = TypeOrUnion::StandardEnum { i32_type: *enum_type, enumerator_list: enumerator_list, index_map: index_map };
        self.types.insert(key.to_string(), (type_or_union, None));

        Ok(())
    }

    pub fn basic_type_enum_from_type(&self, typ: &Type, ctx: &'ctx Context, pos: &Position) -> Result<BasicTypeEnum<'ctx>, Box<dyn Error>> {
        match typ {
            Type::Struct { name, fields } => {
                if let Some(id) = name {
                    if let Some(type_or_union) = self.get_type(id) {
                        let mut type_or_union = type_or_union;
                        loop {
                            match type_or_union {
                                TypeOrUnion::Type(t) => {
                                    if let Ok(basic_type) = BasicTypeEnum::try_from(*t) {
                                        return Ok(basic_type);
                                    }else{
                                        return Err(Box::new(CodeGenError::mismatch_type_struct_fields(Some(id), pos.clone())));
                                    }
                                },
                                TypeOrUnion::Union { max_size_type, .. } => {
                                    let t = max_size_type.ok_or(CodeGenError::union_has_no_field(None, pos.clone()))?;
                                    if let Ok(basic_type) = BasicTypeEnum::try_from(t) {
                                        return Ok(basic_type);
                                    }else{
                                        return Err(Box::new(CodeGenError::mismatch_type_union_fields(Some(id), pos.clone())));
                                    }
                                },
                                TypeOrUnion::StandardEnum { i32_type, .. } => {
                                    return Ok(i32_type.as_basic_type_enum());
                                },
                                TypeOrUnion::TypeDefStruct(_name, raw_ptr) => {
                                    // let t = self.get_type(name).unwrap();
                                    // type_or_union = t;
                                    type_or_union = unsafe { raw_ptr.as_ref().unwrap() };
                                },
                                TypeOrUnion::TypeDefUnion(_name, raw_ptr) => {
                                    // let t = self.get_type(name).unwrap();
                                    // type_or_union = t;
                                    type_or_union = unsafe { raw_ptr.as_ref().unwrap() };
                                },
                            }
                        }
                    }else{
                        let (any_type, _index_map) = CodeGen::struct_from_struct_definition(&None, fields, ctx, pos)?;
                        let basic_type = BasicTypeEnum::try_from(any_type).unwrap();
                        Ok(basic_type)
                    }
                }else{
                    let (any_type, _index_map) = CodeGen::struct_from_struct_definition(&None, fields, ctx, pos)?;
                    let basic_type = BasicTypeEnum::try_from(any_type).unwrap();
                    Ok(basic_type)
                }
            },
            Type::Union { name, fields } => {
                if let Some(id) = name {
                    if let Some(type_or_union) = self.get_type(id) {
                        let mut type_or_union = type_or_union;
                        loop {
                            match type_or_union {
                                TypeOrUnion::Type(t) => {
                                    if let Ok(basic_type) = BasicTypeEnum::try_from(*t) {
                                        return Ok(basic_type);
                                    }else{
                                        return Err(Box::new(CodeGenError::mismatch_type_struct_fields(Some(id), pos.clone())));
                                    }
                                },
                                TypeOrUnion::Union { max_size_type, .. } => {
                                    let t = max_size_type.ok_or(CodeGenError::union_has_no_field(None, pos.clone()))?;
                                    if let Ok(basic_type) = BasicTypeEnum::try_from(t) {
                                        return Ok(basic_type);
                                    }else{
                                        return Err(Box::new(CodeGenError::mismatch_type_union_fields(Some(id), pos.clone())));
                                    }
                                },
                                TypeOrUnion::StandardEnum { i32_type, .. } => {
                                    return Ok(i32_type.as_basic_type_enum());
                                },
                                TypeOrUnion::TypeDefStruct(_name, raw_ptr) => {
                                    // let t = self.get_type(name).unwrap();
                                    // type_or_union = t;
                                    type_or_union = unsafe { raw_ptr.as_ref().unwrap() };
                                },
                                TypeOrUnion::TypeDefUnion(_name, raw_ptr) => {
                                    // let t = self.get_type(name).unwrap();
                                    // type_or_union = t;
                                    type_or_union = unsafe { raw_ptr.as_ref().unwrap() };
                                },                            }
                        }
                    }else{
                        let (_type_list, _index_map, _max_size, max_size_type_opt) = CodeGen::union_from_struct_definition(&None, fields, ctx, pos)?;
                        let max_size_type = max_size_type_opt.ok_or(CodeGenError::union_has_no_field(None, pos.clone()))?;
                        Ok(max_size_type)
                    }
                }else{
                    let (_type_list, _index_map, _max_size, max_size_type_opt) = CodeGen::union_from_struct_definition(&None, fields, ctx, pos)?;
                    let max_size_type = max_size_type_opt.ok_or(CodeGenError::union_has_no_field(None, pos.clone()))?;
                    Ok(max_size_type)
                }
            },
            Type::Enum { name: _, enum_def: _ } => {
                Ok(BasicTypeEnum::IntType(ctx.i32_type()))
            },
            _ => {
                Ok(TypeUtil::to_basic_type_enum(typ, ctx, pos)?)
            },
        }
    }

    pub fn get_block(&self, key: &str) -> Option<&BasicBlock> {
        let list = self.local_labels.last().unwrap();
        let mut index = list.len() - 1;
        loop {
            if let Some(tuple) = list[index].get(key) {
                return Some(tuple);
            }

            if index == 0 { break; }
            index -= 1;
        }

        None
    }

    pub fn get_type_by_id(&self, key: &str) -> Option<&Type> {
        if let Some((typ, _ptr)) = self.get_ptr_from_local(key) {
            Some(typ)
        }else if let Some((typ, val)) = self.global_def.get(key) {
            match val {
                ConstOrGlobalValue::GlobalValue { global: _ } => Some(typ),
                ConstOrGlobalValue::Const { value: _ } => Some(typ),
            }
        }else{
            None
        }
    }

    pub fn get_ptr(&self, key: &str) -> Option<(&Type, PointerValue<'ctx>)> {
        if let Some((typ, ptr)) = self.get_ptr_from_local(key) {
            Some((typ, *ptr))
        }else if let Some((typ, val)) = self.global_def.get(key) {
            match val {
                ConstOrGlobalValue::GlobalValue { global } => Some((typ, global.as_pointer_value())),
                _ => None,
            }
        }else{
            None
        }
    }

    pub fn get_value(&self, key: &str) -> Option<(&Type, BasicValueEnum<'ctx>)> {
        if let Some((typ, val)) = self.global_def.get(key) {
            match val {
                ConstOrGlobalValue::Const { value } => Some((typ, *value)),
                ConstOrGlobalValue::GlobalValue {..} => None,
            }
        }else{
            None
        }
    }

    fn get_ptr_from_local(&self, key: &str) -> Option<&(Type, PointerValue<'ctx>)> {
        let list = self.locals.last().unwrap();
        let mut index = list.len() - 1;
        loop {
            if let Some(tuple) = list[index].get(key) {
                return Some(tuple);
            }

            if index == 0 { break; }
            index -= 1;
        }

        None
    }

    pub fn get_self_ptr(&self) -> Option<(&Type, PointerValue<'ctx>)> {
        if let Some((typ, ptr)) = self.get_ptr_from_local("self") {
            Some((typ, *ptr))
        }else{
            None
        }
    }

    pub fn get_current_function(&self) -> Option<(&CustFunctionType, &FunctionValue<'ctx>)> {
        if let Some((ref typ, ref value)) = self.current_function {
            Some((typ, value))
        }else{
            None
        }
    }

    pub fn get_current_function_type(&self) -> Option<CustFunctionType> {
        if let Some((ref typ, ref _value)) = self.current_function {
            Some(typ.clone())
        }else{
            None
        }
    }

    pub fn set_current_function(&mut self,  typ: &CustFunctionType, func: FunctionValue<'ctx>) {
        self.current_function = Some((typ.clone(), func));
    }

    pub fn get_type(&self, key: &str) -> Option<&TypeOrUnion<'ctx>> {
        if let Some((typ, _index_map)) = self.types.get(key) {
            match typ {
                TypeOrUnion::TypeDefStruct(_name, raw_ptr) => {
                    // self.get_type(name)
                    let t = unsafe { raw_ptr.as_ref().unwrap() };
                    Some(t)
                },
                TypeOrUnion::TypeDefUnion(_name, raw_ptr) => {
                    // self.get_type(name)
                    let t = unsafe { raw_ptr.as_ref().unwrap() };
                    Some(t)
                },
                _ => {
                    Some(typ)
                }
            }
        }else{
            None
        }
    }

    pub fn get_type_and_index_map(&self, key: &str) -> Option<&(TypeOrUnion<'ctx>, Option<HashMap<String, usize>>)> {
        self.types.get(key)
    }

    pub fn is_signed(&self, ast: &ExprAST) -> Result<bool, CodeGenError> {
        match ast {
            ExprAST::Symbol(name, _pos) => {
                if let Some((typ, _pointer)) = self.get_ptr(name) {
                    Ok(typ.is_signed()?)
                }else{
                    Err(CodeGenError::condition_is_not_number(ast, ast.get_position().clone()))
                }
            },
            _ => Ok(ast.is_signed()?),
        }
    }

    #[inline]
    pub fn get_class(&self, name: &str) -> Option<&Class<'ctx>> {
        self.classes.get(name)
    }

    pub fn intern_class(&mut self, class_name: &str) -> &Class<'ctx> {
        if ! self.classes.contains_key(class_name) {
            self.classes.insert(class_name.to_string(), Class::new(class_name));
        }

        self.get_class(class_name).unwrap()
    }

    pub fn set_current_class(&mut self, class: *const Class<'ctx>) {
        self.current_class = Some(class);
    }

    pub fn remove_current_class(&mut self) {
        self.current_class = None;
    }

    pub fn get_current_class(&self) -> Option<*const Class<'ctx>> {
        self.current_class
    }
}
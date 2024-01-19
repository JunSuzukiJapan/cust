#![allow(dead_code)]

// use crate::compiler::CodeGen;

use super::{ParserError, StructDeclaration, SpecifierQualifier, ConstExpr};
use crate::Position;

// use inkwell::AddressSpace;
// use inkwell::context::Context;
// use inkwell::types::{BasicTypeEnum, BasicMetadataTypeEnum, AnyTypeEnum, AnyType};
// use inkwell::types::BasicType;
use std::cmp::Ordering;
use std::fmt;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NumberType {
    _Bool,
    Char,
    Short,
    Int,
    Long,
    LongLong,
    UnsignedChar,
    UnsignedShort,
    UnsignedInt,
    UnsignedLong,
    UnsignedLongLong,
    Float,
    Double,
    // Pointer(Box<NumberType>),
}

impl NumberType {
    // pub fn to_basic_type_enum<'ctx>(&self, ctx: &'ctx Context) -> Result<BasicTypeEnum<'ctx>, ParserError> {
    //     match self {
    //         NumberType::Char   => Ok(BasicTypeEnum::IntType(ctx.i8_type())),
    //         NumberType::Short  => Ok(BasicTypeEnum::IntType(ctx.i16_type())),
    //         NumberType::Int    => Ok(BasicTypeEnum::IntType(ctx.i32_type())),
    //         NumberType::Long   => Ok(BasicTypeEnum::IntType(ctx.i64_type())),
    //         NumberType::Float  => Ok(BasicTypeEnum::FloatType(ctx.f64_type())),
    //         NumberType::Double => Ok(BasicTypeEnum::FloatType(ctx.f128_type())),
    //         NumberType::UnsignedChar   => Ok(BasicTypeEnum::IntType(ctx.i8_type())),
    //         NumberType::UnsignedShort  => Ok(BasicTypeEnum::IntType(ctx.i16_type())),
    //         NumberType::UnsignedInt    => Ok(BasicTypeEnum::IntType(ctx.i32_type())),
    //         NumberType::UnsignedLong   => Ok(BasicTypeEnum::IntType(ctx.i64_type())),
    //         _ => Err(ParserError::cannot_convert_to_basic_type_enum(None)),
    //     }
    // }

    pub fn to_unsigned(&self, pos: &Position) -> Result<NumberType, ParserError> {
        match self {
            NumberType::Char => {
                Ok(NumberType::UnsignedChar)
            },
            NumberType::Short => {
                Ok(NumberType::UnsignedShort)
            },
            NumberType::Int => {
                Ok(NumberType::UnsignedInt)
            },
            NumberType::Long => {
                Ok(NumberType::UnsignedLong)
            },
            NumberType::LongLong => {
                Ok(NumberType::UnsignedLongLong)
            },
            NumberType::Float | NumberType::Double | NumberType::_Bool => {
                Err(ParserError::cannot_to_be_unsigned(pos.clone(), self))
            },
            _ => Ok(self.clone())
        }
    }
}

impl PartialOrd for NumberType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (NumberType::_Bool, NumberType::_Bool)           => Some(Ordering::Equal),
            (NumberType::_Bool, NumberType::Char)            => Some(Ordering::Less),
            (NumberType::_Bool, NumberType::UnsignedChar)    => Some(Ordering::Less),
            (NumberType::_Bool, NumberType::Short)           => Some(Ordering::Less),
            (NumberType::_Bool, NumberType::UnsignedShort)   => Some(Ordering::Less),
            (NumberType::_Bool, NumberType::Int)             => Some(Ordering::Less),
            (NumberType::_Bool, NumberType::UnsignedInt)     => Some(Ordering::Less),
            (NumberType::_Bool, NumberType::Long)            => Some(Ordering::Less),
            (NumberType::_Bool, NumberType::UnsignedLong)    => Some(Ordering::Less),
            (NumberType::_Bool, NumberType::Float)           => Some(Ordering::Less),
            (NumberType::_Bool, NumberType::Double)          => Some(Ordering::Less),

            (NumberType::Char, NumberType::_Bool)           => Some(Ordering::Greater),
            (NumberType::Char, NumberType::Char)            => Some(Ordering::Equal),
            (NumberType::Char, NumberType::UnsignedChar)    => Some(Ordering::Less),
            (NumberType::Char, NumberType::Short)           => Some(Ordering::Less),
            (NumberType::Char, NumberType::UnsignedShort)   => Some(Ordering::Less),
            (NumberType::Char, NumberType::Int)             => Some(Ordering::Less),
            (NumberType::Char, NumberType::UnsignedInt)     => Some(Ordering::Less),
            (NumberType::Char, NumberType::Long)            => Some(Ordering::Less),
            (NumberType::Char, NumberType::UnsignedLong)    => Some(Ordering::Less),
            (NumberType::Char, NumberType::Float)           => Some(Ordering::Less),
            (NumberType::Char, NumberType::Double)          => Some(Ordering::Less),

            (NumberType::Short, NumberType::_Bool)           => Some(Ordering::Greater),
            (NumberType::Short, NumberType::Char)            => Some(Ordering::Greater),
            (NumberType::Short, NumberType::UnsignedChar)    => Some(Ordering::Greater),
            (NumberType::Short, NumberType::Short)           => Some(Ordering::Equal),
            (NumberType::Short, NumberType::UnsignedShort)   => Some(Ordering::Less),
            (NumberType::Short, NumberType::Int)             => Some(Ordering::Less),
            (NumberType::Short, NumberType::UnsignedInt)     => Some(Ordering::Less),
            (NumberType::Short, NumberType::Long)            => Some(Ordering::Less),
            (NumberType::Short, NumberType::UnsignedLong)    => Some(Ordering::Less),
            (NumberType::Short, NumberType::Float)           => Some(Ordering::Less),
            (NumberType::Short, NumberType::Double)          => Some(Ordering::Less),

            (NumberType::Int, NumberType::_Bool)           => Some(Ordering::Greater),
            (NumberType::Int, NumberType::Char)            => Some(Ordering::Greater),
            (NumberType::Int, NumberType::UnsignedChar)    => Some(Ordering::Greater),
            (NumberType::Int, NumberType::Short)           => Some(Ordering::Greater),
            (NumberType::Int, NumberType::UnsignedShort)   => Some(Ordering::Greater),
            (NumberType::Int, NumberType::Int)             => Some(Ordering::Equal),
            (NumberType::Int, NumberType::UnsignedInt)     => Some(Ordering::Less),
            (NumberType::Int, NumberType::Long)            => Some(Ordering::Less),
            (NumberType::Int, NumberType::UnsignedLong)    => Some(Ordering::Less),
            (NumberType::Int, NumberType::Float)           => Some(Ordering::Less),
            (NumberType::Int, NumberType::Double)          => Some(Ordering::Less),

            (NumberType::Long, NumberType::_Bool)           => Some(Ordering::Greater),
            (NumberType::Long, NumberType::Char)            => Some(Ordering::Greater),
            (NumberType::Long, NumberType::UnsignedChar)    => Some(Ordering::Greater),
            (NumberType::Long, NumberType::Short)           => Some(Ordering::Greater),
            (NumberType::Long, NumberType::UnsignedShort)   => Some(Ordering::Greater),
            (NumberType::Long, NumberType::Int)             => Some(Ordering::Greater),
            (NumberType::Long, NumberType::UnsignedInt)     => Some(Ordering::Greater),
            (NumberType::Long, NumberType::Long)            => Some(Ordering::Less),
            (NumberType::Long, NumberType::UnsignedLong)    => Some(Ordering::Less),
            (NumberType::Long, NumberType::Float)           => Some(Ordering::Less),
            (NumberType::Long, NumberType::Double)          => Some(Ordering::Less),

            (NumberType::Float, NumberType::_Bool)           => Some(Ordering::Greater),
            (NumberType::Float, NumberType::Char)            => Some(Ordering::Greater),
            (NumberType::Float, NumberType::UnsignedChar)    => Some(Ordering::Greater),
            (NumberType::Float, NumberType::Short)           => Some(Ordering::Greater),
            (NumberType::Float, NumberType::UnsignedShort)   => Some(Ordering::Greater),
            (NumberType::Float, NumberType::Int)             => Some(Ordering::Greater),
            (NumberType::Float, NumberType::UnsignedInt)     => Some(Ordering::Greater),
            (NumberType::Float, NumberType::Long)            => Some(Ordering::Greater),
            (NumberType::Float, NumberType::UnsignedLong)    => Some(Ordering::Greater),
            (NumberType::Float, NumberType::Float)           => Some(Ordering::Equal),
            (NumberType::Float, NumberType::Double)          => Some(Ordering::Less),

            (NumberType::Double, NumberType::_Bool)           => Some(Ordering::Greater),
            (NumberType::Double, NumberType::Char)            => Some(Ordering::Greater),
            (NumberType::Double, NumberType::UnsignedChar)    => Some(Ordering::Greater),
            (NumberType::Double, NumberType::Short)           => Some(Ordering::Greater),
            (NumberType::Double, NumberType::UnsignedShort)   => Some(Ordering::Greater),
            (NumberType::Double, NumberType::Int)             => Some(Ordering::Greater),
            (NumberType::Double, NumberType::UnsignedInt)     => Some(Ordering::Greater),
            (NumberType::Double, NumberType::Long)            => Some(Ordering::Greater),
            (NumberType::Double, NumberType::UnsignedLong)    => Some(Ordering::Greater),
            (NumberType::Double, NumberType::Float)           => Some(Ordering::Greater),
            (NumberType::Double, NumberType::Double)          => Some(Ordering::Equal),

            (_, _) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pointer {
    pub is_const: bool,
    pub is_volatile: bool,
    pub next_pointer: Option<Box<Pointer>>,
}

impl Pointer {
    pub fn new(is_const: bool, is_volatile: bool) -> Pointer {
        Pointer {
            is_const: is_const,
            is_volatile: is_volatile,
            next_pointer: None,
        }
    }

    pub fn default() -> Pointer {
        Pointer {
            is_const: false,
            is_volatile: false,
            next_pointer: None,
        }
    }

    pub fn new_with_next_pointer(is_const: bool, is_volatile: bool, next_pointer: Pointer) -> Pointer {
        Pointer {
            is_const: is_const,
            is_volatile: is_volatile,
            next_pointer: Some(Box::new(next_pointer)),
        }
    }

    pub fn set_next_pointer(&mut self, next: Pointer) {
        self.next_pointer = Some(Box::new(next));
    }

    pub fn get_next_pointer(&self) -> &Option<Box<Pointer>> {
        &self.next_pointer
    }

    pub fn is_const(&self) -> bool {
        self.is_const
    }

    pub fn is_volatile(&self) -> bool {
        self.is_volatile
    }

    pub fn make_type_to(&self, typ: &Type) -> Type {
        if let Some(p) = &self.next_pointer {
            let next = p.make_type_to(typ);
            Type::Pointer(self.clone(), Box::new(next))
        }else{
            Type::Pointer(self.clone(), Box::new(typ.clone()))
        }
    }
}

impl fmt::Display for Pointer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        s.push_str("*");
        if self.is_const {
            s.push_str(" const");
        }
        if self.is_volatile {
            s.push_str(" volatile");
        }
        if let Some(next) = &self.next_pointer {
            s.push_str(" ");
            s.push_str(&*next.to_string())
        }

        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StructField {
    NormalField {
        name: Option<String>,
        sq: SpecifierQualifier,
        typ: Type,
    },
    BitField {
        name: Option<String>,
        sq: SpecifierQualifier,
        typ: Option<Type>,
        bit_size: u64,
    },
}

impl StructField {
    pub fn new_normal_field(name: Option<String>, typ: Type, sq: SpecifierQualifier) -> StructField {
        StructField::NormalField {
            name: name,
            typ: typ,
            sq: sq,
        }
    }

    pub fn new_bit_field(name: Option<String>, typ: Option<Type>, sq: SpecifierQualifier, bit_size: u64) -> StructField {
        StructField::BitField {
            name: name,
            typ: typ,
            sq: sq,
            bit_size: bit_size,
        }
    }

    pub fn get_type(&self) -> Option<&Type> {
        match self {
            StructField::NormalField {typ, ..} => Some(typ),
            StructField::BitField {typ, ..} => {
                if let Some(t) = typ {
                    Some(t)
                }else{
                    None
                }
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefinition {
    name: Option<String>,
    fields_and_index_map: Option<(Vec<StructField>, HashMap<String, usize>)>,
}

impl StructDefinition {
    pub fn try_new(struct_name: Option<String>, fields: Option<Vec<StructDeclaration>>, pos: &Position) -> Result<StructDefinition, ParserError> {
        if let Some(vec) = fields {
            let fields_and_index_map = Self::make_fields_from_vec(vec, pos)?;
            Ok(StructDefinition {
                name: struct_name,
                fields_and_index_map: Some(fields_and_index_map),
            })
        }else{
            Ok(StructDefinition {
                name: struct_name,
                fields_and_index_map: None,
            })
        }
    }

    pub fn make_fields_from_vec(fields: Vec<StructDeclaration>, pos: &Position) -> Result<(Vec<StructField>, HashMap<String, usize>), ParserError> {
        let mut struct_fields = Vec::new();
        let mut index_map: HashMap<String, usize> = HashMap::new();
        let mut index = 0;

        for decl in fields {
            let sq = decl.get_specifier_qualifier();
            let typ =  decl.get_type();
            let list = decl.get_declarator_list();

            for d in list {
                let field_name = if let Some(name) = d.get_name() {
                    Some(name.to_string())
                }else{
                    None
                };

                let field;
                if let Some(size) = d.get_bit_size() {
                    field = StructField::new_bit_field(field_name.clone(), typ.clone(), sq.clone(), *size);
                }else{
                    if let Some(t) = typ {
                        field = StructField::new_normal_field(field_name.clone(), t.clone(), sq.clone());
                    }else{
                        return Err(ParserError::no_type_for_struct_field(pos.clone()));
                    }
                }
                struct_fields.push(field);
                if let Some(id) = field_name {
                    index_map.insert(id.to_string(), index);
                };

                index += 1;
            }
        }

        Ok((struct_fields, index_map))
    }

    pub fn has_fields(&self) -> bool {
        self.fields_and_index_map.is_some()
    }

    pub fn len(&self) -> usize {
        if let Some(fields) = &self.fields_and_index_map {
            fields.0.len()
        }else{
            0
        }
    }

    pub fn get_type(&self, name: &str) -> Option<&Type> {
        if let Some((fields, index_map)) = &self.fields_and_index_map {
            if let Some(index) = index_map.get(name) {
                fields[*index].get_type()
            }else{
                None
            }
        }else{
            None
        }
    }

    pub fn get_fields(&self) -> Option<&Vec<StructField>> {
        if let Some(tuple) = &self.fields_and_index_map {
            Some(&tuple.0)
        }else{
            None
        }
    }

    pub fn get_index(&self, name: &str) -> Option<usize> {
        if let Some(tuple) = &self.fields_and_index_map {
            if let Some(val) = tuple.1.get(name) {
                Some(*val)
            }else{
                None
            }
        }else{
            None
        }
    }

    // pub fn try_size_of(&self, ctx: &Context, _defs: &Defines) -> Result<u64, ParserError> {
    //     let (struct_type, _map) = CodeGen::struct_from_struct_definition(&None, self, ctx)?;
    //     let int_value = struct_type.size_of().ok_or(ParserError::cannot_size_of_struct(None))?;
    //     let size = int_value.get_zero_extended_constant().ok_or(ParserError::cannot_size_of_struct(None))?;
    //     Ok(size)
    // }

}

#[derive(Debug, Clone, PartialEq)]
pub enum Enumerator {
    Const {
        name: String,
        const_value: u32,
    },
}

impl Enumerator {
    pub fn new(name: &str, value: u32) -> Enumerator {
        Enumerator::Const { name: name.to_string(), const_value: value }
    }

    #[inline]
    pub fn get_name(&self) -> &str {
        match self {
            Enumerator::Const { name, .. } => name,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDefinition {
    name: Option<String>,
    fields: Option<Vec<Enumerator>>,
    index_map: Option<HashMap<String, usize>>,
}

impl EnumDefinition {
    pub fn new(enum_name: Option<String>, enum_list: Option<Vec<Enumerator>>) -> EnumDefinition {
        let index_map = Self::make_map_from_vec(&enum_list);
        EnumDefinition {
            name: enum_name,
            fields: enum_list,
            index_map: index_map,
        }
    }

    pub fn make_map_from_vec(enumerators: &Option<Vec<Enumerator>>) -> Option<HashMap<String, usize>> {
        if let Some(list) = enumerators {
            let mut index_map: HashMap<String, usize> = HashMap::new();
            let mut index = 0;

            for enumerator in list {
                let id = enumerator.get_name();
                index_map.insert(id.to_string(), index);

                index += 1;
            }

            Some(index_map)
        }else{
            None
        }
    }

    pub fn get_fields(&self) -> & Option<Vec<Enumerator>> {
        &self.fields
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CustFunctionType {
    name: Option<String>,
    ret_type: Box<Type>,
    params_type: Vec<Type>,
    has_variadic: bool,
}

impl CustFunctionType {
    pub fn new(name: Option<String>, ret_type: Type, params_type: Vec<Type>, has_variadic: bool) -> CustFunctionType {
        CustFunctionType {
            name,
            ret_type: Box::new(ret_type),
            params_type,
            has_variadic,
        }
    }

    pub fn get_return_type(&self) -> &Type {
        &self.ret_type
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number(NumberType),
    Void,
    Symbol(String),
    Pointer(Pointer, Box<Type>),
    Function(CustFunctionType),
    Struct {
        name: Option<String>,
        fields: StructDefinition,
    },
    Union {
        name: Option<String>,
        fields: StructDefinition,
    },
    BitField,  // only for union member
    Array {
        name: Option<String>,
        typ: Box<Type>,
        opt_size_list: Vec<Option<ConstExpr>>,
    },
    Enum {
        name: Option<String>,
        enum_def: EnumDefinition,
    },
}

impl Type {
    pub fn new_function_type(name: Option<String>, ret_type: Type, params_type: Vec<Type>, has_variadic: bool) -> Type {
        let fun_type = CustFunctionType {
            name,
            ret_type: Box::new(ret_type),
            params_type,
            has_variadic,
        };
        Type::Function(fun_type)
    }

    pub fn new_pointer_type(typ: Type, is_const: bool, is_volatile: bool) -> Type {
        let pointer = Pointer::new(is_const, is_volatile);
        Type::Pointer(pointer, Box::new(typ))
    }

    pub fn struct_from_struct_definition(name: Option<String>, fields: StructDefinition) -> Type {
        Type::Struct { name, fields }
    }

    pub fn union_from_struct_definition(name: Option<String>, fields: StructDefinition) -> Type {
        Type::Union { name, fields }
    }

    pub fn enum_from_enum_definition(name: Option<String>, enum_def: EnumDefinition) -> Type {
        Type::Enum { name: name, enum_def: enum_def }
    }

    pub fn is_void(&self) -> bool {
        match self {
            Type::Void => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        match self {
            Type::Struct {..} => true,
            _ => false,
        }
    }

    pub fn get_pointed_type(&self, pos: &Position) -> Result<&Type, ParserError> {
        if let Type::Pointer(_pointer, to_type) = self {
            Ok(&**to_type)
        }else{
            Err(ParserError::not_pointer(pos.clone(), self))
        }
    }

    pub fn get_element_type(&self, pos: &Position) -> Result<&Type, ParserError> {
        if let Type::Array { typ, .. } = self {
            Ok(&**typ)
        }else{
            Err(ParserError::not_array(pos.clone(), self))
        }
    }

    pub fn get_fn_ret_type(&self, pos: &Position) -> Result<&Type, ParserError> {
        if let Type::Function(fun_type) = self {
            Ok(&fun_type.ret_type)
        }else{
            Err(ParserError::not_function(pos.clone(), self))
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Type::Number(_) => true,
            _ => false,
        }
    }

    pub fn is_signed(&self) -> Result<bool, ParserError> {
        match self {
            // Type::Number(NumberType::_Bool) => Ok(false),
            Type::Number(NumberType::Char) => Ok(true),
            Type::Number(NumberType::Short) => Ok(true),
            Type::Number(NumberType::Int) => Ok(true),
            Type::Number(NumberType::Long) => Ok(true),
            Type::Number(NumberType::LongLong) => Ok(true),
            _ => Ok(false)
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            Type::Array {..} => true,
            _ => false,
        }
    }

    pub fn can_sign(&self) -> bool {
        match self {
            Type::Number(NumberType::_Bool) => false,
            Type::Number(NumberType::Float) => false,
            Type::Number(NumberType::Double) => false,
            Type::Number(_) => true,
            _ => false,
        }
    }

    pub fn get_number_type(&self, pos: &Position) -> Result<&NumberType, ParserError> {
        match self {
            Type::Number(nt) => Ok(nt),
            _ => Err(ParserError::not_number_type(pos.clone(), self)),
        }
    }

    pub fn get_function_type(&self, pos: &Position) -> Result<&CustFunctionType, ParserError> {
        match self {
            Type::Function(fun_type) => Ok(fun_type),
            _ => Err(ParserError::not_function(pos.clone(), self))
        }
    }

    pub fn to_unsigned(&self, pos: &Position) -> Result<Type, ParserError> {
        match self {
            Type::Number(nt) => {
                Ok(Type::Number(nt.to_unsigned(pos)?))
            },
            Type::Pointer(p, typ) => {
                Ok(Type::Pointer(p.clone(), Box::new(typ.to_unsigned(pos)?)))
            },
            _ => Err(ParserError::not_number_type_to_be_unsigned(pos.clone(), self)),
        }
    }

    // pub fn to_basic_type_enum<'a>(&self, ctx: &'a Context) -> Result<BasicTypeEnum<'a>, ParserError> {
    //     match self {
    //         Type::Number(NumberType::_Bool)  => Ok(BasicTypeEnum::IntType(ctx.bool_type())),
    //         Type::Number(NumberType::Char)   => Ok(BasicTypeEnum::IntType(ctx.i8_type())),
    //         Type::Number(NumberType::Double) => Ok(BasicTypeEnum::FloatType(ctx.f128_type())),
    //         Type::Number(NumberType::Float)  => Ok(BasicTypeEnum::FloatType(ctx.f64_type())),
    //         Type::Number(NumberType::Int)    => Ok(BasicTypeEnum::IntType(ctx.i32_type())),
    //         Type::Number(NumberType::Long)  => Ok(BasicTypeEnum::IntType(ctx.i64_type())),
    //         Type::Number(NumberType::Short)  => Ok(BasicTypeEnum::IntType(ctx.i16_type())),
    //         Type::Number(NumberType::UnsignedChar)   => Ok(BasicTypeEnum::IntType(ctx.i8_type())),
    //         Type::Number(NumberType::UnsignedInt)    => Ok(BasicTypeEnum::IntType(ctx.i32_type())),
    //         Type::Number(NumberType::UnsignedLong)  => Ok(BasicTypeEnum::IntType(ctx.i64_type())),
    //         Type::Number(NumberType::UnsignedShort)  => Ok(BasicTypeEnum::IntType(ctx.i16_type())),
    //         // Type::Void   => Ok(BasicTypeEnum::VoidType(ctx.void_type())),
    //         Type::Pointer(_p, to_type) => {
    //             let typ = to_type.to_llvm_type(ctx)?;

    //             if typ.is_int_type() {
    //                 Ok(typ.into_int_type().ptr_type(AddressSpace::Generic).into())
    //             }else if typ.is_float_type() {
    //                 Ok(typ.into_float_type().ptr_type(AddressSpace::Generic).into())
    //             }else if typ.is_pointer_type() {
    //                 Ok(typ.into_pointer_type().ptr_type(AddressSpace::Generic).into())
    //             }else if typ.is_array_type() {




    //                 unimplemented!()
    //             }else if typ.is_vector_type() {





    //                 unimplemented!()
    //             }else if typ.is_struct_type() {
    //                 Ok(typ.into_struct_type().ptr_type(AddressSpace::Generic).into())
    //             }else{
    //                 Err(ParserError::illegal_type_for_pointer(None, &to_type))
    //             }

    //         },
    //         Type::Array { name: _, typ, opt_size_list } => {
    //             let mut to_type = typ.to_basic_type_enum(ctx)?;

    //             for opt_size in opt_size_list.iter().rev() {
    //                 let size = if let Some(sz) = opt_size {
    //                     sz.to_usize()?
    //                 }else{
    //                     0
    //                 };

    //                 to_type = to_type.array_type(size as u32).as_basic_type_enum();
    //             }

    //             Ok(to_type)
    //         },
    //         Type::Symbol(_name) => {





    //             unimplemented!()
    //         },
    //         Type::Struct { name, fields } => {
    //             let (struct_type, _index_map) = CodeGen::struct_from_struct_definition(name, fields, ctx)?;
    //             Ok(BasicTypeEnum::StructType(struct_type))
    //         },
    //         Type::Union { name, fields } => {
    //             let (_union_type, _index_map, _max_size, max_size_type) = CodeGen::union_from_struct_definition(name, fields, ctx)?;
    //             if let Some(typ) = max_size_type {
    //                 Ok(typ)
    //             }else{
    //                 if let Some(id) = name {
    //                     Err(ParserError::union_has_no_field(None, Some(id)))
    //                 }else{
    //                     Err(ParserError::union_has_no_field(None, None))
    //                 }
    //             }
    //         },
    //         Type::Enum { name, enum_def } => {



    //             unimplemented!()
    //         },
    //         _ => {
    //             Err(ParserError::no_such_a_type(None, &self.to_string()))
    //         },
    //     }
    // }


    // pub fn to_llvm_type<'a>(&self, ctx: &'a Context) -> Result<BasicMetadataTypeEnum<'a>, ParserError> {
    //     Ok(self.to_basic_type_enum(ctx)?.into())
    // }

    // pub fn to_llvm_any_type<'a>(&self, ctx: &'a Context) -> Result<AnyTypeEnum<'a>, ParserError> {
    //     match self {
    //         Type::Number(NumberType::Char)   => Ok(AnyTypeEnum::IntType(ctx.i8_type())),
    //         Type::Number(NumberType::Short)  => Ok(AnyTypeEnum::IntType(ctx.i16_type())),
    //         Type::Number(NumberType::Int)    => Ok(AnyTypeEnum::IntType(ctx.i32_type())),
    //         Type::Number(NumberType::Long)   => Ok(AnyTypeEnum::IntType(ctx.i64_type())),
    //         Type::Number(NumberType::Float)  => Ok(AnyTypeEnum::FloatType(ctx.f64_type())),
    //         Type::Number(NumberType::Double) => Ok(AnyTypeEnum::FloatType(ctx.f128_type())),
    //         Type::Number(NumberType::UnsignedChar)   => Ok(AnyTypeEnum::IntType(ctx.i8_type())),
    //         Type::Number(NumberType::UnsignedShort)  => Ok(AnyTypeEnum::IntType(ctx.i16_type())),
    //         Type::Number(NumberType::UnsignedInt)    => Ok(AnyTypeEnum::IntType(ctx.i32_type())),
    //         Type::Number(NumberType::UnsignedLong)   => Ok(AnyTypeEnum::IntType(ctx.i64_type())),
    //         Type::Void   => Ok(AnyTypeEnum::VoidType(ctx.void_type())),
    //         Type::Struct { name, fields } => {
    //             let name = if let Some(id) = name {
    //                 Some(id.clone())
    //             }else{
    //                 None
    //             };
    //             let (struct_type, _tbl) = CodeGen::struct_from_struct_definition(&name, &fields, ctx)?;
    //             Ok(struct_type.as_any_type_enum())
    //         },
    //         Type::Symbol(_name) => {
    //             unimplemented!("'{}' to AnyTypeEnum", &self.to_string())
    //         },
    //         _ => {
    //             unimplemented!("'{}' to AnyTypeEnum", &self.to_string())
    //         },
    //     }
    // }

    pub fn get_type_name(&self) -> String {
        match self {
            Type::Number(NumberType::_Bool)  => String::from("_Bool"),
            Type::Number(NumberType::Char)   => String::from("char"),
            Type::Number(NumberType::Short)  => String::from("short"),
            Type::Number(NumberType::Int)    => String::from("int"),
            Type::Number(NumberType::Long)   => String::from("long"),
            Type::Number(NumberType::Float)  => String::from("float"),
            Type::Number(NumberType::Double) => String::from("double"),
            Type::Number(NumberType::UnsignedChar)   => String::from("unsigned char"),
            Type::Number(NumberType::UnsignedShort)  => String::from("unsigned short"),
            Type::Number(NumberType::UnsignedInt)    => String::from("unsigned int"),
            Type::Number(NumberType::UnsignedLong)   => String::from("unsigned long"),

            Type::Void   => String::from("void"),
            Type::Symbol(name) => name.to_string(),
            Type::Struct { name, .. } => {
                if let Some(id) = name {
                    id.clone()
                }else{
                    panic!("no named struct")
                }
            }
            _ => String::from("<<no such a type>>"),
        }
    }

    pub fn is_union(&self) -> bool {
        match self {
            Type::Union {..} => true,
            _ => false,
        }
    }

    // pub fn size_of(&self, ctx: &Context, defs: &Defines) -> Option<u64> {
    //     match self {
    //         Type::Number(num_type) => {
    //             match num_type {
    //                 NumberType::_Bool => Some(1),
    //                 NumberType::Char => Some(1),
    //                 NumberType::UnsignedChar => Some(1),
    //                 NumberType::Short => Some(2),
    //                 NumberType::UnsignedShort => Some(2),
    //                 NumberType::Int => Some(4),
    //                 NumberType::UnsignedInt => Some(4),
    //                 NumberType::Long => Some(8),
    //                 NumberType::UnsignedLong => Some(8),
    //                 NumberType::LongLong => Some(16),
    //                 NumberType::UnsignedLongLong => Some(16),
    //                 NumberType::Float => {
    //                     let f32_type = ctx.f32_type();
    //                     let f32_type_size = f32_type.size_of();
    //                     let opt_size = f32_type_size.get_zero_extended_constant();
    //                     opt_size
    //                 },
    //                 NumberType::Double => {
    //                     let f64_type = ctx.f64_type();
    //                     let f64_type_size = f64_type.size_of();
    //                     let opt_size = f64_type_size.get_zero_extended_constant();
    //                     opt_size
    //                 },
    //             }
    //         },
    //         Type::Pointer(_pointer, typ) => {
    //             let some_type = typ.to_basic_type_enum(ctx);
    //             if let Ok(t) = some_type {
    //                 let ptr_type = t.ptr_type(AddressSpace::Generic);
    //                 let ptr_basic_type_size = ptr_type.size_of();
    //                 let opt_size = ptr_basic_type_size.get_zero_extended_constant();
    //                 opt_size
    //             }else{
    //                 None
    //             }
    //         },
    //         Type::Symbol(name) => {
    //             if let Some(typ) = defs.get_type(name) {
    //                 typ.size_of(ctx, defs)
    //             }else{
    //                 None
    //             }
    //         },
    //         Type::Struct { name: _, fields } => {
    //             if let Ok(size) = fields.try_size_of(ctx, defs) {
    //                 Some(size)
    //             }else{
    //                 None
    //             }
    //         },
    //         Type::Function {..} => None,

    //         _ => unimplemented!(),
    //     }
    // }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Number(number_type) => {
                match number_type {
                    NumberType::_Bool => write!(f, "_Bool"),
                    NumberType::Char => write!(f, "char"),
                    NumberType::Double => write!(f, "double"),
                    NumberType::Float => write!(f, "float"),
                    NumberType::Int => write!(f, "int"),
                    NumberType::Long => write!(f, "long"),
                    NumberType::LongLong => write!(f, "long long"),
                    NumberType::Short => write!(f, "sort"),
                    NumberType::UnsignedChar => write!(f, "unsigned char"),
                    NumberType::UnsignedInt => write!(f, "unsigned int"),
                    NumberType::UnsignedLong => write!(f, "unsigned long"),
                    NumberType::UnsignedLongLong => write!(f, "unsigned long long"),
                    NumberType::UnsignedShort => write!(f, "unsigned short"),
                }
            },
            Type::Void => write!(f, "void"),
            Type::Symbol(id) => write!(f, "symbol({})", id),
            Type::Pointer(p, t) => {
                write!(f, "{} {}", *t, p)
            },
            Type::Struct {name, fields: _} => {
                let name = if let Some(id) = name {
                    id
                }else{
                    "<no_name>"
                };
                write!(f, "struct {}", name)
            },
            Type::Union {name, fields: _} => {
                let name = if let Some(id) = name {
                    id
                }else{
                    "<no_name>"
                };
                write!(f, "union {}", name)
            },
            Type::Enum { name: _, enum_def: _} => {



                unimplemented!()
            },
            Type::Array { name, typ: _, opt_size_list } => {
                if let Some(id) = name {
                    write!(f, "array {}", id)?
                }else{
                    write!(f, "array <no_name>")?
                }

                for opt_size in opt_size_list {
                    if let Some(size) = opt_size {
                        write!(f, "[{:?}]", size)?
                    }else{
                        write!(f, "[]")?
                    }
                }

                Ok(())
            },
            Type::Function(fun_type) => {
                let name = if let Some(id) = &fun_type.name {
                    &id
                }else{
                    "<no_name>"
                };
                if fun_type.has_variadic {
                    write!(f, "{} {}(.., ...)", fun_type.ret_type, name)
                }else{
                    write!(f, "{} {}(..)", fun_type.ret_type, name)
                }
            },
            Type::BitField => {
                write!(f, "bit_field")
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeOrVariadic {
    Type(Type),
    Variadic,
}

impl TypeOrVariadic {
    pub fn from_type(typ: Type) -> TypeOrVariadic {
        TypeOrVariadic::Type(typ)
    }

    pub fn new_variadic() -> TypeOrVariadic {
        TypeOrVariadic::Variadic
    }

    pub fn is_variadic(&self) -> bool {
        *self == TypeOrVariadic::Variadic
    }

    pub fn get_type(&self) -> Option<&Type> {
        match self {
            TypeOrVariadic::Type(typ) => Some(typ),
            _ => None,
        }
    }
}

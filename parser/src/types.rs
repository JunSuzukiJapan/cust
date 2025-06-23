#![allow(dead_code)]

use tokenizer::Token;

use super::{ParserError, StructDeclaration, SpecifierQualifier};
use crate::ast::{StructLiteral, TupleLiteral};
use crate::{Position};

use std::cmp::Ordering;
use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;

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
}

impl NumberType {
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
                Err(ParserError::cannot_to_be_unsigned(self, pos.clone()))
            },
            _ => Ok(self.clone())
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            NumberType::Char => false,
            NumberType::Double => true,
            NumberType::Float => true,
            NumberType::Int => true,
            NumberType::Long => true,
            NumberType::LongLong => true,
            NumberType::Short => true,
            NumberType::UnsignedChar => false,
            NumberType::UnsignedInt => false,
            NumberType::UnsignedLong => false,
            NumberType::UnsignedLongLong => false,
            NumberType::UnsignedShort => false,
            NumberType::_Bool => false,
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

impl fmt::Display for NumberType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::_Bool => write!(f, "bool"),
            Self::Char => write!(f, "char"),
            Self::Short => write!(f, "short int"),
            Self::Int => write!(f, "int"),
            Self::Long => write!(f, "long int"),
            Self::LongLong => write!(f, "long long int"),
            Self::UnsignedChar => write!(f, "unsigned char"),
            Self::UnsignedShort => write!(f, "unsigned short"),
            Self::UnsignedInt => write!(f, "unsigned int"),
            Self::UnsignedLong => write!(f, "unsigned long int"),
            Self::UnsignedLongLong => write!(f, "unsigned long long int"),
            Self::Float => write!(f, "float"),
            Self::Double => write!(f, "double"),
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

    #[inline]
    pub fn get_next_pointer(&self) -> &Option<Box<Pointer>> {
        &self.next_pointer
    }

    #[inline]
    pub fn is_const(&self) -> bool {
        self.is_const
    }

    #[inline]
    pub fn is_volatile(&self) -> bool {
        self.is_volatile
    }

    pub fn make_type_to(&self, typ: &Rc<Type>) -> Rc<Type> {
        if let Some(p) = &self.next_pointer {
            let next = p.make_type_to(typ);
            let mut t = self.clone();
            t.next_pointer = None;
            Rc::new(Type::Pointer(t, Box::new(next)))
        }else{
            Rc::new(Type::Pointer(self.clone(), Box::new(Rc::clone(typ))))
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
        typ: Rc<Type>,
    },
    BitField {
        name: Option<String>,
        sq: SpecifierQualifier,
        typ: Option<Rc<Type>>,
        bit_size: usize,
    },
}

impl StructField {
    pub fn new_normal_field(name: Option<String>, typ: Rc<Type>, sq: SpecifierQualifier) -> StructField {
        StructField::NormalField {
            name: name,
            typ: typ,
            sq: sq,
        }
    }

    pub fn new_bit_field(name: Option<String>, typ: Option<Rc<Type>>, sq: SpecifierQualifier, bit_size: usize) -> StructField {
        StructField::BitField {
            name: name,
            typ: typ,
            sq: sq,
            bit_size: bit_size,
        }
    }

    pub fn is_normal(&self) -> bool {
        match self {
            StructField::NormalField {..} => true,
            _ => false,
        }
    }

    pub fn is_bit_field(&self) -> bool {
        match self {
            StructField::BitField {..} => true,
            _ => false,
        }
    }

    pub fn get_type(&self) -> Option<&Rc<Type>> {
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

    pub fn get_name(&self) -> &Option<String> {
        match self {
            StructField::NormalField {name, ..} => name,
            StructField::BitField {name, ..} => name,
        }
    }

    pub fn get_specifier_qualifier(&self) -> &SpecifierQualifier {
        match self {
            StructField::NormalField {sq, ..} => sq,
            StructField::BitField {sq, ..} => sq,
        }
    }

    pub fn get_bit_size(&self) -> Option<usize> {
        match self {
            StructField::BitField {bit_size, ..} => Some(*bit_size),
            _ => None,
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
                        field = StructField::new_normal_field(field_name.clone(), Rc::clone(t), sq.clone());
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

    pub fn get_name(&self) -> &Option<String> {
        &self.name
    }

    pub fn get_type(&self, name: &str) -> Option<&Rc<Type>> {
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

    pub fn get_field_by_name(&self, name: &str) -> Option<&StructField> {
        if let Some((list, index_map)) = &self.fields_and_index_map {
            if let Some(val) = index_map.get(name) {
                Some(&list[*val])
            }else{
                None
            }
        }else{
            None
        }
    }

    pub fn get_specifier_qualifier(&self, name: &str) -> Option<&SpecifierQualifier> {
        if let Some(tuple) = &self.fields_and_index_map {
            if let Some(val) = tuple.1.get(name) {
                Some(&tuple.0[*val].get_specifier_qualifier())
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
    //
    // EnumName::FieldName
    //
    Const {
        name: String,
        const_value: u32,
    },
    //
    // EnumName::FieldName(Type1, Type2, ..)
    //
    TypeTuple {
        name: String,
        // type_list: Vec<(Rc<Type>, u32)>,
        tuple_type: Rc<Type>,
    },
    //
    // EnumName::FieldName {name1: Type1, name2: Type2, ..}
    //
    TypeStruct {
        name: String,
        // definition: StructDefinition,
        struct_type: Rc<Type>,
    }
}

impl Enumerator {
    pub fn new(name: &str, value: u32) -> Enumerator {
        Enumerator::Const { name: name.to_string(), const_value: value }
    }

    pub fn new_tuple(name: &str, list: Vec<Rc<Type>>) -> Enumerator {
        let typ = Type::Tuple(list);
        Enumerator::TypeTuple { name: name.to_string(), tuple_type: Rc::new(typ) }
    }

    pub fn new_struct(name: &str, definition: StructDefinition) -> Enumerator {
        let struct_type = Type::struct_from_struct_definition(None, definition, None);
        Enumerator::TypeStruct { name: name.to_string(), struct_type: Rc::new(struct_type) }
    }

    pub fn is_const(&self) -> bool {
        match self {
            Enumerator::Const {..} => true,
            _ => false,
        }
    }

    pub fn is_struct_type(&self) -> bool {
        match self {
            Enumerator::TypeStruct {..} => true,
            _ => false,
        }
    }

    pub fn is_tuple_type(&self) -> bool {
        match self {
            Enumerator::TypeTuple {..} => true,
            _ => false,
        }
    }

    #[inline]
    pub fn get_name(&self) -> &str {
        match self {
            Enumerator::Const { name, .. } => name,
            Enumerator::TypeTuple { name, .. } => name,
            Enumerator::TypeStruct { name, .. } => name,
        }
    }

    pub fn get_struct_type(&self) -> Option<&Rc<Type>> {
        match self {
            Enumerator::TypeStruct { struct_type, .. } => Some(struct_type),
            _ => None,
        }
    }

    pub fn get_tuple_type(&self) -> Option<&Rc<Type>> {
        match self {
            Self::TypeTuple { tuple_type, .. } => Some(tuple_type),
            _ => None,
        }
    }

    pub fn get_type(&self) -> Option<&Rc<Type>> {
        match self {
            Enumerator::TypeStruct { struct_type, .. } => Some(struct_type),
            Enumerator::TypeTuple { tuple_type, .. } => Some(tuple_type),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumDefinition {
    StandardEnum {
        name: String,
        fields: Vec<Enumerator>,
        index_map: HashMap<String, usize>,
    },
    TaggedEnum {
        name: String,
        fields: Vec<Enumerator>,
        index_map: HashMap<String, usize>,
    },
}

impl EnumDefinition {
    pub fn new_standard(enum_name: String, enum_list: Vec<Enumerator>) -> EnumDefinition {
        let index_map = Self::make_map_from_vec(&enum_list);
        EnumDefinition::StandardEnum {
            name: enum_name,
            fields: enum_list,
            index_map: index_map,
        }
    }

    pub fn new_tagged(enum_name: String, enum_list: Vec<Enumerator>) -> EnumDefinition {
        let index_map = Self::make_map_from_vec(&enum_list);
        EnumDefinition::TaggedEnum {
            name: enum_name,
            fields: enum_list,
            index_map: index_map,
        }
    }

    pub fn is_standard(&self) -> bool {
        match self {
            EnumDefinition::StandardEnum { .. } => true,
            _ => false,
        }
    }

    pub fn is_tagged(&self) -> bool {
        match self {
            EnumDefinition::TaggedEnum { .. } => true,
            _ => false,
        }
    }

    fn make_map_from_vec(enumerators: &Vec<Enumerator>) -> HashMap<String, usize> {
        let mut index_map: HashMap<String, usize> = HashMap::new();
        let mut index = 0;

        for enumerator in enumerators {
            let id = enumerator.get_name();
            index_map.insert(id.to_string(), index);

            index += 1;
        }

        index_map
    }

    pub fn get_name(&self) -> &str {
        match self {
            EnumDefinition::StandardEnum { name, .. } => name,
            EnumDefinition::TaggedEnum { name, .. } => name,
        }
    }

    pub fn get_fields(&self) -> &Vec<Enumerator> {
        match self {
            EnumDefinition::StandardEnum { fields, .. } => &fields,
            EnumDefinition::TaggedEnum { fields, .. } => &fields,
        }
    }

    pub fn get_field_by_name(&self, name: &str) -> &Enumerator {
        let fields = self.get_fields();
        let index_map = self.get_index_map().unwrap();
        let index = index_map[name];
        let field = &fields[index];
        field
    }

    pub fn get_index_map(&self) -> Option<&HashMap<String, usize>> {
        match self {
            EnumDefinition::TaggedEnum { index_map, .. } => Some(index_map),
            EnumDefinition::StandardEnum { index_map, .. } => Some(index_map),
        }
    }

    pub fn get_index_by_name(&self, name: &str) -> Option<usize> {
        match self {
            EnumDefinition::StandardEnum { index_map, .. } => {
                if let Some(index) = index_map.get(name) {
                    Some(*index)
                }else{
                    None
                }
            },
            EnumDefinition::TaggedEnum { index_map, .. } => {
                if let Some(index) = index_map.get(name) {
                    Some(*index)
                }else{
                    None
                }
            },
        }
    }

    pub fn get_struct_type_index(&self, sub_type_name: &str, member_name: &str) -> Option<usize> {
        match self {
            EnumDefinition::TaggedEnum { index_map, fields, .. } => {
                let index = index_map.get(sub_type_name).map(|x| *x)?;
                let sub_type = fields.get(index)?;

                match sub_type {
                    Enumerator::TypeStruct { struct_type, .. } => {
                        let struct_def = struct_type.get_struct_definition()?;
                        struct_def.get_index(member_name)
                    },
                    _ => None,
                }
            },
            EnumDefinition::StandardEnum { index_map, fields, .. } => {
                let index = index_map.get(sub_type_name).map(|x| *x)?;
                let sub_type = fields.get(index)?;

                match sub_type {
                    Enumerator::TypeStruct { struct_type, .. } => {
                        let struct_def = struct_type.get_struct_definition()?;
                        struct_def.get_index(member_name)
                    },
                    _ => None,
                }
            },
        }
    }

    pub fn get_type(&self, name: &str) -> Option<&Rc<Type>> {
        match self {
            EnumDefinition::StandardEnum { fields, .. } => {
                for field in fields {
                    if field.get_name() == name {
                        return field.get_type();
                    }
                }
                None
            },
            EnumDefinition::TaggedEnum { fields, .. } => {
                for field in fields {
                    if field.get_name() == name {
                        return field.get_type();
                    }
                }
                None
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumInitializer {
    Symbol(String),
    SymbolWithIndex(String, u64),
    Tuple(String, u64, TupleLiteral),
    Struct(String, u64, StructLiteral),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CustFunctionType {
    name: Option<String>,
    ret_type: Box<Rc<Type>>,
    params_type: Vec<Rc<Type>>,
    has_variadic: bool,
}

impl CustFunctionType {
    pub fn new(name: Option<String>, ret_type: Rc<Type>, params_type: Vec<Rc<Type>>, has_variadic: bool) -> CustFunctionType {
        CustFunctionType {
            name,
            ret_type: Box::new(ret_type),
            params_type,
            has_variadic,
        }
    }

    pub fn get_return_type(&self) -> &Rc<Type> {
        &self.ret_type
    }
}

// #[derive(Debug, Clone, PartialEq)]
// pub enum GenericType {
//     Struct {
//         name: String,
//         fields: StructDefinition,
//         type_variables: Vec<String>,
//     },
//     Union {
//         name: String,
//         fields: StructDefinition,
//         type_variables: Vec<String>,
//     },
//     Enum {
//         name: String,
//         fields: EnumDefinition,
//         type_variables: Vec<String>,
//     },
// }

// impl GenericType {
//     pub fn get_name(&self) -> &str {
//         match self {
//             GenericType::Struct { name, .. } => name,
//             GenericType::Union { name, .. } => name,
//             GenericType::Enum { name, .. } => name,
//         }
//     }

//     pub fn get_type_variables(&self) -> &Vec<String> {
//         match self {
//             GenericType::Struct { type_variables, .. } => type_variables,
//             GenericType::Union { type_variables, .. } => type_variables,
//             GenericType::Enum { type_variables, .. } => type_variables,
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number(NumberType),
    Void,
    Symbol(String),
    Pointer(Pointer, Box<Rc<Type>>),
    Function(CustFunctionType),
    BitField,  // only for union member
    Array {
        name: Option<String>,
        typ: Box<Rc<Type>>,
        size_list: Vec<u32>,
    },
    Struct {
        name: Option<String>,
        fields: StructDefinition,
        type_variables: Option<Vec<String>>,
    },
    Union {
        name: Option<String>,
        fields: StructDefinition,
        type_variables: Option<Vec<String>>,
    },
    Enum {
        name: String,
        enum_def: EnumDefinition,
        type_variables: Option<Vec<String>>,
    },
    Tuple(Vec<Rc<Type>>),
    TypeVariable(String),
    BoundStructType {
        struct_type: Rc<Type>,
        map: HashMap<String, Rc<Type>>,
    },
    BoundUnionType {
        union_type: Rc<Type>,
        map: HashMap<String, Rc<Type>>,
    },
    BoundEnumType {
        enum_type: Rc<Type>,
        map: HashMap<String, Rc<Type>>,
    },
}

impl Type {
    pub fn new_number_type(typ: NumberType) -> Type {
        Type::Number(typ)
    }

    pub fn new_function_type(name: Option<String>, ret_type: Rc<Type>, params_type: Vec<Rc<Type>>, has_variadic: bool) -> Type {
        let fun_type = CustFunctionType {
            name,
            ret_type: Box::new(ret_type),
            params_type,
            has_variadic,
        };
        Type::Function(fun_type)
    }

    pub fn new_pointer_type(typ: Rc<Type>, is_const: bool, is_volatile: bool) -> Type {
        let pointer = Pointer::new(is_const, is_volatile);
        Type::Pointer(pointer, Box::new(typ))
    }

    pub fn from_token(token: &Token, pos: &Position) -> Result<Type, ParserError> {
        match token {
            Token::Char => Ok(Type::Number(NumberType::Char)),
            Token::Double => Ok(Type::Number(NumberType::Double)),
            Token::Float => Ok(Type::Number(NumberType::Float)),
            Token::Int => Ok(Type::Number(NumberType::Int)),
            Token::Long => Ok(Type::Number(NumberType::Long)),
            Token::Short => Ok(Type::Number(NumberType::Short)),
            Token::Void => Ok(Type::Void),
            Token::Symbol(name) => Ok(Type::Symbol(name.to_string())),
            _ => Err(ParserError::not_type_token(token.clone(), pos.clone())),
        }
    }

    pub fn struct_from_struct_definition(name: Option<String>, fields: StructDefinition, type_variables:Option<Vec<String>>) -> Type {
        Type::Struct { name, fields, type_variables }
    }

    pub fn union_from_struct_definition(name: Option<String>, fields: StructDefinition, type_variables:Option<Vec<String>>) -> Type {
        Type::Union { name, fields, type_variables }
    }

    pub fn enum_from_enum_definition(name: String, enum_def: EnumDefinition, type_variables:Option<Vec<String>>) -> Type {
        Type::Enum { name, enum_def, type_variables }
    }

    #[inline]
    pub fn is_void(&self) -> bool {
        match self {
            Type::Void => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_char(&self) -> bool {
        match self {
            Type::Number(NumberType::Char) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_array(&self) -> bool {
        match self {
            Type::Array {..} => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_pointer(&self) -> bool {
        match self {
            Type::Pointer(_, _) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_struct(&self) -> bool {
        match self {
            Type::Struct {..} => true,
            Type::BoundStructType {..} => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_bound_struct(&self) -> bool {
        match self {
            Type::BoundStructType {..} => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_union(&self) -> bool {
        match self {
            Type::Union {..} => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_enum(&self) -> bool {
        match self {
            Type::Enum { .. } => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_tagged_enum(&self) -> bool {
        match self {
            Type::Enum { enum_def, .. } => {
                match enum_def {
                    EnumDefinition::TaggedEnum { .. } => true,
                    _ => false,
                }
            },
            _ => false,
        }
    }

    #[inline]
    pub fn is_tuple(&self) -> bool {
        match self {
            Type::Tuple(..) => true,
            _ => false,
        }
    }

    pub fn get_tuple_size(&self) -> Option<usize> {
        match self {
            Type::Tuple(vec) => Some(vec.len()),
            _ => None,
        }
    }

    pub fn get_tuple_type_list(&self) -> Option<&Vec<Rc<Type>>> {
        match self {
            Type::Tuple(vec) => Some(vec),
            _ => None,
        }
    }

    pub fn get_field_type_from_tuple_at_index(&self, index: usize) -> Option<&Rc<Type>> {
        match self {
            Type::Tuple(vec) => {
                if index < vec.len() {
                    Some(&vec[index])
                }else{
                    None
                }
            },
            _ => None,
        }
    }

    #[inline]
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

    pub fn get_array_dimension(&self) -> &Vec<u32> {
        match self {
            Type::Array { size_list, .. } => size_list,
            _ => panic!("not array when get array dimension"),
        }
    }

    pub fn get_array_item_type(&self) -> &Rc<Type> {
        match self {
            Type::Array { typ, .. } => &*typ,
            _ => panic!("not array when get array item type"),
        }
    }

    pub fn get_array_name(&self) -> &Option<String> {
        match self {
            Type::Array { name, .. } => name,
            _ => panic!("not array when get array name"),
        }
    }

    pub fn get_struct_fields(&self) -> Option<&Vec<StructField>> {
        match self {
            Type::Struct {fields, ..} => fields.get_fields(),
            Type::BoundStructType { struct_type, .. } => struct_type.get_struct_fields(),
            _ => None,
        }
    }

    pub fn get_field_type_from_struct_at_index(&self, index: usize) -> Option<&Rc<Type>> {
        match self {
            Type::Struct { fields, .. } => {
                if let Some(field) = fields.get_fields() {
                    if index < field.len() {
                        field[index].get_type()
                    }else{
                        None
                    }
                }else{
                    None
                }
            },
            Type::BoundStructType { struct_type, .. } => struct_type.get_field_type_from_struct_at_index(index),
            _ => None,
        }
    }

    pub fn get_field_type_from_struct_by_name(&self, name: &str) -> Option<&Rc<Type>> {
        match self {
            Type::Struct { fields, .. } => {
                if let Some(field) = fields.get_field_by_name(name) {
                    field.get_type()
                }else{
                    None
                }
            },
            Type::BoundStructType { struct_type, .. } => struct_type.get_field_type_from_struct_by_name(name),
            _ => None,
        }
    }

    pub fn get_enum_sub_type_by_name(&self, sub_type_name: &str) -> Option<&Rc<Type>> {
        match self {
            Type::Enum { enum_def, .. } => {
                if let Some(index) = enum_def.get_index_by_name(sub_type_name) {
                    let field = enum_def.get_fields().get(index)?;
                    field.get_type()
                }else{
                    None
                }
            },
            _ => None,
        }
    }

    pub fn get_struct_definition(&self) -> Option<&StructDefinition> {
        match self {
            Type::Struct { fields, .. } => Some(fields),
            _ => None,
        }
    }

    pub fn get_union_fields(&self) -> Option<&Vec<StructField>> {
        match self {
            Type::Union { fields, .. } => fields.get_fields(),
            _ => None,
        }
    }

    pub fn get_enum_definition(&self) -> Option<&EnumDefinition> {
        match self {
            Type::Enum { enum_def, .. } => Some(enum_def),
            Type::BoundEnumType { enum_type, map } => {
                if let Type::Enum { enum_def, .. } = &**enum_type {
                    Some(enum_def)
                }else{
                    None
                }
            },
            _ => None,
        }
    }

    pub fn get_pointer(&self) -> Option<&Pointer> {
        match self {
            Type::Pointer(p, _) => Some(p),
            _ => None,
        }
    }

    pub fn get_pointed_type(&self, pos: &Position) -> Result<&Type, ParserError> {
        if let Type::Pointer(_pointer, to_type) = self {
            Ok(&**to_type)
        }else{
            Err(ParserError::not_pointer(self, pos.clone()))
        }
    }

    pub fn get_element_type(&self, pos: &Position) -> Result<&Type, ParserError> {
        if let Type::Array { typ, .. } = self {
            Ok(&**typ)
        }else{
            Err(ParserError::not_array(self, pos.clone()))
        }
    }

    pub fn get_fn_ret_type(&self, pos: &Position) -> Result<&Type, ParserError> {
        if let Type::Function(fun_type) = self {
            Ok(&fun_type.ret_type)
        }else{
            Err(ParserError::not_function(self, pos.clone()))
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
            _ => Err(ParserError::not_number_type(self, pos.clone())),
        }
    }

    pub fn get_function_type(&self, pos: &Position) -> Result<&CustFunctionType, ParserError> {
        match self {
            Type::Function(fun_type) => Ok(fun_type),
            _ => Err(ParserError::not_function(self, pos.clone()))
        }
    }

    pub fn to_unsigned(&self, pos: &Position) -> Result<Type, ParserError> {
        match self {
            Type::Number(nt) => {
                Ok(Type::Number(nt.to_unsigned(pos)?))
            },
            Type::Pointer(p, typ) => {
                Ok(Type::Pointer(p.clone(), Box::new(Rc::new(typ.to_unsigned(pos)?))))
            },
            _ => Err(ParserError::not_number_type_to_be_unsigned(self, pos.clone())),
        }
    }

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
                    // panic!("no named struct")
                    String::from("<no named struct>")
                }
            }
            _ => String::from("<<no such a type>>"),
        }
    }

    pub fn peel_off_pointer(&self) -> Option<Rc<Type>> {
        match self {
            Self::Pointer(_, boxed_type) => Some(Rc::clone(&*boxed_type.as_ref())),
            _ => None,
        }
    }

    pub fn has_type_variables(&self) -> bool {
        match self {
            Type::Struct { type_variables, .. } => type_variables.is_some(),
            Type::Union { type_variables, .. } => type_variables.is_some(),
            Type::Enum { type_variables, .. } => type_variables.is_some(),
            _ => false,
        }
    }

    pub fn get_type_variables(&self) -> &Option<Vec<String>> {
        match self {
            Type::Struct { type_variables, .. } => type_variables,
            Type::Union { type_variables, .. } => type_variables,
            Type::Enum { type_variables, .. } => type_variables,
            Type::BoundEnumType { enum_type, .. } => {
                if let Type::Enum { type_variables, .. } = &**enum_type {
                    type_variables
                }else{
                    &None
                }
            },
            Type::BoundStructType { struct_type, .. } => {
                if let Type::Struct { type_variables, .. } = &**struct_type {
                    type_variables
                }else{
                    &None
                }
            },
            Type::BoundUnionType { union_type, .. } => {
                if let Type::Union { type_variables, .. } = &**union_type {
                    type_variables
                }else{
                    &None
                }
            },
            _ => &None,
        }
    }

    pub fn get_type_map(&self) -> Option<&HashMap<String, Rc<Type>>> {
        match self {
            Type::BoundStructType { map, .. } => Some(map),
            Type::BoundUnionType { map, .. } => Some(map),
            Type::BoundEnumType { map, .. } => Some(map),
            _ => None,
        }
    }

    fn check_exist_type_variables(type_variables: &Option<Vec<String>>, map:&HashMap<String, Rc<Type>>, pos: &Position) -> Result<(), ParserError> {
        if let Some(vars) = type_variables {
            for var in vars {
                if map.len() != vars.len() {
                    return Err(ParserError::not_match_type_variables_count(vars.len(), map.len(), pos.clone()));
                }

                if ! map.contains_key(var) {
                    return Err(ParserError::no_such_a_type_variable(var.into(), pos.clone()));
                }
            }
        }else{
            if map.len() > 0 {
                return Err(ParserError::not_match_type_variables_count(0, map.len(), pos.clone()));
            }
        }

        Ok(())
    }

    pub fn bind_type_variables(&self, map: HashMap<String, Rc<Type>>, pos: &Position) -> Result<Rc<Type>, ParserError> {
        match self {
            Type::Struct { type_variables, .. } => {
                Self::check_exist_type_variables(&type_variables, &map, pos)?;

                let typ = Type::BoundStructType { struct_type: Rc::new(self.clone()), map: map };
                Ok(Rc::new(typ))
            },
            Type::Union { type_variables, .. } => {
                Self::check_exist_type_variables(&type_variables, &map, pos)?;

                let typ = Type::BoundUnionType { union_type: Rc::new(self.clone()), map: map };
                Ok(Rc::new(typ))
            },
            Type::Enum { type_variables, .. } => {
                Self::check_exist_type_variables(type_variables, &map, pos)?;

                let typ = Type::BoundEnumType { enum_type: Rc::new(self.clone()), map: map };
                Ok(Rc::new(typ))
            },
            _ => panic!("not a type with type variables"),
        }
    }
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
            Type::Struct {name, .. } => {
                let name = if let Some(id) = name {
                    id
                }else{
                    "<no_name>"
                };
                write!(f, "struct {}", name)
            },
            Type::Union {name, .. } => {
                let name = if let Some(id) = name {
                    id
                }else{
                    "<no_name>"
                };
                write!(f, "union {}", name)
            },
            Type::Enum { name, .. } => {
                write!(f, "Enum {}", name)
            },
            Type::Array { name, typ: _, size_list } => {
                if let Some(id) = name {
                    write!(f, "array {}", id)?
                }else{
                    write!(f, "array <no_name>")?
                }

                for size in size_list {
                    write!(f, "[{:?}]", size)?
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
            Type::Tuple(_type_list) => {
                write!(f, "tuple")
            },
            Type::TypeVariable(name) => {
                write!(f, "<generic type: {}>", name)
            },
            Type::BoundStructType { struct_type, .. } => {
                if let Type::Struct { name, .. } = struct_type.as_ref() {
                    let name = if let Some(id) = name {
                        id
                    }else{
                        "<no_name>"
                    };
                    write!(f, "bound struct {}", name)
                }else{
                    panic!("not a struct type")
                }
            },
            Type::BoundUnionType { union_type: struct_type, .. } => {
                if let Type::Union { name, .. } = struct_type.as_ref() {
                    let name = if let Some(id) = name {
                        id
                    }else{
                        "<no_name>"
                    };
                    write!(f, "bound union {}", name)
                }else{
                    panic!("not a union type")
                }
            },
            Type::BoundEnumType { enum_type, .. } => {
                if let Type::Enum { name, .. } = enum_type.as_ref() {
                    write!(f, "bound enum {}", name)
                }else{
                    panic!("not a enum type")
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeOrVariadic {
    Type(Rc<Type>),
    Variadic,
}

impl TypeOrVariadic {
    pub fn from_type(typ: Type) -> TypeOrVariadic {
        TypeOrVariadic::Type(Rc::new(typ))
    }

    pub fn new_variadic() -> TypeOrVariadic {
        TypeOrVariadic::Variadic
    }

    pub fn is_variadic(&self) -> bool {
        *self == TypeOrVariadic::Variadic
    }

    pub fn get_type(&self) -> Option<&Rc<Type>> {
        match self {
            TypeOrVariadic::Type(typ) => Some(typ),
            _ => None,
        }
    }
}

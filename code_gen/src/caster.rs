// use crate::parser::{AST, ExprAST, BinOp, Type, Pointer, Block, Params, StructDefinition, StructField, NumberType, Function, FunProto, FunOrProto, EnumDefinition, Enumerator};
// use crate::parser::{CompileError, Declaration, DeclarationSpecifier};
use super::CodeGenError;
// use crate::compiler::CompiledValue;
// use super::Env;
// use super::env::{BreakCatcher, ContinueCatcher, TypeOrUnion};
use crate::parser::Type;
// #[cfg(test)]
// use crate::parser::{SpecifierQualifier, DirectDeclarator, Declarator, Defines, Param};
// use crate::parser::{Switch, Case};

// use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
// use inkwell::execution_engine::ExecutionEngine;
// use inkwell::module::Module;
use inkwell::values::{AnyValueEnum, AnyValue};
// use inkwell::types::{BasicTypeEnum, AnyTypeEnum};
// use inkwell::values::IntMathValue;
// use inkwell::types::IntType;
use parser::NumberType;
// use inkwell::values::{BasicValueEnum, BasicMetadataValueEnum, AnyValueEnum, AnyValue, FunctionValue, InstructionOpcode, PointerValue, InstructionValue, BasicValue, IntValue};
// use inkwell::types::{BasicTypeEnum, AnyTypeEnum, FunctionType, BasicType, BasicMetadataTypeEnum};
// use inkwell::{IntPredicate, FloatPredicate};
// use inkwell::AddressSpace;
// use inkwell::types::AnyType;
// use inkwell::types::StructType;
use std::error::Error;

pub struct Caster;

impl Caster {
    pub fn gen_cast<'ctx>(builder: &Builder<'ctx>, ctx: &'ctx Context, value: &AnyValueEnum<'ctx>, from_type: &Type, to_type: &Type) -> Result<AnyValueEnum<'ctx>, Box<dyn Error>> {
println!("gen_cast. from: '{}', to: '{}'", from_type, to_type);

        match (from_type, to_type) {
            //
            // same types
            //
            (Type::Number(NumberType::Char), Type::Number(NumberType::Char)) |
            (Type::Number(NumberType::Short), Type::Number(NumberType::Short)) |
            (Type::Number(NumberType::Int), Type::Number(NumberType::Int)) |
            (Type::Number(NumberType::Long), Type::Number(NumberType::Long)) |
            (Type::Number(NumberType::LongLong), Type::Number(NumberType::LongLong)) |
            (Type::Number(NumberType::UnsignedChar), Type::Number(NumberType::UnsignedChar)) |
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::UnsignedShort)) |
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::UnsignedInt)) |
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::UnsignedLong)) |
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::UnsignedLongLong)) |
            (Type::Number(NumberType::Float), Type::Number(NumberType::Float)) |
            (Type::Number(NumberType::Double), Type::Number(NumberType::Double)) => Ok(*value),
            //
            // from char
            //
            (Type::Number(NumberType::Char), Type::Number(NumberType::Short)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, true, "cast_from_char_to_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Char), Type::Number(NumberType::Int)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_char_to_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Char), Type::Number(NumberType::Long)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_char_to_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Char), Type::Number(NumberType::LongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, true, "cast_from_char_to_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Char), Type::Number(NumberType::UnsignedChar)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, false, "cast_from_char_to_unsigned_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Char), Type::Number(NumberType::UnsignedShort)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, false, "cast_from_char_to_unsigned_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Char), Type::Number(NumberType::UnsignedInt)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_char_to_unsigned_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Char), Type::Number(NumberType::UnsignedLong)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_char_to_unsigned_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Char), Type::Number(NumberType::UnsignedLongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, false, "cast_from_char_to_unsigned_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Char), Type::Number(NumberType::Float)) => {
                let f32_type = ctx.f32_type();
                let result = value.into_int_value().const_signed_to_float(f32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Char), Type::Number(NumberType::Double)) => {
                let f64_type = ctx.f64_type();
                let result = value.into_int_value().const_signed_to_float(f64_type);
                Ok(result.as_any_value_enum())
            },
            //
            // from short
            //
            (Type::Number(NumberType::Short), Type::Number(NumberType::Char)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, true, "cast_from_short_to_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Short), Type::Number(NumberType::Int)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_short_to_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Short), Type::Number(NumberType::Long)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_short_to_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Short), Type::Number(NumberType::LongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, true, "cast_from_short_to_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Short), Type::Number(NumberType::UnsignedChar)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, false, "cast_from_short_to_unsigned_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Short), Type::Number(NumberType::UnsignedShort)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, false, "cast_from_short_to_unsigned_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Short), Type::Number(NumberType::UnsignedInt)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_short_to_unsigned_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Short), Type::Number(NumberType::UnsignedLong)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_short_to_unsigned_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Short), Type::Number(NumberType::UnsignedLongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, false, "cast_from_short_to_unsigned_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Short), Type::Number(NumberType::Float)) => {
                let f32_type = ctx.f32_type();
                let result = value.into_int_value().const_signed_to_float(f32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Short), Type::Number(NumberType::Double)) => {
                let f64_type = ctx.f64_type();
                let result = value.into_int_value().const_signed_to_float(f64_type);
                Ok(result.as_any_value_enum())
            },
            //
            // from int
            //
            (Type::Number(NumberType::Int), Type::Number(NumberType::Char)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, true, "cast_from_int_to_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Int), Type::Number(NumberType::Short)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, true, "cast_from_int_to_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Int), Type::Number(NumberType::Long)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_int_to_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Int), Type::Number(NumberType::LongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, true, "cast_from_int_to_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Int), Type::Number(NumberType::UnsignedChar)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, false, "cast_from_int_to_unsigned_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Int), Type::Number(NumberType::UnsignedShort)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, false, "cast_from_int_to_unsigned_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Int), Type::Number(NumberType::UnsignedInt)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_int_to_unsigned_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Int), Type::Number(NumberType::UnsignedLong)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_int_to_unsigned_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Int), Type::Number(NumberType::UnsignedLongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, false, "cast_from_int_to_unsigned_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Int), Type::Number(NumberType::Float)) => {
                let f32_type = ctx.f32_type();
                let result = value.into_int_value().const_signed_to_float(f32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Int), Type::Number(NumberType::Double)) => {
                let f64_type = ctx.f64_type();
                let result = value.into_int_value().const_signed_to_float(f64_type);
                Ok(result.as_any_value_enum())
            },
            //
            // from long
            //
            (Type::Number(NumberType::Long), Type::Number(NumberType::Char)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, true, "cast_from_long_to_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Long), Type::Number(NumberType::Short)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, true, "cast_from_long_to_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Long), Type::Number(NumberType::Int)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_long_to_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Long), Type::Number(NumberType::LongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, true, "cast_from_long_to_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Long), Type::Number(NumberType::UnsignedChar)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, false, "cast_from_long_to_unsigned_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Long), Type::Number(NumberType::UnsignedShort)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, false, "cast_from_long_to_unsigned_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Long), Type::Number(NumberType::UnsignedInt)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_long_to_unsigned_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Long), Type::Number(NumberType::UnsignedLong)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_long_to_unsigned_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Long), Type::Number(NumberType::UnsignedLongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, false, "cast_from_long_to_unsigned_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Long), Type::Number(NumberType::Float)) => {
                let f32_type = ctx.f32_type();
                let result = value.into_int_value().const_signed_to_float(f32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Long), Type::Number(NumberType::Double)) => {
                let f64_type = ctx.f64_type();
                let result = value.into_int_value().const_signed_to_float(f64_type);
                Ok(result.as_any_value_enum())
            },
            //
            // from longlong
            //
            (Type::Number(NumberType::LongLong), Type::Number(NumberType::Char)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, true, "cast_from_longlong_to_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::LongLong), Type::Number(NumberType::Short)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, true, "cast_from_longlong_to_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::LongLong), Type::Number(NumberType::Int)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_longlong_to_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::LongLong), Type::Number(NumberType::Long)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_longlong_to_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::LongLong), Type::Number(NumberType::UnsignedChar)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, false, "cast_from_longlong_to_unsigned_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::LongLong), Type::Number(NumberType::UnsignedShort)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, false, "cast_from_longlong_to_unsigned_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::LongLong), Type::Number(NumberType::UnsignedInt)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_longlong_to_unsigned_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::LongLong), Type::Number(NumberType::UnsignedLong)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_longlong_to_unsigned_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::LongLong), Type::Number(NumberType::UnsignedLongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, false, "cast_from_longlong_to_unsigned_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::LongLong), Type::Number(NumberType::Float)) => {
                let f32_type = ctx.f32_type();
                let result = value.into_int_value().const_signed_to_float(f32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::LongLong), Type::Number(NumberType::Double)) => {
                let f64_type = ctx.f64_type();
                let result = value.into_int_value().const_signed_to_float(f64_type);
                Ok(result.as_any_value_enum())
            },
            //
            // from float
            //
            (Type::Number(NumberType::Float), Type::Number(NumberType::Char)) => {
                let i8_type = ctx.i8_type();
                let result = value.into_float_value().const_to_signed_int(i8_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Float), Type::Number(NumberType::Short)) => {
                let i16_type = ctx.i16_type();
                let result = value.into_float_value().const_to_signed_int(i16_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Float), Type::Number(NumberType::Int)) => {
                let i32_type = ctx.i32_type();
                let result = value.into_float_value().const_to_signed_int(i32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Float), Type::Number(NumberType::Long)) => {
                let i32_type = ctx.i32_type();
                let result = value.into_float_value().const_to_signed_int(i32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Float), Type::Number(NumberType::LongLong)) => {
                let i32_type = ctx.i32_type();
                let result = value.into_float_value().const_to_signed_int(i32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Float), Type::Number(NumberType::UnsignedChar)) => {
                let i8_type = ctx.i8_type();
                let result = value.into_float_value().const_to_unsigned_int(i8_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Float), Type::Number(NumberType::UnsignedShort)) => {
                let i16_type = ctx.i16_type();
                let result = value.into_float_value().const_to_unsigned_int(i16_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Float), Type::Number(NumberType::UnsignedInt)) => {
                let i32_type = ctx.i32_type();
                let result = value.into_float_value().const_to_unsigned_int(i32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Float), Type::Number(NumberType::UnsignedLong)) => {
                let i32_type = ctx.i32_type();
                let result = value.into_float_value().const_to_unsigned_int(i32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Float), Type::Number(NumberType::UnsignedLongLong)) => {
                let i64_type = ctx.i64_type();
                let result = value.into_float_value().const_to_unsigned_int(i64_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Float), Type::Number(NumberType::Double)) => {
                let f64_type = ctx.f64_type();
                let result = value.into_float_value().const_cast(f64_type);
                Ok(result.as_any_value_enum())
            },






            //
            // from unsigned char
            //
            (Type::Number(NumberType::UnsignedChar), Type::Number(NumberType::Char)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, true, "cast_from_unsigned_char_to_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedChar), Type::Number(NumberType::Short)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, true, "cast_from_unsigned_char_to_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedChar), Type::Number(NumberType::Int)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_char_to_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedChar), Type::Number(NumberType::Long)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_char_to_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedChar), Type::Number(NumberType::LongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, true, "cast_from_unsigned_char_to_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedChar), Type::Number(NumberType::UnsignedShort)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, false, "cast_from_unsigned_char_to_unsigned_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedChar), Type::Number(NumberType::UnsignedInt)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_unsigned_char_to_unsigned_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedChar), Type::Number(NumberType::UnsignedLong)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_unsigned_char_to_unsigned_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedChar), Type::Number(NumberType::UnsignedLongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, false, "cast_from_unsigned_char_to_unsigned_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedChar), Type::Number(NumberType::Float)) => {
                let f32_type = ctx.f32_type();
                let result = value.into_int_value().const_signed_to_float(f32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedChar), Type::Number(NumberType::Double)) => {
                let f64_type = ctx.f64_type();
                let result = value.into_int_value().const_signed_to_float(f64_type);
                Ok(result.as_any_value_enum())
            },






            _ => unimplemented!("from type: '{}', to type: '{}'", from_type, to_type),
        }

        // match (from_type, to_type) {
        //     (AnyTypeEnum::IntType(_t1), BasicTypeEnum::IntType(t2)) => {
        //         let is_signed = unimplemented!();
        //         let result = builder.build_int_cast_sign_flag(value.into_int_value(), *t2, is_signed, "int_cast");
        //         // let result = value.into_int_value().const_cast(*t2, is_signed);
        //         Ok(result.as_any_value_enum())
        //     },
        //     (AnyTypeEnum::IntType(_t1), BasicTypeEnum::FloatType(t2)) => {
        //         let result = value.into_int_value().const_signed_to_float(*t2);
        //         Ok(result.as_any_value_enum())
        //     },
        //     (AnyTypeEnum::FloatType(_t1), BasicTypeEnum::IntType(_t2)) => {




        //         unimplemented!()
        //     },
        //     (AnyTypeEnum::FloatType(_t1), BasicTypeEnum::FloatType(_t2)) => {




        //         unimplemented!()
        //     },
        //     _ => {
        //         return Err(Box::new(CompileError::cannot_cast(None, &from_type, to_type)));
        //     }
        // }
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cast_byte_to_int() {

    }
}
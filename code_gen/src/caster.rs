use crate::parser::Type;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::values::{AnyValueEnum, AnyValue, BasicValueEnum};

use parser::{NumberType, ExprAST};
use super::type_util::TypeUtil;
use super::CodeGenError;
use std::error::Error;

pub struct Caster;

impl Caster {
    pub fn gen_implicit_cast<'ctx>(builder: &Builder<'ctx>, ctx: &'ctx Context, value: &AnyValueEnum<'ctx>, from_type: &Type, to_type: &Type) -> Result<AnyValueEnum<'ctx>, Box<dyn Error>> {
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
                let i64_type = ctx.i64_type();
                let result = value.into_float_value().const_to_signed_int(i64_type);
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
            // from double
            //
            (Type::Number(NumberType::Double), Type::Number(NumberType::Char)) => {
                let i8_type = ctx.i8_type();
                let result = value.into_float_value().const_to_signed_int(i8_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::Short)) => {
                let i16_type = ctx.i16_type();
                let result = value.into_float_value().const_to_signed_int(i16_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::Int)) => {
                let i32_type = ctx.i32_type();
                let result = value.into_float_value().const_to_signed_int(i32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::Long)) => {
                let i32_type = ctx.i32_type();
                let result = value.into_float_value().const_to_signed_int(i32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::LongLong)) => {
                let i64_type = ctx.i64_type();
                let result = value.into_float_value().const_to_signed_int(i64_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::UnsignedChar)) => {
                let i8_type = ctx.i8_type();
                let result = value.into_float_value().const_to_unsigned_int(i8_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::UnsignedShort)) => {
                let i16_type = ctx.i16_type();
                let result = value.into_float_value().const_to_unsigned_int(i16_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::UnsignedInt)) => {
                let i32_type = ctx.i32_type();
                let result = value.into_float_value().const_to_unsigned_int(i32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::UnsignedLong)) => {
                let i32_type = ctx.i32_type();
                let result = value.into_float_value().const_to_unsigned_int(i32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::UnsignedLongLong)) => {
                let i64_type = ctx.i64_type();
                let result = value.into_float_value().const_to_unsigned_int(i64_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::Float)) => {
                let f32_type = ctx.f32_type();
                let result = value.into_float_value().const_cast(f32_type);
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
            //
            // from unsigned short
            //
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::Char)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, true, "cast_from_unsigned_short_to_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::Short)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, true, "cast_from_unsigned_short_to_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::Int)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_short_to_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::Long)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_short_to_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::LongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, true, "cast_from_unsigned_short_to_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::UnsignedChar)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, false, "cast_from_unsigned_short_to_unsigned_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::UnsignedInt)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_unsigned_short_to_unsigned_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::UnsignedLong)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_unsigned_short_to_unsigned_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::UnsignedLongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, false, "cast_from_unsigned_short_to_unsigned_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::Float)) => {
                let f32_type = ctx.f32_type();
                let result = value.into_int_value().const_signed_to_float(f32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::Double)) => {
                let f64_type = ctx.f64_type();
                let result = value.into_int_value().const_signed_to_float(f64_type);
                Ok(result.as_any_value_enum())
            },
            //
            // from unsigned int
            //
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::Char)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, true, "cast_from_unsigned_int_to_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::Short)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, true, "cast_from_unsigned_int_to_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::Int)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_int_to_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::Long)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_int_to_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::LongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, true, "cast_from_unsigned_int_to_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::UnsignedChar)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, false, "cast_from_unsigned_int_to_unsigned_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::UnsignedShort)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, false, "cast_from_unsigned_int_to_unsigned_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::UnsignedLong)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_unsigned_int_to_unsigned_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::UnsignedLongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, false, "cast_from_unsigned_int_to_unsigned_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::Float)) => {
                let f32_type = ctx.f32_type();
                let result = value.into_int_value().const_signed_to_float(f32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::Double)) => {
                let f64_type = ctx.f64_type();
                let result = value.into_int_value().const_signed_to_float(f64_type);
                Ok(result.as_any_value_enum())
            },
            //
            // from unsigned long
            //
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::Char)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, true, "cast_from_unsigned_long_to_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::Short)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, true, "cast_from_unsigned_long_to_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::Int)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_long_to_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::Long)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_long_to_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::LongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, true, "cast_from_unsigned_long_to_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::UnsignedChar)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, false, "cast_from_unsigned_long_to_unsigned_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::UnsignedShort)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, false, "cast_from_unsigned_long_to_unsigned_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::UnsignedInt)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_unsigned_long_to_unsigned_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::UnsignedLongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, false, "cast_from_unsigned_long_to_unsigned_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::Float)) => {
                let f32_type = ctx.f32_type();
                let result = value.into_int_value().const_signed_to_float(f32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::Double)) => {
                let f64_type = ctx.f64_type();
                let result = value.into_int_value().const_signed_to_float(f64_type);
                Ok(result.as_any_value_enum())
            },
            //
            // from unsigned longlong
            //
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::Char)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, true, "cast_from_unsigned_longlong_to_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::Short)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, true, "cast_from_unsigned_longlong_to_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::Int)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_longlong_to_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::Long)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_longlong_to_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::LongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, true, "cast_from_unsigned_longlong_to_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::UnsignedChar)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, false, "cast_from_unsigned_longlong_to_unsigned_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::UnsignedShort)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, false, "cast_from_unsigned_longlong_to_unsigned_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::UnsignedInt)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_unsigned_longlong_to_unsigned_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::UnsignedLong)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_unsigned_longlong_to_unsigned_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::Float)) => {
                let f32_type = ctx.f32_type();
                let result = value.into_int_value().const_signed_to_float(f32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::Double)) => {
                let f64_type = ctx.f64_type();
                let result = value.into_int_value().const_signed_to_float(f64_type);
                Ok(result.as_any_value_enum())
            },



            _ => unimplemented!("from type: '{}', to type: '{}'", from_type, to_type),
        }
    }

    pub fn gen_cast<'ctx>(builder: &Builder<'ctx>, ctx: &'ctx Context, value: &AnyValueEnum<'ctx>, from_type: &Type, to_type: &Type, expr: &ExprAST) -> Result<AnyValueEnum<'ctx>, Box<dyn Error>> {


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
                let i64_type = ctx.i64_type();
                let result = value.into_float_value().const_to_signed_int(i64_type);
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
            // from double
            //
            (Type::Number(NumberType::Double), Type::Number(NumberType::Char)) => {
                let i8_type = ctx.i8_type();
                let result = value.into_float_value().const_to_signed_int(i8_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::Short)) => {
                let i16_type = ctx.i16_type();
                let result = value.into_float_value().const_to_signed_int(i16_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::Int)) => {
                let i32_type = ctx.i32_type();
                let result = value.into_float_value().const_to_signed_int(i32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::Long)) => {
                let i32_type = ctx.i32_type();
                let result = value.into_float_value().const_to_signed_int(i32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::LongLong)) => {
                let i64_type = ctx.i64_type();
                let result = value.into_float_value().const_to_signed_int(i64_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::UnsignedChar)) => {
                let i8_type = ctx.i8_type();
                let result = value.into_float_value().const_to_unsigned_int(i8_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::UnsignedShort)) => {
                let i16_type = ctx.i16_type();
                let result = value.into_float_value().const_to_unsigned_int(i16_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::UnsignedInt)) => {
                let i32_type = ctx.i32_type();
                let result = value.into_float_value().const_to_unsigned_int(i32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::UnsignedLong)) => {
                let i32_type = ctx.i32_type();
                let result = value.into_float_value().const_to_unsigned_int(i32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::UnsignedLongLong)) => {
                let i64_type = ctx.i64_type();
                let result = value.into_float_value().const_to_unsigned_int(i64_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::Double), Type::Number(NumberType::Float)) => {
                let f32_type = ctx.f32_type();
                let result = value.into_float_value().const_cast(f32_type);
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
            //
            // from unsigned short
            //
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::Char)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, true, "cast_from_unsigned_short_to_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::Short)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, true, "cast_from_unsigned_short_to_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::Int)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_short_to_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::Long)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_short_to_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::LongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, true, "cast_from_unsigned_short_to_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::UnsignedChar)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, false, "cast_from_unsigned_short_to_unsigned_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::UnsignedInt)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_unsigned_short_to_unsigned_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::UnsignedLong)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_unsigned_short_to_unsigned_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::UnsignedLongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, false, "cast_from_unsigned_short_to_unsigned_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::Float)) => {
                let f32_type = ctx.f32_type();
                let result = value.into_int_value().const_signed_to_float(f32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedShort), Type::Number(NumberType::Double)) => {
                let f64_type = ctx.f64_type();
                let result = value.into_int_value().const_signed_to_float(f64_type);
                Ok(result.as_any_value_enum())
            },
            //
            // from unsigned int
            //
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::Char)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, true, "cast_from_unsigned_int_to_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::Short)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, true, "cast_from_unsigned_int_to_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::Int)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_int_to_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::Long)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_int_to_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::LongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, true, "cast_from_unsigned_int_to_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::UnsignedChar)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, false, "cast_from_unsigned_int_to_unsigned_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::UnsignedShort)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, false, "cast_from_unsigned_int_to_unsigned_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::UnsignedLong)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_unsigned_int_to_unsigned_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::UnsignedLongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, false, "cast_from_unsigned_int_to_unsigned_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::Float)) => {
                let f32_type = ctx.f32_type();
                let result = value.into_int_value().const_signed_to_float(f32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedInt), Type::Number(NumberType::Double)) => {
                let f64_type = ctx.f64_type();
                let result = value.into_int_value().const_signed_to_float(f64_type);
                Ok(result.as_any_value_enum())
            },
            //
            // from unsigned long
            //
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::Char)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, true, "cast_from_unsigned_long_to_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::Short)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, true, "cast_from_unsigned_long_to_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::Int)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_long_to_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::Long)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_long_to_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::LongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, true, "cast_from_unsigned_long_to_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::UnsignedChar)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, false, "cast_from_unsigned_long_to_unsigned_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::UnsignedShort)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, false, "cast_from_unsigned_long_to_unsigned_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::UnsignedInt)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_unsigned_long_to_unsigned_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::UnsignedLongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, false, "cast_from_unsigned_long_to_unsigned_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::Float)) => {
                let f32_type = ctx.f32_type();
                let result = value.into_int_value().const_signed_to_float(f32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLong), Type::Number(NumberType::Double)) => {
                let f64_type = ctx.f64_type();
                let result = value.into_int_value().const_signed_to_float(f64_type);
                Ok(result.as_any_value_enum())
            },
            //
            // from unsigned longlong
            //
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::Char)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, true, "cast_from_unsigned_longlong_to_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::Short)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, true, "cast_from_unsigned_longlong_to_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::Int)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_longlong_to_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::Long)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, true, "cast_from_unsigned_longlong_to_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::LongLong)) => {
                let i64_type = ctx.i64_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i64_type, true, "cast_from_unsigned_longlong_to_longlong")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::UnsignedChar)) => {
                let i8_type = ctx.i8_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i8_type, false, "cast_from_unsigned_longlong_to_unsigned_char")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::UnsignedShort)) => {
                let i16_type = ctx.i16_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i16_type, false, "cast_from_unsigned_longlong_to_unsigned_short")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::UnsignedInt)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_unsigned_longlong_to_unsigned_int")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::UnsignedLong)) => {
                let i32_type = ctx.i32_type();
                let result = builder.build_int_cast_sign_flag(value.into_int_value(), i32_type, false, "cast_from_unsigned_longlong_to_unsigned_long")?;
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::Float)) => {
                let f32_type = ctx.f32_type();
                let result = value.into_int_value().const_signed_to_float(f32_type);
                Ok(result.as_any_value_enum())
            },
            (Type::Number(NumberType::UnsignedLongLong), Type::Number(NumberType::Double)) => {
                let f64_type = ctx.f64_type();
                let result = value.into_int_value().const_signed_to_float(f64_type);
                Ok(result.as_any_value_enum())
            },
            //
            // Pointer to Pointer
            //
            (Type::Pointer(_, _), Type::Pointer(_, _)) => {
                let to = TypeUtil::to_basic_type_enum(to_type, ctx)?;
                let value = if let Ok(val) = BasicValueEnum::try_from(*value) {
                    val
                }else{
                    return Err(Box::new(CodeGenError::cannot_convert_to_basic_value(expr.clone(), expr.get_position().clone())));
                };
                let result = builder.build_bitcast(value, to, "ptr_to_ptr")?;

                Ok(result.as_any_value_enum())
            },



            _ => unimplemented!("from type: '{}', to type: '{}'", from_type, to_type),
        }
    }
        
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cast_byte_to_int() {

    }
}
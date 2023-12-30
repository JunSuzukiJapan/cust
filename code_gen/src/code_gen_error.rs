use parser::{ExprAST, ParserError, NumberType};
use super::{Position, Type};
use std::error::Error;
use std::fmt;
use inkwell::{values::{BasicValueEnum, BasicMetadataValueEnum, AnyValueEnum, AnyValue, FunctionValue, InstructionOpcode, PointerValue, InstructionValue, BasicValue, IntValue}, types::BasicTypeEnum};

#[derive(Debug, Clone, PartialEq)]
pub enum CodeGenError {
    ParserError(ParserError),
    ConditionIsNotNumber(Option<Position>, ExprAST),
    AlreadyTypeDefinedInStruct(Option<Position>, String),
    AlreadyTypeDefinedInUnion(Option<Position>, String),
    AlreadyTypeDefinedInTypedef(Option<Position>, Type, String),
    UnionHasNoField(Option<Position>, Option<String>),
    MismatchTypeUnionFields(Option<Position>, Option<String>),
    MismatchTypeStructFields(Option<Position>, Option<String>),
    NoSuchAType(Option<Position>, String),
    CannotConvertToGlobalValue(Option<Position>),
    CannotConvertToLocalPointer(Option<Position>),
    IllegalTypeForPointer(Option<Position>, Type),
    IllegalEndOfInput(Option<Position>),
    ReturnWithoutFunction(Option<Position>),
    ReturnTypeMismatch {
        opt_pos: Option<Position>,
        real_type: Type,
        required_type: Type,
    },
    NoCurrentFunction(Option<Position>),
    BreakNotInLoopOrSwitch(Option<Position>),
    ContinueNotInLoop(Option<Position>),
    NoSuchALabel(Option<Position>, String),
    NoSuchAVariable(Option<Position>, String),
    NoSuchAFunction(Option<Position>, String),
    NoSuchAStruct(Option<Position>, String),
    CannotGetPointer(Option<Position>),
    MismatchInitializerType(Option<Position>),
    InitialListIsTooLong(Option<Position>),
    CannotInitStructMember(Option<Position>),
    CannotGetZeroValue(Option<Position>),
    CaseAfterDefault(Option<Position>),
    AlreadyDefaultDefined(Option<Position>),
    IllegalBitSize {
        opt_pos: Option<Position>,
        typ: NumberType,
        size: u64,
    },
    CannotAddValue(Option<Position>, Type),
    CannotSubValue(Option<Position>, Type),
    CannotMulValue(Option<Position>, Type),
    CannotDivValue(Option<Position>, Type),
    CannotModValue(Option<Position>, Type),
    CannotCompareValue(Option<Position>, Type),
    CannotApplyLogicalOpValue(Option<Position>, Type),
    NotIntInShift(Option<Position>, Type),
    NotIntBitAnd(Option<Position>, Type),
    NotIntBitOr(Option<Position>, Type),
    NotIntBitXor(Option<Position>, Type),
    AssignIllegalValue(Option<Position>, ExprAST),
    NoSuchAMember(Option<Position>, String),
    CannotAccessStructMember(Option<Position>, String),
    NotUnion(Option<Position>, String),
    NoIndexValueWhileAccessArray(Option<Position>),
    CannotCallNotFunction(Option<Position>),
    NoReturnForType(Option<Position>, Type),
    MismatchTypeInIf {
        opt_pos: Option<Position>,
        then_type: Type,
        else_type: Type,
    },
    CannotMakeFnType(Option<Position>),
    AccessSelfTypeWithoutImpl(Option<Position>),
    AccessSelfWithoutImpl(Option<Position>),
    NotPointer(Option<Position>, Type),
    TypeHasNotMember(Option<Position>, String),
    CannotUseFloatForBitsize(Option<Position>),
    CannotConvertAnyvalueenumToBasicmetadatavalueenum(Option<Position>),
    CannotConvertAnyvalueenumToBasicvalueenum(Option<Position>),
    CannotConvertAnytypeenumToBasictypeenum(Option<Position>),
    NotIntType(Type, Position),
}

impl CodeGenError {
    pub fn condition_is_not_number(opt_pos: Option<Position>, expr: &ExprAST) -> Self {
        Self::ConditionIsNotNumber(opt_pos, expr.clone())
    }

    pub fn already_type_defined_in_struct(opt_pos: Option<Position>, key: &str) -> Self {
        Self::AlreadyTypeDefinedInStruct(opt_pos, key.to_string())
    }

    pub fn already_type_defined_in_union(opt_pos: Option<Position>, key: &str) -> Self {
        Self::AlreadyTypeDefinedInUnion(opt_pos, key.to_string())
    }

    pub fn already_type_defined_in_typedef(opt_pos: Option<Position>, typ: &Type, key: &str) -> Self {
        Self::AlreadyTypeDefinedInTypedef(opt_pos, typ.clone(), key.to_string())
    }

    pub fn union_has_no_field(opt_pos: Option<Position>, opt_id: Option<String>) -> Self {
        Self::UnionHasNoField(opt_pos, opt_id)
    }

    pub fn mismatch_type_union_fields(opt_pos: Option<Position>, opt_id: Option<&str>) -> Self {
        let opt_name = if let Some(s) = opt_id {
            Some(s.to_string())
        }else{
            None
        };
        Self::MismatchTypeUnionFields(opt_pos, opt_name)
    }

    pub fn mismatch_type_struct_fields(opt_pos: Option<Position>, opt_id: Option<&str>)  -> Self {
        let opt_name = if let Some(s) = opt_id {
            Some(s.to_string())
        }else{
            None
        };
        Self::MismatchTypeStructFields(opt_pos, opt_name)
    }

    pub fn no_such_a_type(opt_pos: Option<Position>, name: &str) -> Self {
        Self::NoSuchAType(opt_pos, name.to_string())
    }

    pub fn cannot_convert_to_global_value(opt_pos: Option<Position>) -> Self {
        Self::CannotConvertToGlobalValue(opt_pos)
    }

    pub fn cannot_convert_to_local_pointer(opt_pos: Option<Position>) -> Self {
        Self::CannotConvertToLocalPointer(opt_pos)
    }

    pub fn illegal_type_for_pointer(opt_pos: Option<Position>, typ: &Type) -> Self {
        Self::IllegalTypeForPointer(opt_pos, typ.clone())
    }

    pub fn illegal_end_of_input(opt_pos: Option<Position>) -> Self {
        Self::IllegalEndOfInput(opt_pos)
    }

    pub fn return_without_function(opt_pos: Option<Position>) -> Self {
        Self::ReturnWithoutFunction(opt_pos)
    }

    pub fn return_type_mismatch(opt_pos: Option<Position>, real_type: Type, required_type: Type) -> Self {
        Self::ReturnTypeMismatch {
            opt_pos: opt_pos,
            real_type: real_type,
            required_type: required_type,
        }
    }

    pub fn no_current_function(opt_pos: Option<Position>) -> Self {
        Self::NoCurrentFunction(opt_pos)
    }

    pub fn break_not_in_loop_or_switch(opt_pos: Option<Position>) -> Self {
        Self::BreakNotInLoopOrSwitch(opt_pos)
    }

    pub fn continue_not_in_loop(opt_pos: Option<Position>) -> Self {
        Self::ContinueNotInLoop(opt_pos)
    }

    pub fn no_such_a_label(opt_pos: Option<Position>, id: &str) -> Self {
        Self::NoSuchALabel(opt_pos, id.to_string())
    }

    pub fn no_such_a_variable(opt_pos: Option<Position>, id: &str) -> Self {
        Self::NoSuchAVariable(opt_pos, id.to_string())
    }

    pub fn no_such_a_function(opt_pos: Option<Position>, id: &str) -> Self {
        Self::NoSuchAFunction(opt_pos, id.to_string())
    }

    pub fn no_such_a_struct(opt_pos: Option<Position>, id: &str) -> Self {
        Self::NoSuchAStruct(opt_pos, id.to_string())
    }

    pub fn cannot_get_pointer(opt_pos: Option<Position>) -> Self {
        Self::CannotGetPointer(opt_pos)
    }

    pub fn mismatch_initializer_type(opt_pos: Option<Position>) -> Self {
        Self::MismatchInitializerType(opt_pos)
    }

    pub fn initial_list_is_too_long(opt_pos: Option<Position>) -> Self {
        Self::InitialListIsTooLong(opt_pos)
    }

    pub fn cannot_init_struct_member(opt_pos: Option<Position>) -> Self {
        Self::CannotInitStructMember(opt_pos)
    }

    pub fn cannot_get_zero_value(opt_pos: Option<Position>) -> Self {
        Self::CannotGetZeroValue(opt_pos)
    }

    pub fn case_after_default(opt_pos: Option<Position>) -> Self {
        Self::CaseAfterDefault(opt_pos)
    }

    pub fn already_default_defined(opt_pos: Option<Position>) -> Self {
        Self::AlreadyDefaultDefined(opt_pos)
    }

    pub fn illegal_bit_size(opt_pos: Option<Position>, typ: &NumberType, size: u64) -> Self {
        Self::IllegalBitSize { opt_pos: opt_pos, typ: typ.clone(), size: size }
    }

    pub fn cannot_add_value(opt_pos: Option<Position>, typ: &Type) -> Self {
        Self::CannotAddValue(opt_pos, typ.clone())
    }

    pub fn cannot_sub_value(opt_pos: Option<Position>, typ: &Type) -> Self {
        Self::CannotAddValue(opt_pos, typ.clone())
    }

    pub fn cannot_mul_value(opt_pos: Option<Position>, typ: &Type) -> Self {
        Self::CannotAddValue(opt_pos, typ.clone())
    }

    pub fn cannot_div_value(opt_pos: Option<Position>, typ: &Type) -> Self {
        Self::CannotAddValue(opt_pos, typ.clone())
    }

    pub fn cannot_mod_value(opt_pos: Option<Position>, typ: &Type) -> Self {
        Self::CannotAddValue(opt_pos, typ.clone())
    }

    pub fn cannot_compare_value(opt_pos: Option<Position>, typ: &Type) -> Self {
        Self::CannotAddValue(opt_pos, typ.clone())
    }

    pub fn cannot_apply_logical_op_value(opt_pos: Option<Position>, typ: &Type) -> Self {
        Self::CannotAddValue(opt_pos, typ.clone())
    }

    pub fn not_int_in_shift(opt_pos: Option<Position>, typ: &Type) -> Self {
        Self::NotIntInShift(opt_pos, typ.clone())
    }

    pub fn not_int_bit_and(opt_pos: Option<Position>, typ: &Type) -> Self {
        Self::NotIntInShift(opt_pos, typ.clone())
    }

    pub fn not_int_bit_or(opt_pos: Option<Position>, typ: &Type) -> Self {
        Self::NotIntInShift(opt_pos, typ.clone())
    }

    pub fn not_int_bit_xor(opt_pos: Option<Position>, typ: &Type) -> Self {
        Self::NotIntInShift(opt_pos, typ.clone())
    }

    pub fn assign_illegal_value(opt_pos: Option<Position>, expr: &ExprAST) -> Self {
        Self::AssignIllegalValue(opt_pos, expr.clone())
    }

    pub fn no_such_a_member(opt_pos: Option<Position>, id: &str) -> Self {
        Self::NoSuchAMember(opt_pos, id.to_string())
    }

    pub fn cannot_access_struct_member(opt_pos: Option<Position>, id: &str) -> Self {
        Self::CannotAccessStructMember(opt_pos, id.to_string())
    }

    pub fn not_union(opt_pos: Option<Position>, id: &str) -> Self {
        Self::NotUnion(opt_pos, id.to_string())
    }

    pub fn no_index_value_while_access_array(opt_pos: Option<Position>) -> Self {
        Self::NoIndexValueWhileAccessArray(opt_pos)
    }

    pub fn cannot_call_not_function(opt_pos: Option<Position>, any_val_enum: &AnyValueEnum) -> Self {
        Self::CannotCallNotFunction(opt_pos)
    }

    pub fn no_return_for_type(opt_pos: Option<Position>, typ: &Type) -> Self {
        Self::NoReturnForType(opt_pos, typ.clone())
    }

    pub fn mismatch_type_in_if(opt_pos: Option<Position>, then_type: Type, else_type: Type) -> Self {
        Self::MismatchTypeInIf{
            opt_pos: opt_pos,
            then_type: then_type,
            else_type: else_type,
        }
    }

    pub fn cannot_make_fn_type(opt_pos: Option<Position>) -> Self {
        Self::CannotMakeFnType(opt_pos)
    }

    pub fn access_self_type_without_impl(opt_pos: Option<Position>) -> Self {
        Self::AccessSelfTypeWithoutImpl(opt_pos)
    }

    pub fn access_self_without_impl(opt_pos: Option<Position>) -> Self {
        Self::AccessSelfWithoutImpl(opt_pos)
    }

    pub fn not_pointer(opt_pos: Option<Position>, typ: &Type) -> Self {
        Self::NotPointer(opt_pos, typ.clone())
    }

    pub fn type_has_not_member(opt_pos: Option<Position>, id: &str) -> Self {
        Self::TypeHasNotMember(opt_pos, id.to_string())
    }

    pub fn cannot_use_float_for_bitsize(opt_pos: Option<Position>) -> Self {
        Self::CannotUseFloatForBitsize(opt_pos)
    }

    pub fn cannot_convert_anyvalueenum_to_basicmetadatavalueenum(opt_pos: Option<Position>) -> Self {
        Self::CannotConvertAnyvalueenumToBasicmetadatavalueenum(opt_pos)
    }

    pub fn cannot_convert_anyvalueenum_to_basicvalueenum(opt_pos: Option<Position>) -> Self {
        Self::CannotConvertAnyvalueenumToBasicvalueenum(opt_pos)
    }

    pub fn cannot_convert_anytypeenum_to_basictypeenum(opt_pos: Option<Position>) -> Self {
        Self::CannotConvertAnytypeenumToBasictypeenum(opt_pos)
    }

    pub fn not_int_type(typ: Type, pos: Position) -> Self {
        Self::NotIntType(typ, pos)
    }
}

impl From<ParserError> for CodeGenError {
    fn from(item: ParserError) -> Self {
        CodeGenError::ParserError(item)
    }
}

impl fmt::Display for CodeGenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ParserError(err) => err.fmt(f),



            _ => unimplemented!(),
        }
        
    }
}

impl Error for CodeGenError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

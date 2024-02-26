use parser::{ExprAST, ParserError, NumberType};
use super::{Position, Type};
use std::error::Error;
use std::fmt;
// use inkwell::{values::{BasicValueEnum, BasicMetadataValueEnum, AnyValueEnum, AnyValue, FunctionValue, InstructionOpcode, PointerValue, InstructionValue, BasicValue, IntValue}, types::BasicTypeEnum};
use inkwell::values::AnyValueEnum;

#[derive(Debug, Clone, PartialEq)]
pub enum CodeGenError {
    ParserError(ParserError),
    ConditionIsNotNumber(Position, ExprAST),
    AlreadyTypeDefinedInStruct(Position, String),
    AlreadyTypeDefinedInUnion(Position, String),
    AlreadyTypeDefinedInTypedef(Position, Type, String),
    UnionHasNoField(Position, Option<String>),
    MismatchTypeUnionFields(Position, Option<String>),
    MismatchTypeStructFields(Position, Option<String>),
    NoSuchAType(Position, String),
    CannotConvertToGlobalValue(Position),
    CannotConvertToLocalPointer(Position),
    IllegalTypeForPointer(Position, Type),
    IllegalEndOfInput(Position),
    ReturnWithoutFunction(Position),
    ReturnTypeMismatch {
        pos: Position,
        real_type: Type,
        required_type: Type,
    },
    NoCurrentFunction(Position),
    BreakNotInLoopOrSwitch(Position),
    ContinueNotInLoop(Position),
    NoSuchALabel(Position, String),
    NoSuchAVariable(Position, String),
    NoSuchAFunction(Position, String),
    NoSuchAStruct(Position, String),
    CannotGetPointer(Position),
    MismatchInitializerType(Position),
    InitialListIsTooLong(Position),
    CannotInitStructMember(Position),
    CannotGetZeroValue(Position),
    CaseAfterDefault(Position),
    AlreadyDefaultDefined(Position),
    IllegalBitSize {
        pos: Position,
        typ: NumberType,
        size: usize,
    },
    CannotAddValue(Position, Type),
    CannotSubValue(Position, Type),
    CannotMulValue(Position, Type),
    CannotDivValue(Position, Type),
    CannotModValue(Position, Type),
    CannotCompareValue(Position, Type),
    CannotApplyLogicalOpValue(Position, Type),
    CannotCalculate(Position),
    NotIntInShift(Position, Type),
    NotIntBitAnd(Position, Type),
    NotIntBitOr(Position, Type),
    NotIntBitXor(Position, Type),
    AssignIllegalValue(Position, ExprAST),
    NoSuchAMember(Position, String),
    CannotAccessStructMember(Position, String),
    NotUnion(Position, String),
    NoIndexValueWhileAccessArray(Position),
    CannotCallNotFunction(Position),
    NoReturnForType(Position, Type),
    MismatchTypeInIf {
        pos: Position,
        then_type: Type,
        else_type: Type,
    },
    CannotMakePointerType(Type, Position),
    CannotMakeFnType(Position),
    AccessSelfTypeWithoutImpl(Position),
    AccessSelfWithoutImpl(Position),
    NotPointer(Position, Type),
    TypeHasNotMember(Position, String),
    CannotUseFloatForBitsize(Position),
    CannotConvertAnyvalueenumToBasicmetadatavalueenum(Position),
    CannotConvertAnyvalueenumToBasicvalueenum(Position),
    CannotConvertAnytypeenumToBasictypeenum(Position),
    NotIntType(Type, Position),
    CannotGetSizeOf(Type, Position),
    CannotConvertToBasicValue(ExprAST, Position),
    CannotConvertToBasicType(String, Position),
    NotFunction(String, Position),
    CannotImplicitCast(Type, Type, Position),
    CannotCast(Type, Type, Position),
    SelfIsNotStatement(Position),
    SelfHasNotLeftValue(Position),
    HasNotLeftValue(String, Position),
    HasNotMember(String, Position),
}

impl CodeGenError {
    pub fn condition_is_not_number(pos: Position, expr: &ExprAST) -> Self {
        Self::ConditionIsNotNumber(pos, expr.clone())
    }

    pub fn already_type_defined_in_struct(pos: Position, key: &str) -> Self {
        Self::AlreadyTypeDefinedInStruct(pos, key.to_string())
    }

    pub fn already_type_defined_in_union(pos: Position, key: &str) -> Self {
        Self::AlreadyTypeDefinedInUnion(pos, key.to_string())
    }

    pub fn already_type_defined_in_typedef(pos: Position, typ: &Type, key: &str) -> Self {
        Self::AlreadyTypeDefinedInTypedef(pos, typ.clone(), key.to_string())
    }

    pub fn union_has_no_field(pos: Position, opt_id: Option<String>) -> Self {
        Self::UnionHasNoField(pos, opt_id)
    }

    pub fn mismatch_type_union_fields(pos: Position, opt_id: Option<&str>) -> Self {
        let opt_name = if let Some(s) = opt_id {
            Some(s.to_string())
        }else{
            None
        };
        Self::MismatchTypeUnionFields(pos, opt_name)
    }

    pub fn mismatch_type_struct_fields(pos: Position, opt_id: Option<&str>)  -> Self {
        let opt_name = if let Some(s) = opt_id {
            Some(s.to_string())
        }else{
            None
        };
        Self::MismatchTypeStructFields(pos, opt_name)
    }

    pub fn no_such_a_type(pos: Position, name: &str) -> Self {
        Self::NoSuchAType(pos, name.to_string())
    }

    pub fn cannot_convert_to_global_value(pos: Position) -> Self {
        Self::CannotConvertToGlobalValue(pos)
    }

    pub fn cannot_convert_to_local_pointer(pos: Position) -> Self {
        Self::CannotConvertToLocalPointer(pos)
    }

    pub fn illegal_type_for_pointer(pos: Position, typ: &Type) -> Self {
        Self::IllegalTypeForPointer(pos, typ.clone())
    }

    pub fn illegal_end_of_input(pos: Position) -> Self {
        Self::IllegalEndOfInput(pos)
    }

    pub fn return_without_function(pos: Position) -> Self {
        Self::ReturnWithoutFunction(pos)
    }

    pub fn return_type_mismatch(pos: Position, real_type: Type, required_type: Type) -> Self {
        Self::ReturnTypeMismatch {
            pos: pos,
            real_type: real_type,
            required_type: required_type,
        }
    }

    pub fn no_current_function(pos: Position) -> Self {
        Self::NoCurrentFunction(pos)
    }

    pub fn break_not_in_loop_or_switch(pos: Position) -> Self {
        Self::BreakNotInLoopOrSwitch(pos)
    }

    pub fn continue_not_in_loop(pos: Position) -> Self {
        Self::ContinueNotInLoop(pos)
    }

    pub fn no_such_a_label(pos: Position, id: &str) -> Self {
        Self::NoSuchALabel(pos, id.to_string())
    }

    pub fn no_such_a_variable(pos: Position, id: &str) -> Self {
        Self::NoSuchAVariable(pos, id.to_string())
    }

    pub fn no_such_a_function(pos: Position, id: &str) -> Self {
        Self::NoSuchAFunction(pos, id.to_string())
    }

    pub fn no_such_a_struct(pos: Position, id: &str) -> Self {
        Self::NoSuchAStruct(pos, id.to_string())
    }

    pub fn cannot_get_pointer(pos: Position) -> Self {
        Self::CannotGetPointer(pos)
    }

    pub fn mismatch_initializer_type(pos: Position) -> Self {
        Self::MismatchInitializerType(pos)
    }

    pub fn initial_list_is_too_long(pos: Position) -> Self {
        Self::InitialListIsTooLong(pos)
    }

    pub fn cannot_init_struct_member(pos: Position) -> Self {
        Self::CannotInitStructMember(pos)
    }

    pub fn cannot_get_zero_value(pos: Position) -> Self {
        Self::CannotGetZeroValue(pos)
    }

    pub fn case_after_default(pos: Position) -> Self {
        Self::CaseAfterDefault(pos)
    }

    pub fn already_default_defined(pos: Position) -> Self {
        Self::AlreadyDefaultDefined(pos)
    }

    pub fn illegal_bit_size(pos: Position, typ: &NumberType, size: usize) -> Self {
        Self::IllegalBitSize { pos: pos, typ: typ.clone(), size: size }
    }

    pub fn cannot_add_value(pos: Position, typ: &Type) -> Self {
        Self::CannotAddValue(pos, typ.clone())
    }

    pub fn cannot_sub_value(pos: Position, typ: &Type) -> Self {
        Self::CannotSubValue(pos, typ.clone())
    }

    pub fn cannot_mul_value(pos: Position, typ: &Type) -> Self {
        Self::CannotMulValue(pos, typ.clone())
    }

    pub fn cannot_div_value(pos: Position, typ: &Type) -> Self {
        Self::CannotDivValue(pos, typ.clone())
    }

    pub fn cannot_mod_value(pos: Position, typ: &Type) -> Self {
        Self::CannotModValue(pos, typ.clone())
    }

    pub fn cannot_compare_value(pos: Position, typ: &Type) -> Self {
        Self::CannotCompareValue(pos, typ.clone())
    }

    pub fn cannot_apply_logical_op_value(pos: Position, typ: &Type) -> Self {
        Self::CannotApplyLogicalOpValue(pos, typ.clone())
    }

    pub fn cannot_calculate(pos: Position) -> Self {
        Self::CannotCalculate(pos)
    }

    pub fn not_int_in_shift(pos: Position, typ: &Type) -> Self {
        Self::NotIntInShift(pos, typ.clone())
    }

    pub fn not_int_bit_and(pos: Position, typ: &Type) -> Self {
        Self::NotIntInShift(pos, typ.clone())
    }

    pub fn not_int_bit_or(pos: Position, typ: &Type) -> Self {
        Self::NotIntInShift(pos, typ.clone())
    }

    pub fn not_int_bit_xor(pos: Position, typ: &Type) -> Self {
        Self::NotIntInShift(pos, typ.clone())
    }

    pub fn assign_illegal_value(pos: Position, expr: &ExprAST) -> Self {
        Self::AssignIllegalValue(pos, expr.clone())
    }

    pub fn no_such_a_member(pos: Position, id: &str) -> Self {
        Self::NoSuchAMember(pos, id.to_string())
    }

    pub fn cannot_access_struct_member(pos: Position, id: &str) -> Self {
        Self::CannotAccessStructMember(pos, id.to_string())
    }

    pub fn not_union(pos: Position, id: &str) -> Self {
        Self::NotUnion(pos, id.to_string())
    }

    pub fn no_index_value_while_access_array(pos: Position) -> Self {
        Self::NoIndexValueWhileAccessArray(pos)
    }

    pub fn cannot_call_not_function(pos: Position, _any_val_enum: &AnyValueEnum) -> Self {
        Self::CannotCallNotFunction(pos)
    }

    pub fn no_return_for_type(pos: Position, typ: &Type) -> Self {
        Self::NoReturnForType(pos, typ.clone())
    }

    pub fn mismatch_type_in_if(pos: Position, then_type: Type, else_type: Type) -> Self {
        Self::MismatchTypeInIf{
            pos: pos,
            then_type: then_type,
            else_type: else_type,
        }
    }

    pub fn cannot_make_fn_type(pos: Position) -> Self {
        Self::CannotMakeFnType(pos)
    }

    pub fn access_self_type_without_impl(pos: Position) -> Self {
        Self::AccessSelfTypeWithoutImpl(pos)
    }

    pub fn access_self_without_impl(pos: Position) -> Self {
        Self::AccessSelfWithoutImpl(pos)
    }

    pub fn not_pointer(pos: Position, typ: &Type) -> Self {
        Self::NotPointer(pos, typ.clone())
    }

    pub fn type_has_not_member(pos: Position, id: &str) -> Self {
        Self::TypeHasNotMember(pos, id.to_string())
    }

    pub fn cannot_use_float_for_bitsize(pos: Position) -> Self {
        Self::CannotUseFloatForBitsize(pos)
    }

    pub fn cannot_convert_anyvalueenum_to_basicmetadatavalueenum(pos: Position) -> Self {
        Self::CannotConvertAnyvalueenumToBasicmetadatavalueenum(pos)
    }

    pub fn cannot_convert_anyvalueenum_to_basicvalueenum(pos: Position) -> Self {
        Self::CannotConvertAnyvalueenumToBasicvalueenum(pos)
    }

    pub fn cannot_convert_anytypeenum_to_basictypeenum(pos: Position) -> Self {
        Self::CannotConvertAnytypeenumToBasictypeenum(pos)
    }

    pub fn not_int_type(typ: Type, pos: Position) -> Self {
        Self::NotIntType(typ, pos)
    }

    pub fn cannot_get_size_of(typ: Type, pos: Position) -> Self {
        Self::CannotGetSizeOf(typ, pos)
    }

    pub fn cannot_convert_to_basic_value(expr: ExprAST, pos: Position) -> Self {
        Self::CannotConvertToBasicValue(expr, pos)
    }

    pub fn cannot_convert_to_basic_type(name: String, pos: Position) -> Self {
        Self::CannotConvertToBasicType(name, pos)
    }

    pub fn not_function(name: &str, pos: Position) -> Self {
        Self::NotFunction(name.to_string(), pos)
    }

    pub fn cannot_implicit_cast(from_type: Type, to_type: Type, pos: Position) -> Self {
        Self::CannotImplicitCast(from_type, to_type, pos)
    }

    pub fn cannot_cast(from_type: Type, to_type: Type, pos: Position) -> Self {
        Self::CannotImplicitCast(from_type, to_type, pos)
    }

    pub fn cannot_make_pointer_type(typ: Type, pos: Position) -> Self {
        Self::CannotMakePointerType(typ, pos)
    }

    pub fn self_is_not_statement(pos: Position) -> Self {
        Self::SelfIsNotStatement(pos)
    }

    pub fn self_has_not_l_value(pos: Position) -> Self {
        Self::SelfHasNotLeftValue(pos)
    }

    pub fn has_not_l_value(name: String, pos: Position) -> Self {
        Self::HasNotLeftValue(name, pos)
    }

    pub fn has_not_member(name: String, pos: Position) -> Self {
        Self::HasNotMember(name, pos)
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
            Self::ConditionIsNotNumber(_pos, expr_ast) => {
                write!(f, "condition {:?} is not a number", expr_ast)
            },
            Self::AlreadyTypeDefinedInStruct(_pos, id) => {
                write!(f, "already type defined in struct. {id}")
            },
            Self::AlreadyTypeDefinedInUnion(_pos, id) => {
                write!(f, "already type defined in union. {id}")
            },
            Self::AlreadyTypeDefinedInTypedef(_pos, _typ, id) => {
                write!(f, "already type defined {id}")
            },
            Self::UnionHasNoField(_pos, opt_id) => {
                if let Some(id) = opt_id {
                    write!(f, "union {id} has no field")
                }else{
                    write!(f, "union has no field")
                }
            },
            Self::MismatchTypeUnionFields(_pos, opt_id) => {
                if let Some(id) = opt_id {
                    write!(f, "mismatch type union fields {id}")
                }else{
                    write!(f, "mismatch type union fields")
                }
            },
            Self::MismatchTypeStructFields(_pos, opt_id) => {
                if let Some(id) = opt_id {
                    write!(f, "mismatch type struct fields. {id}")
                }else{
                    write!(f, "mismatch type struct fields")
                }
            },
            Self::NoSuchAType(_pos, id) => {
                write!(f, "no such a type '{id}'")
            },
            Self::CannotConvertToGlobalValue(_pos) => {
                write!(f, "cannot convert to global value")
            },
            Self::CannotConvertToLocalPointer(_pos) => {
                write!(f, "cannot convert to local pointer")
            },
            Self::IllegalTypeForPointer(_pos, typ) => {
                write!(f, "illegal type for pointer")
            },
            Self::IllegalEndOfInput(_pos) => {
                write!(f, "illegal end of input")
            },
            Self::ReturnWithoutFunction(_pos) => {
                write!(f, "return without function")
            },
            Self::ReturnTypeMismatch {
                    pos: _,
                    real_type,
                    required_type,
                } =>
            {
                write!(f, "return type mismatch required '{required_type}', but '{real_type}'")
            },
            Self::NoCurrentFunction(_pos) => {
                write!(f, "no current function")
            },
            Self::BreakNotInLoopOrSwitch(_pos) => {
                write!(f, "break not in loop or switch")
            },
            Self::ContinueNotInLoop(_pos) => {
                write!(f, "continue not in loop")
            },
            Self::NoSuchALabel(_pos, id) => {
                write!(f, "no such a label '{id}'")
            },
            Self::NoSuchAVariable(_pos, id) => {
                write!(f, "no such a variable '{id}'")
            },
            Self::NoSuchAFunction(_pos, id) => {
                write!(f, "no such a function '{id}'")
            },
            Self::NoSuchAStruct(_pos, id) => {
                write!(f, "no such a struct '{id}'")
            },
            Self::CannotGetPointer(_pos) => {
                write!(f, "cannot get pointer")
            },
            Self::MismatchInitializerType(_pos) => {
                write!(f, "mismatch initializer type")
            },
            Self::InitialListIsTooLong(_pos) => {
                write!(f, "initial list is too long")
            },
            Self::CannotInitStructMember(_pos) => {
                write!(f, "cannot init struct member")
            },
            Self::CannotGetZeroValue(_pos) => {
                write!(f, "cannot get zero value")
            },
            Self::CaseAfterDefault(_pos) => {
                write!(f, "case after default")
            },
            Self::AlreadyDefaultDefined(_pos) => {
                write!(f, "already default defined")
            },
            Self::IllegalBitSize {
                pos: _,
                typ,
                size: _,
            } => {
                write!(f, "illegal bit size for type {typ}")
            },
            Self::CannotAddValue(_pos, typ) => {
                write!(f, "cannot add for type {typ}")
            },
            Self::CannotSubValue(_pos, typ) => {
                write!(f, "cannot sub for type {typ}")
            },
            Self::CannotMulValue(_pos, typ) => {
                write!(f, "cannot mul for type {typ}")
            },
            Self::CannotDivValue(_pos, typ) => {
                write!(f, "cannot div for type {typ}")
            },
            Self::CannotModValue(_pos, typ) => {
                write!(f, "cannot mod for type {typ}")
            },
            Self::CannotCompareValue(_pos, typ) => {
                write!(f, "cannot compare value for type {typ}")
            },
            Self::CannotApplyLogicalOpValue(_pos, typ) => {
                write!(f, "cannot apply logical op value for type {typ}")
            },
            Self::CannotCalculate(_pos) => {
                write!(f, "cannot calculate")
            },
            Self::NotIntInShift(_pos, typ) => {
                write!(f, "type {typ} cannot shift")
            },
            Self::NotIntBitAnd(_pos, typ) => {
                write!(f, "type {typ} cannot bit-and")
            },
            Self::NotIntBitOr(_pos, typ) => {
                write!(f, "type {typ} cannot bit-or")
            },
            Self::NotIntBitXor(_pos, typ) => {
                write!(f, "type {typ} cannot bit-xor")
            },
            Self::AssignIllegalValue(_pos, expr_ast) => {
                write!(f, "assign illegal value {:?}", expr_ast)
            },
            Self::NoSuchAMember(_pos, id) => {
                write!(f, "no such a member '{id}'")
            },
            Self::CannotAccessStructMember(_pos, id) => {
                write!(f, "{id} cannot access struct member")
            },
            Self::NotUnion(_pos, id) => {
                write!(f, "{id} is not union")
            },
            Self::NoIndexValueWhileAccessArray(_pos) => {
                write!(f, "no index value while access array")
            },
            Self::CannotCallNotFunction(_pos) => {
                write!(f, "cannot call non-function")
            },
            Self::NoReturnForType(_pos, typ) => {
                write!(f, "no return for type {typ}")
            },
            Self::MismatchTypeInIf {
                pos: _,
                then_type,
                else_type,
            } => {
                write!(f, "mismatch type if if-statement. then type is {then_type}, else type is {else_type}")
            },
            Self::CannotMakePointerType(typ, _pos) => {
                write!(f, "{typ} cannot make pointer type")
            },
            Self::CannotMakeFnType(_pos) => {
                write!(f, "cannot make FnType")
            },
            Self::AccessSelfTypeWithoutImpl(_pos) => {
                write!(f, "access Self type without impl")
            },
            Self::AccessSelfWithoutImpl(_pos) => {
                write!(f, "access self without impl")
            },
            Self::NotPointer(_pos, typ) => {
                write!(f, "{typ} is not pointer")
            },
            Self::TypeHasNotMember(_pos, id) => {
                write!(f, "type {id} has not member")
            },
            Self::CannotUseFloatForBitsize(_pos) => {
                write!(f, "cannot use float for bitsize")
            },
            Self::CannotConvertAnyvalueenumToBasicmetadatavalueenum(_pos) => {
                write!(f, "cannot convert anyvalueenum to basicmetadatavalueenum")
            },
            Self::CannotConvertAnyvalueenumToBasicvalueenum(_pos) => {
                write!(f, "cannot convert anyvalueenum to basicvalueenum")
            },
            Self::CannotConvertAnytypeenumToBasictypeenum(_pos) => {
                write!(f, "cannot convert anytypeenum to basictypeenum")
            },
            Self::NotIntType(typ, _pos) => {
                write!(f, "{typ} is not int type")
            },
            Self::CannotGetSizeOf(typ, _pos) => {
                write!(f, "cannot get size of {typ}")
            },
            Self::CannotConvertToBasicValue(expr_ast, _pos) => {
                write!(f, "{:?} cannot convert to basic value", expr_ast)
            },
            Self::CannotConvertToBasicType(id, _pos) => {
                write!(f, "{id} cannot convert to basic type")
            },
            Self::NotFunction(id, _pos) => {
                write!(f, "{id} is not function")
            },
            Self::CannotImplicitCast(typ1, typ2, _pos) => {
                write!(f, "cannot implicit cast from {typ1} to {typ2}")
            },
            Self::CannotCast(typ1, typ2, _pos) => {
                write!(f, "cannot cast from {typ1} to {typ2}")
            },
            Self::SelfIsNotStatement(_pos) => {
                write!(f, "Self is not statement")
            },
            Self::SelfHasNotLeftValue(_pos) => {
                write!(f, "Self has not left value")
            },
            Self::HasNotLeftValue(id, _pos) => {
                write!(f, "{id} has not left value")
            },
            Self::HasNotMember(id, _pos) => {
                write!(f, "{id} has not member")
            },
        }
        
    }
}

impl Error for CodeGenError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

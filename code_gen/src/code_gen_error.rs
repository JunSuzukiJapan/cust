use parser::{ExprAST, ParserError, NumberType};
use super::{Position, Type};
use std::error::Error;
use std::fmt;
use inkwell::values::AnyValueEnum;

#[derive(Debug, Clone, PartialEq)]
pub enum CodeGenError {
    ParserError(ParserError),
    SystemError(Position),
    ConditionIsNotNumber(ExprAST, Position),
    AlreadyTypeDefinedInStruct(String, Position),
    AlreadyTypeDefinedInUnion(String, Position),
    AlreadyTypeDefinedInTypedef(Type, String, Position),
    AlreadyTypeDefinedInEnum(String, Position),
    UnionHasNoField(Option<String>, Position),
    EnumHasNoField(String, Position),
    MismatchTypeUnionFields(Option<String>, Position),
    MismatchTypeStructFields(Option<String>, Position),
    MismatchTypeEnumFields(String, Position),
    NoSuchAType(String, Position),
    CannotConvertToGlobalValue(Position),
    CannotConvertToLocalPointer(Position),
    IllegalTypeForPointer(Type, Position),
    IllegalEndOfInput(Position),
    ReturnWithoutFunction(Position),
    ReturnTypeMismatch {
        real_type: Type,
        required_type: Type,
        pos: Position,
    },
    NoCurrentFunction(Position),
    BreakNotInLoopOrSwitch(Position),
    ContinueNotInLoop(Position),
    NoSuchALabel(String, Position),
    NoSuchAVariable(String, Position),
    NoSuchAFunction(String, Position),
    NoSuchAMemberFunction(String, String, Position),  // (class_name, method_name, position)
    NoSuchAClassFunction(String, String, Position),  // (class_name, method_name, position)
    NoSuchAStruct(String, Position),
    CannotGetPointer(Position),
    MismatchInitializerType(Type, Type, Position),
    InitializerIsNone(Position),
    InitializerIsNotArray(Position),
    InitializerIsNotStruct(Position),
    InitialListIsTooLong(Position),
    CannotInitStructMember(Position),
    CannotGetZeroValue(Position),
    CaseAfterDefault(Position),
    AlreadyDefaultDefined(Position),
    IllegalBitSize {
        typ: NumberType,
        size: usize,
        pos: Position,
    },
    CannotAddValue(Type, Type, Position),
    CannotSubValue(Type, Type, Position),
    CannotMulValue(Type, Type, Position),
    CannotDivValue(Type, Type, Position),
    CannotModValue(Type, Type, Position),
    CannotCompareValue(Type, Position),
    CannotApplyLogicalOpValue(Type, Position),
    CannotCalculate(Position),
    NotIntInShift(Type, Position),
    NotIntBitAnd(Type, Position),
    NotIntBitOr(Type, Position),
    NotIntBitXor(Type, Position),
    AssignIllegalValue(ExprAST, Position),
    NoSuchAMember(Option<String>, String, Position),  // (type name, member name, position)
    CannotAccessStructMember(String, Position),
    NotUnion(String, Position),
    NoIndexValueWhileAccessArray(Position),
    CannotCallNotFunction(Position),
    NoReturnForType(Type, Position),
    MismatchTypeInIf {
        then_type: Type,
        else_type: Type,
        pos: Position,
    },
    CannotMakePointerType(Type, Position),
    CannotMakeFnType(Position),
    AccessSelfTypeWithoutImpl(Position),
    AccessSelfWithoutImpl(Position),
    NotPointer(Type, Position),
    TypeHasNotMember(String, Position),
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
    HasNotMember(String, String, Position),  // (type name, member name, Position)
    NotArray(ExprAST, Position),
    ArrayIndexIsTooLong(Position),
    AlreadyClassFunctionDefined(String, String, Position),
    AlreadyMemberFunctionDefined(String, String, Position),
    AlreadyClassVarDefined(String, String, Position),
    NoSuchAClassVar(String, String, Position),
    NoCurrentClass(Position),
    CannotAssignConstant(Position),
    NoSuchAEnumMember(String, String, Position),
}

impl CodeGenError {
    pub fn condition_is_not_number(expr: &ExprAST, pos: Position) -> Self {
        Self::ConditionIsNotNumber(expr.clone(), pos)
    }

    pub fn system_error(pos: Position) -> Self {
        Self::SystemError(pos)
    }

    pub fn already_type_defined_in_struct(key: &str, pos: Position) -> Self {
        Self::AlreadyTypeDefinedInStruct(key.to_string(), pos)
    }

    pub fn already_type_defined_in_union(key: &str, pos: Position) -> Self {
        Self::AlreadyTypeDefinedInUnion(key.to_string(), pos)
    }

    pub fn already_type_defined_in_typedef(typ: &Type, key: &str, pos: Position) -> Self {
        Self::AlreadyTypeDefinedInTypedef(typ.clone(), key.to_string(), pos)
    }

    pub fn already_type_defined_in_enum(key: &str, pos: Position) -> Self {
        Self::AlreadyTypeDefinedInEnum(key.to_string(), pos)
    }

    pub fn union_has_no_field(opt_id: Option<String>, pos: Position) -> Self {
        Self::UnionHasNoField(opt_id, pos)
    }

    pub fn enum_has_no_field(id: String, pos: Position) -> Self {
        Self::EnumHasNoField(id, pos)
    }

    pub fn mismatch_type_union_fields(opt_id: Option<&str>, pos: Position) -> Self {
        let opt_name = if let Some(s) = opt_id {
            Some(s.to_string())
        }else{
            None
        };
        Self::MismatchTypeUnionFields(opt_name, pos)
    }

    pub fn mismatch_type_struct_fields(opt_id: Option<&str>, pos: Position)  -> Self {
        let opt_name = if let Some(s) = opt_id {
            Some(s.to_string())
        }else{
            None
        };
        Self::MismatchTypeStructFields(opt_name, pos)
    }

    pub fn mismatch_type_enum_fields(id: String, pos: Position) -> Self {
        Self::MismatchTypeEnumFields(id, pos)
    }

    pub fn no_such_a_type(name: &str, pos: Position) -> Self {
        Self::NoSuchAType(name.to_string(), pos)
    }

    pub fn cannot_convert_to_global_value(pos: Position) -> Self {
        Self::CannotConvertToGlobalValue(pos)
    }

    pub fn cannot_convert_to_local_pointer(pos: Position) -> Self {
        Self::CannotConvertToLocalPointer(pos)
    }

    pub fn illegal_type_for_pointer(typ: &Type, pos: Position) -> Self {
        Self::IllegalTypeForPointer(typ.clone(), pos)
    }

    pub fn illegal_end_of_input(pos: Position) -> Self {
        Self::IllegalEndOfInput(pos)
    }

    pub fn return_without_function(pos: Position) -> Self {
        Self::ReturnWithoutFunction(pos)
    }

    pub fn return_type_mismatch(real_type: Type, required_type: Type, pos: Position) -> Self {
        Self::ReturnTypeMismatch {
            real_type: real_type,
            required_type: required_type,
            pos: pos,
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

    pub fn no_such_a_label(id: &str, pos: Position) -> Self {
        Self::NoSuchALabel(id.to_string(), pos)
    }

    pub fn no_such_a_variable(id: &str, pos: Position) -> Self {
        Self::NoSuchAVariable(id.to_string(), pos)
    }

    pub fn no_such_a_function(id: &str, pos: Position) -> Self {
        Self::NoSuchAFunction(id.to_string(), pos)
    }

    pub fn no_such_a_member_function(class_name: String, method_name: String, pos: Position) -> Self {
        Self::NoSuchAMemberFunction(class_name, method_name, pos)
    }

    pub fn no_such_a_class_function(class_name: String, member_name: String, pos: Position) -> Self {
        Self::NoSuchAClassFunction(class_name, member_name, pos)
    }

    pub fn no_such_a_struct(id: &str, pos: Position) -> Self {
        Self::NoSuchAStruct(id.to_string(), pos)
    }

    pub fn cannot_get_pointer(pos: Position) -> Self {
        Self::CannotGetPointer(pos)
    }

    pub fn mismatch_initializer_type(require_type: &Type, real_type: &Type, pos: Position) -> Self {
        Self::MismatchInitializerType(require_type.clone(), real_type.clone(), pos)
    }

    pub fn initializer_is_none(pos: Position) -> Self {
        Self::InitializerIsNone(pos)
    }

    pub fn initializer_is_not_array(pos: Position) -> Self {
        Self::InitializerIsNotArray(pos)
    }

    pub fn initializer_is_not_struct(pos: Position) -> Self {
        Self::InitializerIsNotStruct(pos)
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

    pub fn illegal_bit_size(typ: &NumberType, size: usize, pos: Position) -> Self {
        Self::IllegalBitSize { typ: typ.clone(), size: size, pos: pos }
    }

    pub fn cannot_add_value(left_type: &Type, right_type: &Type, pos: Position) -> Self {
        Self::CannotAddValue(left_type.clone(), right_type.clone(), pos)
    }

    pub fn cannot_sub_value(left_type: &Type, right_type: &Type, pos: Position) -> Self {
        Self::CannotSubValue(left_type.clone(), right_type.clone(), pos)
    }

    pub fn cannot_mul_value(left_type: &Type, right_type: &Type, pos: Position) -> Self {
        Self::CannotMulValue(left_type.clone(), right_type.clone(), pos)
    }

    pub fn cannot_div_value(left_type: &Type, right_type: &Type, pos: Position) -> Self {
        Self::CannotDivValue(left_type.clone(), right_type.clone(), pos)
    }

    pub fn cannot_mod_value(left_type: &Type, right_type: &Type, pos: Position) -> Self {
        Self::CannotModValue(left_type.clone(), right_type.clone(), pos)
    }

    pub fn cannot_compare_value(typ: &Type, pos: Position) -> Self {
        Self::CannotCompareValue(typ.clone(), pos)
    }

    pub fn cannot_apply_logical_op_value(typ: &Type, pos: Position) -> Self {
        Self::CannotApplyLogicalOpValue(typ.clone(), pos)
    }

    pub fn cannot_calculate(pos: Position) -> Self {
        Self::CannotCalculate(pos)
    }

    pub fn not_int_in_shift(typ: &Type, pos: Position) -> Self {
        Self::NotIntInShift(typ.clone(), pos)
    }

    pub fn not_int_bit_and(typ: &Type, pos: Position) -> Self {
        Self::NotIntInShift(typ.clone(), pos)
    }

    pub fn not_int_bit_or(typ: &Type, pos: Position) -> Self {
        Self::NotIntInShift(typ.clone(), pos)
    }

    pub fn not_int_bit_xor(typ: &Type, pos: Position) -> Self {
        Self::NotIntInShift(typ.clone(), pos)
    }

    pub fn assign_illegal_value(expr: &ExprAST, pos: Position) -> Self {
        Self::AssignIllegalValue(expr.clone(), pos)
    }

    pub fn no_such_a_member(type_name: &Option<String>, member_name: &str, pos: Position) -> Self {
        if let Some(name) = type_name {
            Self::NoSuchAMember(Some(name.to_string()), member_name.to_string(), pos)
        }else{
            Self::NoSuchAMember(None, member_name.to_string(), pos)
        }
    }

    pub fn cannot_access_struct_member(id: &str, pos: Position) -> Self {
        Self::CannotAccessStructMember(id.to_string(), pos)
    }

    pub fn not_union(id: &str, pos: Position) -> Self {
        Self::NotUnion(id.to_string(), pos)
    }

    pub fn no_index_value_while_access_array(pos: Position) -> Self {
        Self::NoIndexValueWhileAccessArray(pos)
    }

    pub fn cannot_call_not_function(_any_val_enum: &AnyValueEnum, pos: Position) -> Self {
        Self::CannotCallNotFunction(pos)
    }

    pub fn no_return_for_type(typ: &Type, pos: Position) -> Self {
        Self::NoReturnForType(typ.clone(), pos)
    }

    pub fn mismatch_type_in_if(pos: Position, then_type: Type, else_type: Type) -> Self {
        Self::MismatchTypeInIf{
            then_type: then_type,
            else_type: else_type,
            pos: pos,
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

    pub fn not_pointer(typ: &Type, pos: Position) -> Self {
        Self::NotPointer(typ.clone(), pos)
    }

    pub fn type_has_not_member(id: &str, pos: Position) -> Self {
        Self::TypeHasNotMember(id.to_string(), pos)
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

    pub fn has_not_member(type_name: String, member_name: String, pos: Position) -> Self {
        Self::HasNotMember(type_name, member_name, pos)
    }

    pub fn not_array(expr: ExprAST, pos: Position) -> Self {
        Self::NotArray(expr, pos)
    }

    pub fn array_index_is_too_long(pos: Position) -> Self {
        Self::ArrayIndexIsTooLong(pos)
    }

    pub fn already_class_function_defined(class_name: String, func_name: String, pos: Position) -> Self {
        Self::AlreadyClassFunctionDefined(class_name, func_name, pos)
    }

    pub fn already_member_function_defined(class_name: String, func_name: String, pos: Position) -> Self {
        Self::AlreadyMemberFunctionDefined(class_name, func_name, pos)
    }

    pub fn already_class_var_defined(class_name: String, var_name: String, pos: Position) -> Self {
        Self::AlreadyClassVarDefined(class_name, var_name, pos)
    }

    pub fn no_such_a_class_var(class_name: String, var_name: String, pos: Position) -> Self {
        Self::NoSuchAClassVar(class_name, var_name, pos)
    }

    pub fn no_current_class(pos: Position) -> Self {
        Self::NoCurrentClass(pos)
    }

    pub fn cannot_assign_constant(pos: Position) -> Self {
        Self::CannotAssignConstant(pos)
    }

    pub fn no_such_a_enum_member(enum_name: String, member_name: String, pos: Position) -> Self {
        Self::NoSuchAEnumMember(enum_name, member_name, pos)
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
            Self::SystemError(pos) => {
                write!(f, "system error")
            },
            Self::ConditionIsNotNumber(expr_ast, _pos) => {
                write!(f, "condition {:?} is not a number", expr_ast)
            },
            Self::AlreadyTypeDefinedInStruct(id, _pos) => {
                write!(f, "already type defined in struct. {id}")
            },
            Self::AlreadyTypeDefinedInUnion(id, _pos) => {
                write!(f, "already type defined in union. {id}")
            },
            Self::AlreadyTypeDefinedInTypedef(_typ, id, _pos) => {
                write!(f, "already type defined {id}")
            },
            Self::AlreadyTypeDefinedInEnum(id, _pos) => {
                write!(f, "already type defined in enum. {id}")
            },
            Self::UnionHasNoField(opt_id, _pos) => {
                if let Some(id) = opt_id {
                    write!(f, "union {id} has no field")
                }else{
                    write!(f, "union has no field")
                }
            },
            Self::EnumHasNoField(id, _pos) => {
                write!(f, "enum {id} has no field")
            },
            Self::MismatchTypeUnionFields(opt_id, _pos) => {
                if let Some(id) = opt_id {
                    write!(f, "mismatch type union fields {id}")
                }else{
                    write!(f, "mismatch type union fields")
                }
            },
            Self::MismatchTypeStructFields(opt_id, _pos) => {
                if let Some(id) = opt_id {
                    write!(f, "mismatch type struct fields. {id}")
                }else{
                    write!(f, "mismatch type struct fields")
                }
            },
            Self::MismatchTypeEnumFields(id, _pos) => {
                write!(f, "mismatch type enum fields {id}")
            },
            Self::NoSuchAType(id, _pos) => {
                write!(f, "no such a type '{id}'")
            },
            Self::CannotConvertToGlobalValue(_pos) => {
                write!(f, "cannot convert to global value")
            },
            Self::CannotConvertToLocalPointer(_pos) => {
                write!(f, "cannot convert to local pointer")
            },
            Self::IllegalTypeForPointer(_typ, _pos) => {
                write!(f, "illegal type for pointer")
            },
            Self::IllegalEndOfInput(_pos) => {
                write!(f, "illegal end of input")
            },
            Self::ReturnWithoutFunction(_pos) => {
                write!(f, "return without function")
            },
            Self::ReturnTypeMismatch {
                    real_type,
                    required_type,
                    pos: _,
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
            Self::NoSuchALabel(id, _pos) => {
                write!(f, "no such a label '{id}'")
            },
            Self::NoSuchAVariable(id, _pos) => {
                write!(f, "no such a variable '{id}'")
            },
            Self::NoSuchAFunction(id, _pos) => {
                write!(f, "no such a function '{id}'")
            },
            Self::NoSuchAMemberFunction(class_name, method_name, _pos) => {
                write!(f, "no such a member function '{method_name}' in struct '{class_name}'")
            },
            Self::NoSuchAClassFunction(class_name, method_name, _pos) => {
                write!(f, "no such a class function '{method_name}' in struct '{class_name}'")
            }
            Self::NoSuchAStruct(id, _pos) => {
                write!(f, "no such a struct '{id}'")
            },
            Self::CannotGetPointer(_pos) => {
                write!(f, "cannot get pointer")
            },
            Self::MismatchInitializerType(require_type, real_type, _pos) => {
                write!(f, "mismatch initializer type. require type: {:?}, real initializer: {:?}", require_type, real_type)
            },
            Self::InitializerIsNone(_pos) => {
                write!(f, "initializer is none")
            },
            Self::InitializerIsNotArray(_pos) => {
                write!(f, "initializer is not array")
            },
            Self::InitializerIsNotStruct(_pos) => {
                write!(f, "initializer is not struct")
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
                typ,
                size: _,
                pos: _,
            } => {
                write!(f, "illegal bit size for type {typ}")
            },
            Self::CannotAddValue(l_type, r_type, _pos) => {
                write!(f, "cannot add. type {l_type} and {r_type}")
            },
            Self::CannotSubValue(l_type, r_type, _pos) => {
                write!(f, "cannot sub. type {l_type} and {r_type}")
            },
            Self::CannotMulValue(l_type, r_type, _pos) => {
                write!(f, "cannot mul. type {l_type} and {r_type}")
            },
            Self::CannotDivValue(l_type, r_type, _pos) => {
                write!(f, "cannot div. type {l_type} and {r_type}")
            },
            Self::CannotModValue(l_type, r_type, _pos) => {
                write!(f, "cannot mod. type {l_type} by {r_type}")
            },
            Self::CannotCompareValue(typ, _pos) => {
                write!(f, "cannot compare value for type {typ}")
            },
            Self::CannotApplyLogicalOpValue(typ, _pos) => {
                write!(f, "cannot apply logical op value for type {typ}")
            },
            Self::CannotCalculate(_pos) => {
                write!(f, "cannot calculate")
            },
            Self::NotIntInShift(typ, _pos) => {
                write!(f, "type {typ} cannot shift")
            },
            Self::NotIntBitAnd(typ, _pos) => {
                write!(f, "type {typ} cannot bit-and")
            },
            Self::NotIntBitOr(typ, _pos) => {
                write!(f, "type {typ} cannot bit-or")
            },
            Self::NotIntBitXor(typ, _pos) => {
                write!(f, "type {typ} cannot bit-xor")
            },
            Self::AssignIllegalValue(expr_ast, _pos) => {
                write!(f, "assign illegal value {:?}", expr_ast)
            },
            Self::NoSuchAMember(opt_type_name, member_name, _pos) => {
                if let Some(id) = opt_type_name {
                    write!(f, "no such a member '{member_name}' in {}", id)
                }else{
                    write!(f, "no such a member '{member_name}'")
                }
            },
            Self::CannotAccessStructMember(id, _pos) => {
                write!(f, "{id} cannot access struct member")
            },
            Self::NotUnion(id, _pos) => {
                write!(f, "{id} is not union")
            },
            Self::NoIndexValueWhileAccessArray(_pos) => {
                write!(f, "no index value while access array")
            },
            Self::CannotCallNotFunction(_pos) => {
                write!(f, "cannot call non-function")
            },
            Self::NoReturnForType(typ, _pos) => {
                write!(f, "no return for type {typ}")
            },
            Self::MismatchTypeInIf {
                then_type,
                else_type,
                pos: _,
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
            Self::NotPointer(typ, _pos) => {
                write!(f, "{typ} is not pointer")
            },
            Self::TypeHasNotMember(id, _pos) => {
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
            Self::HasNotMember(type_name, member_name, _pos) => {
                write!(f, "'{type_name}' has not member '{member_name}'")
            },
            Self::NotArray(expr, _pos) => {
                write!(f, "{:?} is not array", expr)
            },
            Self::ArrayIndexIsTooLong(_pos) => {
                write!(f, "array index is too long")
            },
            Self::AlreadyClassFunctionDefined(class_name, func_name, _pos) => {
                write!(f, "already static function '{func_name}' is defined in struct '{class_name}'")
            },
            Self::AlreadyMemberFunctionDefined(class_name, func_name, _pos) => {
                write!(f, "already member function '{func_name}' is defined in struct '{class_name}'")
            },
            Self::AlreadyClassVarDefined(class_name, var_name, _pos) => {
                write!(f, "already static var '{var_name}' is defined in struct '{class_name}'")
            },
            Self::NoSuchAClassVar(class_name, var_name, _pos) => {
                write!(f, "no such a var '{var_name}' in struct '{class_name}'")
            },
            Self::NoCurrentClass(_pos) => {
                write!(f, "no Self")
            },
            Self::CannotAssignConstant(_pos) => {
                write!(f, "cannot assign constant")
            },
            Self::NoSuchAEnumMember(enum_name, member_name, _pos) => {
                write!(f, "no such a member '{member_name}' in enum '{enum_name}'")
            },
        }
        
    }
}

impl Error for CodeGenError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

use tokenizer::Token;
use crate::{ConstExpr, ExprAST, SpecifierQualifier, TypeOrVariadic, NumberType};
use crate::Type;
use crate::Position;
use super::TokenizerError;
use std::fmt;
use std::error::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum ParserError {
    TokenizerError(TokenizerError),
    NotNumber(ExprAST, Position),
    NotPointer(Type, Position),
    NotArray(Type, Position),
    NotSymbol(Position),
    NotExpr(Position),
    NotFunction(Type, Position),
    NotNumberType(Type, Position),
    NotNumberTypeToBeUnsigned(Type, Position),
    CannotToBeUnsigned(NumberType, Position),
    SyntaxError(Position),
    IllegalEndOfInput(Position),
    WithoutExpectedToken{expected_token: Token, real_token: Token, pos: Position},
    NoSuchAOperator{token_type: Token, pos: Position},
    NeedExpr(Position),
    NoTypeDefined(Option<String>, Position),
    NoExprWhileAccessArray(Position),
    NoIdAfterDot(Position),
    NoIdAfterArrow(Position),
    NeedBraceRightOrCommaWhenParsingInitializerList(Position),
    CannotConvertToUsize(ConstExpr, Position),
    CannotApplyOperatorToFloat(String, Position),
    CannotNotOfFloat(Position),
    AlreadyVarDefined(String, Position),
    AlreadyTypeDefinedInEnv(String, Position),
    AlreadyTypeDefined {
        typ: Type,
        pos: Position,
        pre_type: Type,
        pre_pos: Position,
    },
    AccessSelfTypeWithoutImpl(Position),
    NoSuchAStruct(String, Position),
    NoSuchAField(String, String, Position),
    NoSuchAConstant(String, Position),
    IsNotConstant(ExprAST, Position),
    CannotGetBlock(Position),
    NotDefvarWhenGet(Position),
    CannotCombineWithPreviousSignedDeclarationSpecifier(Position, Position),
    CannotCombineWithPreviousUnsignedDeclarationSpecifier(Position, Position),
    NotNumberSigned(Type, Position),
    NotNumberUnsigned(Type, Position),
    SyntaxErrorWhileParsingStruct(SpecifierQualifier, TypeOrVariadic, Position),
    NotSelfAfterRef(Position),
    NoConstantExprParsingStructAfterColon(Position),
    NotSymbolParsingEnum(Position),
    EnumShouldBeInt(Position),
    ShouldBe(Vec<Token>, Token, Position),
    NoTypeForStructField(Position),
    NoConstantExprAfterCase(Position),
    LabeledStatementWithoutFunction(Position),
    NoIdForGotoStatement(Position),
    NotFunctionOrVarDefineInImpl(Position),
    NotBraceLeftOrForWhileParsingImpl(Token, Position),
    NoIdAfterForWhileParsingImpl(Token, Position),
    NotSymbolWhileParsingImpl(Position),
    NoSuchAType{name: String, pos: Position},
    UndefindSymbol(String, Position),
    ArrayNeedExplicitSizeOrInitializer(Position),
    ArrayNeedArrayInitializer(Position),
    NotLBraceParsingArrayInitializer(Token, Position),
    ArrayLengthMismatch(usize, usize, Position),
    NotStructTypeWhenParsingStructInitializer(Position),
    NoTypeWhileParsingStructInitializer(Position),
    DuplicateFieldInStructInitializer(String, Position),
    NumberOfElementsDoesNotMatch(Position),
    NotGenericType(Token, Position),
    AlreadyGenericsTypeDefined(String, Position),
    TaggedEnumCannotHaveValue(Position),
    StandardEnumCannotBeTagged(Position),
    NotTaggedEnum(Position),
    NotStructTypeEnum(String, String, Position),
    NotPattern(Position),
}

impl ParserError {
    pub fn not_number(exp: &ExprAST, pos: Position) -> ParserError {
        ParserError::NotNumber(exp.clone(), pos)
    }
    pub fn not_pointer(typ: &Type, pos: Position) -> ParserError {
        ParserError::NotPointer(typ.clone(), pos)
    }

    pub fn not_array(typ: &Type, pos: Position) -> ParserError {
        ParserError::NotArray(typ.clone(), pos)
    }

    pub fn not_symbol(pos: Position) -> Self {
        ParserError::NotSymbol(pos)
    }

    pub fn not_expr(pos: Position) -> Self {
        ParserError::NotExpr(pos)
    }

    pub fn not_function(typ: &Type, pos: Position) -> ParserError {
        ParserError::NotFunction(typ.clone(), pos)
    }

    pub fn not_number_type(typ: &Type, pos: Position) -> ParserError {
        ParserError::NotNumberType(typ.clone(), pos)
    }

    pub fn not_number_type_to_be_unsigned(typ: &Type, pos: Position) -> ParserError {
        ParserError::NotNumberTypeToBeUnsigned(typ.clone(), pos)
    }

    pub fn syntax_error(pos: Position) -> ParserError {
        ParserError::SyntaxError(pos)
    }

    pub fn illegal_end_of_input(pos: Position) -> ParserError {
        ParserError::IllegalEndOfInput(pos)
    }

    pub fn without_expected_token(expected: Token, real: Token, pos: Position) -> ParserError {
        ParserError::WithoutExpectedToken{expected_token: expected, real_token: real, pos}
    }

    pub fn no_such_a_operator(token_type: Token, pos: Position) -> ParserError {
        ParserError::NoSuchAOperator { token_type, pos }
    }

    pub fn need_expr(pos: Position) -> ParserError {
        ParserError::NeedExpr(pos)
    }

    pub fn no_type_defined(opt_name: Option<String>, pos: Position) -> ParserError {
        ParserError::NoTypeDefined(opt_name, pos)
    }

    pub fn no_expr_while_access_array(pos: Position) -> ParserError {
        ParserError::NoExprWhileAccessArray(pos)
    }

    pub fn no_id_after_dot(pos: Position) -> ParserError {
        ParserError::NoIdAfterDot(pos)
    }

    pub fn no_id_after_arrow(pos: Position) -> ParserError {
        ParserError::NoIdAfterArrow(pos)
    }

    pub fn need_brace_right_or_comma_when_parsing_initializer_list(pos: Position) -> ParserError {
        ParserError::NeedBraceRightOrCommaWhenParsingInitializerList(pos)
    }

    pub fn cannot_convert_to_usize(const_expr: &ConstExpr, pos: Position) -> ParserError {
        ParserError::CannotConvertToUsize(const_expr.clone(), pos)
    }

    pub fn cannot_apply_operator_to_float(msg: &str, pos: Position) -> ParserError {
        ParserError::CannotApplyOperatorToFloat(msg.to_string(), pos)
    }

    pub fn cannot_not_of_float(pos: Position) -> ParserError {
        ParserError::CannotNotOfFloat(pos)
    }

    pub fn already_var_defined(name: &str, pos: Position) -> ParserError {
        ParserError::AlreadyVarDefined(name.to_string(), pos)
    }

    pub fn already_type_defined_in_env(name: &str, pos: Position) -> ParserError {
        ParserError::AlreadyTypeDefinedInEnv(name.to_string(), pos)
    }

    pub fn access_self_type_without_impl(pos: Position) -> ParserError {
        ParserError::AccessSelfTypeWithoutImpl(pos)
    }

    pub fn no_such_a_struct(name: &str, pos: Position) -> ParserError {
        ParserError::NoSuchAStruct(name.to_string(), pos)
    }

    pub fn no_such_a_field(class_name: String, field_name: String, pos: Position) -> Self {
        ParserError::NoSuchAField(class_name, field_name, pos)
    }

    pub fn no_such_a_constant(name: &str, pos: Position) -> ParserError {
        ParserError::NoSuchAConstant(name.to_string(), pos)
    }

    pub fn is_not_constant(expr: &ExprAST, pos: Position) -> ParserError {
        ParserError::IsNotConstant(expr.clone(), pos)
    }

    pub fn cannot_get_block(pos: Position) -> ParserError {
        ParserError::CannotGetBlock(pos)
    }

    pub fn not_defvar_when_get(pos: Position) -> ParserError {
        ParserError::NotDefvarWhenGet(pos)
    }

    pub fn cannot_combine_with_previous_signed_declaration_specifier(pos: Position, pre_pos: Position) -> ParserError {
        ParserError::CannotCombineWithPreviousUnsignedDeclarationSpecifier(pos, pre_pos)
    }

    pub fn cannot_combine_with_previous_unsigned_declaration_specifier(pos: Position, pre_pos: Position) -> ParserError {
        ParserError::CannotCombineWithPreviousUnsignedDeclarationSpecifier(pos, pre_pos)
    }

    pub fn not_number_signed(typ: &Type, pos: Position) -> ParserError {
        ParserError::NotNumberSigned(typ.clone(), pos)
    }

    pub fn not_number_unsigned(typ: &Type, pos: Position) -> ParserError {
        ParserError::NotNumberUnsigned(typ.clone(), pos)
    }

    pub fn already_type_defined(typ: &Type, pos: Position, pre_type: &Type, pre_pos: &Position) -> ParserError {
        ParserError::AlreadyTypeDefined { typ: typ.clone(), pos, pre_type: pre_type.clone(), pre_pos: pre_pos.clone() }
    }

    pub fn syntax_error_while_parsing_struct(sp: &SpecifierQualifier, type_or_variadic: &TypeOrVariadic, pos: Position) -> ParserError {
        ParserError::SyntaxErrorWhileParsingStruct(sp.clone(), type_or_variadic.clone(), pos)
    }

    pub fn not_self_after_ref(pos: Position) -> ParserError {
        ParserError::NotSelfAfterRef(pos)
    }

    pub fn no_constant_expr_parsing_struct_after_colon(pos: Position) -> ParserError {
        ParserError::NoConstantExprParsingStructAfterColon(pos)
    }

    pub fn not_symbol_parsing_enum(pos: Position) -> ParserError {
        ParserError::NotSymbolParsingEnum(pos)
    }

    pub fn enum_should_be_int(pos: Position) -> ParserError {
        ParserError::EnumShouldBeInt(pos)
    }

    pub fn should_be(token_type_vec: Vec<Token>, typ: &Token, pos: Position) -> ParserError {
        ParserError::ShouldBe(token_type_vec, typ.clone(), pos)
    }

    pub fn cannot_to_be_unsigned(num_type: &NumberType, pos: Position) -> ParserError {
        ParserError::CannotToBeUnsigned(num_type.clone(), pos)
    }

    pub fn no_type_for_struct_field(pos: Position) -> ParserError {
        ParserError::NoTypeForStructField(pos)
    }

    pub fn no_constant_expr_after_case(pos: Position) -> ParserError {
        ParserError::NoConstantExprAfterCase(pos)
    }

    pub fn labeled_statement_without_function(pos: Position) -> ParserError {
        ParserError::LabeledStatementWithoutFunction(pos)
    }

    pub fn no_id_for_goto_statement(pos: Position) -> ParserError {
        ParserError::NoIdForGotoStatement(pos)
    }

    pub fn not_function_or_var_define_in_impl(pos: Position) -> ParserError {
        ParserError::NotFunctionOrVarDefineInImpl(pos)
    }

    pub fn not_brace_left_or_for_while_parsing_impl(tok: &Token, pos: Position) -> ParserError {
        ParserError::NotBraceLeftOrForWhileParsingImpl(tok.clone(), pos)
    }

    pub fn no_id_after_for_while_parsing_impl(tok: &Token, pos: Position) -> ParserError {
        ParserError::NoIdAfterForWhileParsingImpl(tok.clone(), pos)
    }

    pub fn not_symbol_while_parsing_impl(pos: Position) -> ParserError {
        ParserError::NotSymbolWhileParsingImpl(pos)
    }

    pub fn no_such_a_type(name: &str, pos: Position) -> ParserError {
        ParserError::NoSuchAType { name: name.to_string(), pos }
    }

    pub fn undefined_symbol(name: &str, pos: Position) -> ParserError {
        Self::UndefindSymbol(name.to_string(), pos)
    }

    pub fn array_need_explicit_size_or_initializer(pos: Position) -> ParserError {
        Self::ArrayNeedExplicitSizeOrInitializer(pos)
    }

    pub fn array_need_array_initializer(pos: Position) -> ParserError {
        Self::ArrayNeedArrayInitializer(pos)
    }

    pub fn not_l_brace_parsing_array_initializer(tok: Token, pos: Position) -> ParserError {
        ParserError::NotLBraceParsingArrayInitializer(tok, pos)
    }

    pub fn array_length_mismatch(required_len: usize, real_len: usize, pos: Position) -> ParserError {
        ParserError::ArrayLengthMismatch(required_len, real_len, pos)
    }

    pub fn not_struct_type_when_parsing_struct_initializer(pos: Position) -> ParserError {
        ParserError::NotStructTypeWhenParsingStructInitializer(pos)
    }

    pub fn no_type_while_parsing_struct_initializer(pos: Position) -> ParserError {
        ParserError::NoTypeWhileParsingStructInitializer(pos)
    }

    pub fn duplicate_field_in_struct_initializer(field_name: String, pos: Position) -> Self {
        ParserError::DuplicateFieldInStructInitializer(field_name, pos)
    }

    pub fn number_of_elements_does_not_match(pos: Position) -> Self {
        ParserError::NumberOfElementsDoesNotMatch(pos)
    }

    pub fn not_generic_type(tok: Token, pos: Position) -> Self {
        ParserError::NotGenericType(tok, pos)
    }

    pub fn already_generics_type_defined(name: String, pos: Position) -> Self {
        ParserError::AlreadyGenericsTypeDefined(name, pos)
    }

    pub fn tagged_enum_cannot_have_value(pos: Position) -> Self {
        ParserError::TaggedEnumCannotHaveValue(pos)
    }

    pub fn standard_enum_cannot_be_tagged(pos: Position) -> Self {
        ParserError::StandardEnumCannotBeTagged(pos)
    }

    pub fn not_tagged_enum(pos: Position) -> Self {
        ParserError::NotTaggedEnum(pos)
    }

    pub fn not_struct_type_enum(enum_name: String, elem_name: String, pos: Position) -> Self {
        ParserError::NotStructTypeEnum(enum_name, elem_name, pos)
    }

    pub fn not_pattern(pos: Position) -> Self {
        ParserError::NotPattern(pos)
    }
}

impl From<TokenizerError> for ParserError {
    fn from(item: TokenizerError) -> Self {
        ParserError::TokenizerError(item)
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::TokenizerError(tok_err) => write!(f, "{}", tok_err.to_string()),
            Self::NotNumber(_expr, _pos) => write!(f, "not a number"),
            Self::NotPointer(_typ, _pos) => write!(f, "not pointer"),
            Self::NotArray(_typ, _pos) => write!(f, "not array"),
            Self::NotSymbol(_pos) => write!(f, "not symbol"),
            Self::NotExpr(_pos) => write!(f, "not expr"),
            Self::NotFunction(_typ, _pos) => write!(f, "not function"),
            Self::NotNumberType(_typ, _pos) => write!(f, "not number type"),
            Self::NotNumberTypeToBeUnsigned(_typ, _pos) => write!(f, "not number type to be unsigned"),
            Self::CannotToBeUnsigned(_number_type, _pos) => write!(f, "cannot to be unsigned"),
            Self::SyntaxError(_pos) => write!(f, "syntax error"),
            Self::IllegalEndOfInput(_pos) => write!(f, "illegal end of input"),
            Self::WithoutExpectedToken{expected_token: _, real_token: _, pos: _} => write!(f, "without expected token"),
            Self::NoSuchAOperator{token_type: _, pos: _} => write!(f, "no such a operator"),
            Self::NeedExpr(_pos) => write!(f, "need expr"),
            Self::NoTypeDefined(opt_name, _pos) => {
                if let Some(name) = opt_name {
                    write!(f, "no type defined. '{}'", name)
                }else{
                    write!(f, "no type defined")
                }
            },
            Self::NoExprWhileAccessArray(_pos) => write!(f, "no expr while access array"),
            Self::NoIdAfterDot(_pos) => write!(f, "no id after dot"),
            Self::NoIdAfterArrow(_pos) => write!(f, "no id after arrow"),
            Self::NeedBraceRightOrCommaWhenParsingInitializerList(_pos) => write!(f, "need brace right or comma when parsing initializer list"),
            Self::CannotConvertToUsize(_const_expr, _pos) => write!(f, "cannot convert to usize"),
            Self::CannotApplyOperatorToFloat(_name, _pos) => write!(f, "cannot apply operator to float"),
            Self::CannotNotOfFloat(_pos) => write!(f, "cannot not of float"),
            Self::AlreadyVarDefined(name, _pos) => write!(f, "already var '{}' defined", name),
            Self::AlreadyTypeDefinedInEnv(name, _pos) => write!(f, "already type '{}' defined", name),
            Self::AlreadyTypeDefined { typ: _, pos: _, pre_type: _, pre_pos: _ } => write!(f, "already type defined"),
            Self::AccessSelfTypeWithoutImpl(_pos) => write!(f, "access self type without impl"),
            Self::NoSuchAStruct(name, _pos) => write!(f, "no such a struct '{}'", name),
            Self::NoSuchAField(class_name, field_name, _pos) => write!(f, "no such a field 'field_name' in struct '{class_name}'"),
            Self::NoSuchAConstant(name, _pos) => write!(f, "no such a constant '{}'", name),
            Self::IsNotConstant(_expr, _pos) => write!(f, "is not constant"),
            Self::CannotGetBlock(_pos) => write!(f, "cannot get block"),
            Self::NotDefvarWhenGet(_pos) => write!(f, "not defvar when get"),
            Self::CannotCombineWithPreviousSignedDeclarationSpecifier(_pos, _pos2) => write!(f, "cannot combine with previous signed declaration specifier"),
            Self::CannotCombineWithPreviousUnsignedDeclarationSpecifier(_pos, _pos2) => write!(f, "cannot combine with previous unsigned declaration specifier"),
            Self::NotNumberSigned(_typ, _pos) => write!(f, "not number signed"),
            Self::NotNumberUnsigned(_typ, _pos) => write!(f, "not number unsigned"),
            Self::SyntaxErrorWhileParsingStruct(_specifier_qualifier, _type_or_variadic, _pos) => write!(f, "syntax error while parsing struct"),
            Self::NotSelfAfterRef(_pos) => write!(f, "not self after ref"),
            Self::NoConstantExprParsingStructAfterColon(_pos) => write!(f, "no constant expr parsing struct after colon"),
            Self::NotSymbolParsingEnum(_pos) => write!(f, "not symbol parsing enum"),
            Self::EnumShouldBeInt(_pos) => write!(f, "enum should be int"),
            Self::ShouldBe(_token_list, _tok, _pos) => write!(f, "should be"),
            Self::NoTypeForStructField(_pos) => write!(f, "no type for struct field"),
            Self::NoConstantExprAfterCase(_pos) => write!(f, "no constant expr after case"),
            Self::LabeledStatementWithoutFunction(_pos) => write!(f, "labeled statement without function"),
            Self::NoIdForGotoStatement(_pos) => write!(f, "no id for goto statement"),
            Self::NotFunctionOrVarDefineInImpl(_pos) => write!(f, "not function or variable define in impl"),
            Self::NotBraceLeftOrForWhileParsingImpl(_tok, _pos) => write!(f, "not brace left or for while parsing impl"),
            Self::NoIdAfterForWhileParsingImpl(_tok, _pos) => write!(f, "no id after for while parsing impl"),
            Self::NotSymbolWhileParsingImpl(_pos) => write!(f, "not symbol while parsing impl"),
            Self::NoSuchAType{name, pos: _} => write!(f, "no such a type '{}'", name),
            Self::UndefindSymbol(name, _pos) => write!(f, "undefined symbol '{}'", name),
            Self::ArrayNeedExplicitSizeOrInitializer(_pos) => write!(f, "array need explicit size or initializer"),
            Self::ArrayNeedArrayInitializer(_pos) => write!(f, "array need array initializer"),
            Self::NotLBraceParsingArrayInitializer(tok, _pos) => write!(f, "need '{{', but '{}'", tok),
            Self::ArrayLengthMismatch(required_len, real_len, _pos) => write!(f, "mismatch array length. required {required_len}, but {real_len}"),
            Self::NotStructTypeWhenParsingStructInitializer(_pos) => write!(f, "not struct type when parsing struct initializer"),
            Self::NoTypeWhileParsingStructInitializer(_pos) => write!(f, "no type while parsing struct initializer"),
            Self::DuplicateFieldInStructInitializer(field_name, _pos) => write!(f, "duplicate field '{field_name}' in struct initializer"),
            Self::NumberOfElementsDoesNotMatch(_pos) => write!(f, "number of elements does not match"),
            Self::NotGenericType(tok, _pos) => write!(f, "{tok:?} cannot not be generic type"),
            Self::AlreadyGenericsTypeDefined(name, _pos) => write!(f, "already generics type '{name}' is defined"),
            Self::TaggedEnumCannotHaveValue(_pos) => write!(f, "tagged enum cannot have value"),
            Self::StandardEnumCannotBeTagged(_pos) => write!(f, "standard enum cannot be tagged"),
            Self::NotTaggedEnum(_pos) => write!(f, "not tagged enum"),
            Self::NotStructTypeEnum(enum_name, elem_name, _pos) => write!(f, "{enum_name}::{elem_name} is not struct type"),
            Self::NotPattern(_pos) => write!(f, "not pattern"),
        }
    }
}

impl Error for ParserError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}
use tokenizer::TokenType;
use crate::{ConstExpr, ExprAST, SpecifierQualifier, TypeOrVariadic, NumberType};
use crate::Type;
use crate::Location;

#[derive(Debug, Clone, PartialEq)]
pub enum ParserError {
    NotNumber(Option<Location>, ExprAST),
    NotPointer(Option<Location>, Type),
    NotArray(Option<Location>, Type),
    NotFunction(Option<Location>, Type),
    NotNumberType(Option<Location>, Type),
    NotNumberTypeToBeUnsigned(Option<Location>, Type),
    CannotToBeUnsigned(Option<Location>, NumberType),
    SyntaxError{opt_loc: Option<Location>, filename: &'static str, line: u32, column: u32},
    IllegalEndOfInput(Option<Location>),
    WithoutExpectedToken{opt_loc: Option<Location>, expected: TokenType, real: TokenType},
    NoSuchAOperator{opt_loc: Option<Location>, token_type: TokenType},
    NeedExpr(Option<Location>),
    NoTypeDefined(Option<Location>),
    NoExprWhileAccessArray(Option<Location>),
    NoIdAfterDot(Option<Location>),
    NoIdAfterArrow(Option<Location>),
    NeedBraceRightOrCommaWhenParsingInitializerList(Option<Location>),
    CannotConvertToUsize(Option<Location>, ConstExpr),
    CannotApplyOperatorToFloat(Option<Location>, String),
    CannotNotOfFloat(Option<Location>),
    AlreadyVarDefined(Option<Location>, String),
    AlreadyTypeDefinedInEnv(Option<Location>, String),
    AlreadyTypeDefined {
        opt_loc: Option<Location>,
        typ: Type,
        pre_type: Type,
        pre_loc: Location,
    },
    AccessSelfTypeWithoutImpl(Option<Location>),
    NoSuchAStruct(Option<Location>, String),
    NoSuchAConstant(Option<Location>, String),
    IsNotConstant(Option<Location>, ExprAST),
    CannotGetBlock(Option<Location>),
    NotDefvarWhenGet(Option<Location>),
    CannotCombineWithPreviousSignedDeclarationSpecifier(Option<Location>, Location),
    CannotCombineWithPreviousUnsignedDeclarationSpecifier(Option<Location>, Location),
    NotNumberSigned(Option<Location>, Type),
    NotNumberUnsigned(Option<Location>, Type),
    SyntaxErrorWhileParsingStruct(Option<Location>, SpecifierQualifier, TypeOrVariadic),
    NotSelfAfterRef(Option<Location>),
    NoConstantExprParsingStructAfterColon(Option<Location>),
    NotSymbolParsingEnum(Option<Location>),
    EnumShouldBeInt(Option<Location>),
    ShouldBe(Option<Location>, Vec<TokenType>, TokenType),
    NoTypeForStructField(Option<Location>),
}

impl ParserError {
    pub fn not_number(opt_loc: Option<Location>, exp: &ExprAST) -> ParserError {
        ParserError::NotNumber(opt_loc, exp.clone())
    }
    pub fn not_pointer(opt_loc: Option<Location>, typ: &Type) -> ParserError {
        ParserError::NotPointer(opt_loc, typ.clone())
    }

    pub fn not_array(opt_loc: Option<Location>, typ: &Type) -> ParserError {
        ParserError::NotArray(opt_loc, typ.clone())
    }

    pub fn not_function(opt_loc: Option<Location>, typ: &Type) -> ParserError {
        ParserError::NotFunction(opt_loc, typ.clone())
    }

    pub fn not_number_type(opt_loc: Option<Location>, typ: &Type) -> ParserError {
        ParserError::NotNumberType(opt_loc, typ.clone())
    }

    pub fn not_number_type_to_be_unsigned(opt_loc: Option<Location>, typ: &Type) -> ParserError {
        ParserError::NotNumberTypeToBeUnsigned(opt_loc, typ.clone())
    }

    pub fn syntax_error(opt_loc: Option<Location>, filename: &'static str, line: u32, column: u32) -> ParserError {
        ParserError::SyntaxError { opt_loc, filename, line, column }
    }

    pub fn illegal_end_of_input(opt_loc: Option<Location>) -> ParserError {
        ParserError::IllegalEndOfInput(opt_loc)
    }

    pub fn without_expected_token(opt_loc: Option<Location>, expected: TokenType, real: TokenType) -> ParserError {
        ParserError::WithoutExpectedToken{opt_loc, expected, real}
    }

    pub fn no_such_a_operator(opt_loc: Option<Location>, token_type: TokenType) -> ParserError {
        ParserError::NoSuchAOperator { opt_loc, token_type }
    }

    pub fn need_expr(opt_loc: Option<Location>) -> ParserError {
        ParserError::NeedExpr(opt_loc)
    }

    pub fn no_type_defined(opt_loc: Option<Location>) -> ParserError {
        ParserError::NoTypeDefined(opt_loc)
    }

    pub fn no_expr_while_access_array(opt_loc: Option<Location>) -> ParserError {
        ParserError::NoExprWhileAccessArray(opt_loc)
    }

    pub fn no_id_after_dot(opt_loc: Option<Location>) -> ParserError {
        ParserError::NoIdAfterDot(opt_loc)
    }

    pub fn no_id_after_arrow(opt_loc: Option<Location>) -> ParserError {
        ParserError::NoIdAfterArrow(opt_loc)
    }

    pub fn need_brace_right_or_comma_when_parsing_initializer_list(opt_loc: Option<Location>) -> ParserError {
        ParserError::NeedBraceRightOrCommaWhenParsingInitializerList(opt_loc)
    }

    pub fn cannot_convert_to_usize(opt_loc: Option<Location>, const_expr: &ConstExpr) -> ParserError {
        ParserError::CannotConvertToUsize(opt_loc, const_expr.clone())
    }

    pub fn cannot_apply_operator_to_float(opt_loc: Option<Location>, msg: &str) -> ParserError {
        ParserError::CannotApplyOperatorToFloat(opt_loc, msg.to_string())
    }

    pub fn cannot_not_of_float(opt_loc: Option<Location>) -> ParserError {
        ParserError::CannotNotOfFloat(opt_loc)
    }

    pub fn already_var_defined(opt_loc: Option<Location>, name: &str) -> ParserError {
        ParserError::AlreadyVarDefined(opt_loc, name.to_string())
    }

    pub fn already_type_defined_in_env(opt_loc: Option<Location>, name: &str) -> ParserError {
        ParserError::AlreadyTypeDefinedInEnv(opt_loc, name.to_string())
    }

    pub fn access_self_type_without_impl(opt_loc: Option<Location>) -> ParserError {
        ParserError::AccessSelfTypeWithoutImpl(opt_loc)
    }

    pub fn no_such_a_struct(opt_loc: Option<Location>, name: &str) -> ParserError {
        ParserError::NoSuchAStruct(opt_loc, name.to_string())
    }

    pub fn no_such_a_constant(opt_loc: Option<Location>, name: &str) -> ParserError {
        ParserError::NoSuchAConstant(opt_loc, name.to_string())
    }

    pub fn is_not_constant(opt_loc: Option<Location>, expr: &ExprAST) -> ParserError {
        ParserError::IsNotConstant(opt_loc, expr.clone())
    }

    pub fn cannot_get_block(opt_loc: Option<Location>) -> ParserError {
        ParserError::CannotGetBlock(opt_loc)
    }

    pub fn not_defvar_when_get(opt_loc: Option<Location>) -> ParserError {
        ParserError::NotDefvarWhenGet(opt_loc)
    }

    pub fn cannot_combine_with_previous_signed_declaration_specifier(opt_loc: Option<Location>, pre_loc: Location) -> ParserError {
        ParserError::CannotCombineWithPreviousUnsignedDeclarationSpecifier(opt_loc, pre_loc)
    }

    pub fn cannot_combine_with_previous_unsigned_declaration_specifier(opt_loc: Option<Location>, pre_loc: Location) -> ParserError {
        ParserError::CannotCombineWithPreviousUnsignedDeclarationSpecifier(opt_loc, pre_loc)
    }

    pub fn not_number_signed(opt_loc: Option<Location>, typ: &Type) -> ParserError {
        ParserError::NotNumberSigned(opt_loc, typ.clone())
    }

    pub fn not_number_unsigned(opt_loc: Option<Location>, typ: &Type) -> ParserError {
        ParserError::NotNumberUnsigned(opt_loc, typ.clone())
    }

    pub fn already_type_defined(opt_loc: Option<Location>, typ: &Type, pre_type: &Type, pre_loc: Location) -> ParserError {
        ParserError::AlreadyTypeDefined { opt_loc, typ: typ.clone(), pre_type: pre_type.clone(), pre_loc }
    }

    pub fn syntax_error_while_parsing_struct(opt_loc: Option<Location>, sp: &SpecifierQualifier, type_or_variadic: &TypeOrVariadic) -> ParserError {
        ParserError::SyntaxErrorWhileParsingStruct(opt_loc, sp.clone(), type_or_variadic.clone())
    }

    pub fn not_self_after_ref(opt_loc: Option<Location>) -> ParserError {
        ParserError::NotSelfAfterRef(opt_loc)
    }

    pub fn no_constant_expr_parsing_struct_after_colon(opt_loc: Option<Location>) -> ParserError {
        ParserError::NoConstantExprParsingStructAfterColon(opt_loc)
    }

    pub fn not_symbol_parsing_enum(opt_loc: Option<Location>) -> ParserError {
        ParserError::NotSymbolParsingEnum(opt_loc)
    }

    pub fn enum_should_be_int(opt_loc: Option<Location>) -> ParserError {
        ParserError::EnumShouldBeInt(opt_loc)
    }

    pub fn should_be(opt_loc: Option<Location>, token_type_vec: Vec<TokenType>, typ: &TokenType) -> ParserError {
        ParserError::ShouldBe(opt_loc, token_type_vec, typ.clone())
    }

    pub fn cannot_to_be_unsigned(opt_loc: Option<Location>, num_type: &NumberType) -> ParserError {
        ParserError::CannotToBeUnsigned(opt_loc, num_type.clone())
    }

    pub fn no_type_for_struct_field(opt_loc: Option<Location>) -> ParserError {
        ParserError::NoTypeForStructField(opt_loc)
    }
}
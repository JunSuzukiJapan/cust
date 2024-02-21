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
    NotNumber(Position, ExprAST),
    NotPointer(Position, Type),
    NotArray(Position, Type),
    NotSymbol(Position),
    NotFunction(Position, Type),
    NotNumberType(Position, Type),
    NotNumberTypeToBeUnsigned(Position, Type),
    CannotToBeUnsigned(Position, NumberType),
    SyntaxError(Position),
    IllegalEndOfInput(Position),
    WithoutExpectedToken{pos: Position, expected_token: Token, real_token: Token},
    NoSuchAOperator{pos: Position, token_type: Token},
    NeedExpr(Position),
    NoTypeDefined(Position),
    NoExprWhileAccessArray(Position),
    NoIdAfterDot(Position),
    NoIdAfterArrow(Position),
    NeedBraceRightOrCommaWhenParsingInitializerList(Position),
    CannotConvertToUsize(Position, ConstExpr),
    CannotApplyOperatorToFloat(Position, String),
    CannotNotOfFloat(Position),
    AlreadyVarDefined(Position, String),
    AlreadyTypeDefinedInEnv(Position, String),
    AlreadyTypeDefined {
        pos: Position,
        typ: Type,
        pre_type: Type,
        pre_pos: Position,
    },
    AccessSelfTypeWithoutImpl(Position),
    NoSuchAStruct(Position, String),
    NoSuchAConstant(Position, String),
    IsNotConstant(Position, ExprAST),
    CannotGetBlock(Position),
    NotDefvarWhenGet(Position),
    CannotCombineWithPreviousSignedDeclarationSpecifier(Position, Position),
    CannotCombineWithPreviousUnsignedDeclarationSpecifier(Position, Position),
    NotNumberSigned(Position, Type),
    NotNumberUnsigned(Position, Type),
    SyntaxErrorWhileParsingStruct(Position, SpecifierQualifier, TypeOrVariadic),
    NotSelfAfterRef(Position),
    NoConstantExprParsingStructAfterColon(Position),
    NotSymbolParsingEnum(Position),
    EnumShouldBeInt(Position),
    ShouldBe(Position, Vec<Token>, Token),
    NoTypeForStructField(Position),
    NoConstantExprAfterCase(Position),
    LabeledStatementWithoutFunction(Position),
    NoIdForGotoStatement(Position),
    NotFunctionDefineInImpl(Position),
    NotBraceLeftOrForWhileParsingImpl(Position, Token),
    NoIdAfterForWhileParsingImpl(Position, Token),
    NotSymbolWhileParsingImpl(Position),
    NoSuchAType{pos: Position, name: String},
    UndefindSymbol(Position, String),
    ArrayNeedExplicitSizeOrInitializer(Position),
    ArrayNeedArrayInitializer(Position),
}

impl ParserError {
    pub fn not_number(pos: Position, exp: &ExprAST) -> ParserError {
        ParserError::NotNumber(pos, exp.clone())
    }
    pub fn not_pointer(pos: Position, typ: &Type) -> ParserError {
        ParserError::NotPointer(pos, typ.clone())
    }

    pub fn not_array(pos: Position, typ: &Type) -> ParserError {
        ParserError::NotArray(pos, typ.clone())
    }

    pub fn not_symbol(pos: Position) -> Self {
        ParserError::NotSymbol(pos)
    }

    pub fn not_function(pos: Position, typ: &Type) -> ParserError {
        ParserError::NotFunction(pos, typ.clone())
    }

    pub fn not_number_type(pos: Position, typ: &Type) -> ParserError {
        ParserError::NotNumberType(pos, typ.clone())
    }

    pub fn not_number_type_to_be_unsigned(pos: Position, typ: &Type) -> ParserError {
        ParserError::NotNumberTypeToBeUnsigned(pos, typ.clone())
    }

    pub fn syntax_error(pos: Position) -> ParserError {
        ParserError::SyntaxError(pos)
    }

    pub fn illegal_end_of_input(pos: Position) -> ParserError {
        ParserError::IllegalEndOfInput(pos)
    }

    pub fn without_expected_token(pos: Position, expected: Token, real: Token) -> ParserError {
        ParserError::WithoutExpectedToken{pos, expected_token: expected, real_token: real}
    }

    pub fn no_such_a_operator(pos: Position, token_type: Token) -> ParserError {
        ParserError::NoSuchAOperator { pos, token_type }
    }

    pub fn need_expr(pos: Position) -> ParserError {
        ParserError::NeedExpr(pos)
    }

    pub fn no_type_defined(pos: Position) -> ParserError {
        ParserError::NoTypeDefined(pos)
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

    pub fn cannot_convert_to_usize(pos: Position, const_expr: &ConstExpr) -> ParserError {
        ParserError::CannotConvertToUsize(pos, const_expr.clone())
    }

    pub fn cannot_apply_operator_to_float(pos: Position, msg: &str) -> ParserError {
        ParserError::CannotApplyOperatorToFloat(pos, msg.to_string())
    }

    pub fn cannot_not_of_float(pos: Position) -> ParserError {
        ParserError::CannotNotOfFloat(pos)
    }

    pub fn already_var_defined(pos: Position, name: &str) -> ParserError {
        ParserError::AlreadyVarDefined(pos, name.to_string())
    }

    pub fn already_type_defined_in_env(pos: Position, name: &str) -> ParserError {
        ParserError::AlreadyTypeDefinedInEnv(pos, name.to_string())
    }

    pub fn access_self_type_without_impl(pos: Position) -> ParserError {
        ParserError::AccessSelfTypeWithoutImpl(pos)
    }

    pub fn no_such_a_struct(pos: Position, name: &str) -> ParserError {
        ParserError::NoSuchAStruct(pos, name.to_string())
    }

    pub fn no_such_a_constant(pos: Position, name: &str) -> ParserError {
        ParserError::NoSuchAConstant(pos, name.to_string())
    }

    pub fn is_not_constant(pos: Position, expr: &ExprAST) -> ParserError {
        ParserError::IsNotConstant(pos, expr.clone())
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

    pub fn not_number_signed(pos: Position, typ: &Type) -> ParserError {
        ParserError::NotNumberSigned(pos, typ.clone())
    }

    pub fn not_number_unsigned(pos: Position, typ: &Type) -> ParserError {
        ParserError::NotNumberUnsigned(pos, typ.clone())
    }

    pub fn already_type_defined(pos: Position, typ: &Type, pre_type: &Type, pre_pos: &Position) -> ParserError {
        ParserError::AlreadyTypeDefined { pos, typ: typ.clone(), pre_type: pre_type.clone(), pre_pos: pre_pos.clone() }
    }

    pub fn syntax_error_while_parsing_struct(pos: Position, sp: &SpecifierQualifier, type_or_variadic: &TypeOrVariadic) -> ParserError {
        ParserError::SyntaxErrorWhileParsingStruct(pos, sp.clone(), type_or_variadic.clone())
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

    pub fn should_be(pos: Position, token_type_vec: Vec<Token>, typ: &Token) -> ParserError {
        ParserError::ShouldBe(pos, token_type_vec, typ.clone())
    }

    pub fn cannot_to_be_unsigned(pos: Position, num_type: &NumberType) -> ParserError {
        ParserError::CannotToBeUnsigned(pos, num_type.clone())
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

    pub fn not_function_define_in_impl(pos: Position) -> ParserError {
        ParserError::NotFunctionDefineInImpl(pos)
    }

    pub fn not_brace_left_or_for_while_parsing_impl(pos: Position, tok: &Token) -> ParserError {
        ParserError::NotBraceLeftOrForWhileParsingImpl(pos, tok.clone())
    }

    pub fn no_id_after_for_while_parsing_impl(pos: Position, tok: &Token) -> ParserError {
        ParserError::NoIdAfterForWhileParsingImpl(pos, tok.clone())
    }

    pub fn not_symbol_while_parsing_impl(pos: Position) -> ParserError {
        ParserError::NotSymbolWhileParsingImpl(pos)
    }

    pub fn no_such_a_type(pos: Position, name: &str) -> ParserError {
        ParserError::NoSuchAType { pos: pos, name: name.to_string() }
    }

    pub fn undefined_symbol(pos: Position, name: &str) -> ParserError {
        Self::UndefindSymbol(pos, name.to_string())
    }

    pub fn array_need_explicit_size_or_initializer(pos: Position) -> ParserError {
        Self::ArrayNeedExplicitSizeOrInitializer(pos)
    }

    pub fn array_need_array_initializer(pos: Position) -> ParserError {
        Self::ArrayNeedArrayInitializer(pos)
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
            Self::NotNumber(_pos, _expr) => write!(f, "not a number"),
            Self::NotPointer(_pos, _typ) => write!(f, "not pointer"),
            Self::NotArray(_pos, _typ) => write!(f, "not array"),
            Self::NotSymbol(_pos) => write!(f, "not symbol"),
            Self::NotFunction(_pos, _typ) => write!(f, "not function"),
            Self::NotNumberType(_pos, _typ) => write!(f, "not number type"),
            Self::NotNumberTypeToBeUnsigned(_pos, _typ) => write!(f, "not number type to be unsigned"),
            Self::CannotToBeUnsigned(_pos, _number_type) => write!(f, "cannot to be unsigned"),
            Self::SyntaxError(_pos) => write!(f, "syntax error"),
            Self::IllegalEndOfInput(_pos) => write!(f, "illegal end of input"),
            Self::WithoutExpectedToken{pos: _, expected_token: _, real_token: _} => write!(f, "without expected token"),
            Self::NoSuchAOperator{pos: _, token_type: _} => write!(f, "no such a operator"),
            Self::NeedExpr(_pos) => write!(f, "need expr"),
            Self::NoTypeDefined(_pos) => write!(f, "no type defined"),
            Self::NoExprWhileAccessArray(_pos) => write!(f, "no expr while access array"),
            Self::NoIdAfterDot(_pos) => write!(f, "no id after dot"),
            Self::NoIdAfterArrow(_pos) => write!(f, "no id after arrow"),
            Self::NeedBraceRightOrCommaWhenParsingInitializerList(_pos) => write!(f, "need brace right or comma when parsing initializer list"),
            Self::CannotConvertToUsize(_pos, _const_expr) => write!(f, "cannot convert to usize"),
            Self::CannotApplyOperatorToFloat(_pos, _name) => write!(f, "cannot apply operator to float"),
            Self::CannotNotOfFloat(_pos) => write!(f, "cannot not of float"),
            Self::AlreadyVarDefined(_pos, name) => write!(f, "already var '{}' defined", name),
            Self::AlreadyTypeDefinedInEnv(_pos, name) => write!(f, "already type '{}' defined", name),
            Self::AlreadyTypeDefined { pos: _, typ: _, pre_type: _, pre_pos: _ } => write!(f, "already type defined"),
            Self::AccessSelfTypeWithoutImpl(_pos) => write!(f, "access self type without impl"),
            Self::NoSuchAStruct(_pos, name) => write!(f, "no such a struct '{}'", name),
            Self::NoSuchAConstant(_pos, name) => write!(f, "no such a constant '{}'", name),
            Self::IsNotConstant(_pos, _expr) => write!(f, "is not constant"),
            Self::CannotGetBlock(_pos) => write!(f, "cannot get block"),
            Self::NotDefvarWhenGet(_pos) => write!(f, "not defvar when get"),
            Self::CannotCombineWithPreviousSignedDeclarationSpecifier(_pos, _pos2) => write!(f, "cannot combine with previous signed declaration specifier"),
            Self::CannotCombineWithPreviousUnsignedDeclarationSpecifier(_pos, _pos2) => write!(f, "cannot combine with previous unsigned declaration specifier"),
            Self::NotNumberSigned(_pos, _typ) => write!(f, "not number signed"),
            Self::NotNumberUnsigned(_pos, _typ) => write!(f, "not number unsigned"),
            Self::SyntaxErrorWhileParsingStruct(_pos, _specifier_qualifier, _type_or_variadic) => write!(f, "syntax error while parsing struct"),
            Self::NotSelfAfterRef(_pos) => write!(f, "not self after ref"),
            Self::NoConstantExprParsingStructAfterColon(_pos) => write!(f, "no constant expr parsing struct after colon"),
            Self::NotSymbolParsingEnum(_pos) => write!(f, "not symbol parsing enum"),
            Self::EnumShouldBeInt(_pos) => write!(f, "enum should be int"),
            Self::ShouldBe(_pos, _token_list, _tok) => write!(f, "should be"),
            Self::NoTypeForStructField(_pos) => write!(f, "no type for struct field"),
            Self::NoConstantExprAfterCase(_pos) => write!(f, "no constant expr after case"),
            Self::LabeledStatementWithoutFunction(_pos) => write!(f, "labeled statement without function"),
            Self::NoIdForGotoStatement(_pos) => write!(f, "no id for goto statement"),
            Self::NotFunctionDefineInImpl(_pos) => write!(f, "not function define in impl"),
            Self::NotBraceLeftOrForWhileParsingImpl(_pos, _tok) => write!(f, "not brace left or for while parsing impl"),
            Self::NoIdAfterForWhileParsingImpl(_pos, _tok) => write!(f, "no id after for while parsing impl"),
            Self::NotSymbolWhileParsingImpl(_pos) => write!(f, "not symbol while parsing impl"),
            Self::NoSuchAType{pos: _, name} => write!(f, "no such a type '{}'", name),
            Self::UndefindSymbol(_pos, name) => write!(f, "undefined symbol '{}'", name),
            Self::ArrayNeedExplicitSizeOrInitializer(_pos) => write!(f, "array need explicit size or initializer"),
            Self::ArrayNeedArrayInitializer(_pos) => write!(f, "array need array initializer")
        }
    }
}

impl Error for ParserError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}
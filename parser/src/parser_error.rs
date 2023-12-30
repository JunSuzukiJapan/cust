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
    NotPointer(Option<Position>, Type),
    NotArray(Option<Position>, Type),
    NotSymbol(Position),
    NotFunction(Option<Position>, Type),
    NotNumberType(Option<Position>, Type),
    NotNumberTypeToBeUnsigned(Option<Position>, Type),
    CannotToBeUnsigned(Option<Position>, NumberType),
    SyntaxError(Option<Position>),
    IllegalEndOfInput(Option<Position>),
    WithoutExpectedToken{opt_pos: Option<Position>, expected_token: Token, real_token: Token},
    NoSuchAOperator{opt_pos: Option<Position>, token_type: Token},
    NeedExpr(Option<Position>),
    NoTypeDefined(Option<Position>),
    NoExprWhileAccessArray(Option<Position>),
    NoIdAfterDot(Option<Position>),
    NoIdAfterArrow(Option<Position>),
    NeedBraceRightOrCommaWhenParsingInitializerList(Option<Position>),
    CannotConvertToUsize(Option<Position>, ConstExpr),
    CannotApplyOperatorToFloat(Option<Position>, String),
    CannotNotOfFloat(Option<Position>),
    AlreadyVarDefined(Option<Position>, String),
    AlreadyTypeDefinedInEnv(Option<Position>, String),
    AlreadyTypeDefined {
        opt_pos: Option<Position>,
        typ: Type,
        pre_type: Type,
        pre_pos: Position,
    },
    AccessSelfTypeWithoutImpl(Option<Position>),
    NoSuchAStruct(Option<Position>, String),
    NoSuchAConstant(Option<Position>, String),
    IsNotConstant(Option<Position>, ExprAST),
    CannotGetBlock(Option<Position>),
    NotDefvarWhenGet(Option<Position>),
    CannotCombineWithPreviousSignedDeclarationSpecifier(Option<Position>, Position),
    CannotCombineWithPreviousUnsignedDeclarationSpecifier(Option<Position>, Position),
    NotNumberSigned(Option<Position>, Type),
    NotNumberUnsigned(Option<Position>, Type),
    SyntaxErrorWhileParsingStruct(Option<Position>, SpecifierQualifier, TypeOrVariadic),
    NotSelfAfterRef(Option<Position>),
    NoConstantExprParsingStructAfterColon(Option<Position>),
    NotSymbolParsingEnum(Option<Position>),
    EnumShouldBeInt(Option<Position>),
    ShouldBe(Option<Position>, Vec<Token>, Token),
    NoTypeForStructField(Option<Position>),
    NoConstantExprAfterCase(Option<Position>),
    LabeledStatementWithoutFunction(Option<Position>),
    NoIdForGotoStatement(Option<Position>),
    NotFunctionDefineInImpl(Option<Position>),
    NotBraceLeftOrForWhileParsingImpl(Option<Position>, Token),
    NoIdAfterForWhileParsingImpl(Option<Position>, Token),
    NotSymbolWhileParsingImpl(Option<Position>),
    NoSuchAType{opt_pos: Option<Position>, name: String},
}

impl ParserError {
    pub fn not_number(pos: Position, exp: &ExprAST) -> ParserError {
        ParserError::NotNumber(pos, exp.clone())
    }
    pub fn not_pointer(opt_pos: Option<Position>, typ: &Type) -> ParserError {
        ParserError::NotPointer(opt_pos, typ.clone())
    }

    pub fn not_array(opt_pos: Option<Position>, typ: &Type) -> ParserError {
        ParserError::NotArray(opt_pos, typ.clone())
    }

    pub fn not_symbol(pos: Position) -> Self {
        ParserError::NotSymbol(pos)
    }

    pub fn not_function(opt_pos: Option<Position>, typ: &Type) -> ParserError {
        ParserError::NotFunction(opt_pos, typ.clone())
    }

    pub fn not_number_type(opt_pos: Option<Position>, typ: &Type) -> ParserError {
        ParserError::NotNumberType(opt_pos, typ.clone())
    }

    pub fn not_number_type_to_be_unsigned(opt_pos: Option<Position>, typ: &Type) -> ParserError {
        ParserError::NotNumberTypeToBeUnsigned(opt_pos, typ.clone())
    }

    pub fn syntax_error(opt_pos: Option<Position>) -> ParserError {
        ParserError::SyntaxError(opt_pos)
    }

    pub fn illegal_end_of_input(opt_pos: Option<Position>) -> ParserError {
        ParserError::IllegalEndOfInput(opt_pos)
    }

    pub fn without_expected_token(opt_pos: Option<Position>, expected: Token, real: Token) -> ParserError {
        ParserError::WithoutExpectedToken{opt_pos, expected_token: expected, real_token: real}
    }

    pub fn no_such_a_operator(opt_pos: Option<Position>, token_type: Token) -> ParserError {
        ParserError::NoSuchAOperator { opt_pos, token_type }
    }

    pub fn need_expr(opt_pos: Option<Position>) -> ParserError {
        ParserError::NeedExpr(opt_pos)
    }

    pub fn no_type_defined(opt_pos: Option<Position>) -> ParserError {
        ParserError::NoTypeDefined(opt_pos)
    }

    pub fn no_expr_while_access_array(opt_pos: Option<Position>) -> ParserError {
        ParserError::NoExprWhileAccessArray(opt_pos)
    }

    pub fn no_id_after_dot(opt_pos: Option<Position>) -> ParserError {
        ParserError::NoIdAfterDot(opt_pos)
    }

    pub fn no_id_after_arrow(opt_pos: Option<Position>) -> ParserError {
        ParserError::NoIdAfterArrow(opt_pos)
    }

    pub fn need_brace_right_or_comma_when_parsing_initializer_list(opt_pos: Option<Position>) -> ParserError {
        ParserError::NeedBraceRightOrCommaWhenParsingInitializerList(opt_pos)
    }

    pub fn cannot_convert_to_usize(opt_pos: Option<Position>, const_expr: &ConstExpr) -> ParserError {
        ParserError::CannotConvertToUsize(opt_pos, const_expr.clone())
    }

    pub fn cannot_apply_operator_to_float(opt_pos: Option<Position>, msg: &str) -> ParserError {
        ParserError::CannotApplyOperatorToFloat(opt_pos, msg.to_string())
    }

    pub fn cannot_not_of_float(opt_pos: Option<Position>) -> ParserError {
        ParserError::CannotNotOfFloat(opt_pos)
    }

    pub fn already_var_defined(opt_pos: Option<Position>, name: &str) -> ParserError {
        ParserError::AlreadyVarDefined(opt_pos, name.to_string())
    }

    pub fn already_type_defined_in_env(opt_pos: Option<Position>, name: &str) -> ParserError {
        ParserError::AlreadyTypeDefinedInEnv(opt_pos, name.to_string())
    }

    pub fn access_self_type_without_impl(opt_pos: Option<Position>) -> ParserError {
        ParserError::AccessSelfTypeWithoutImpl(opt_pos)
    }

    pub fn no_such_a_struct(opt_pos: Option<Position>, name: &str) -> ParserError {
        ParserError::NoSuchAStruct(opt_pos, name.to_string())
    }

    pub fn no_such_a_constant(opt_pos: Option<Position>, name: &str) -> ParserError {
        ParserError::NoSuchAConstant(opt_pos, name.to_string())
    }

    pub fn is_not_constant(opt_pos: Option<Position>, expr: &ExprAST) -> ParserError {
        ParserError::IsNotConstant(opt_pos, expr.clone())
    }

    pub fn cannot_get_block(opt_pos: Option<Position>) -> ParserError {
        ParserError::CannotGetBlock(opt_pos)
    }

    pub fn not_defvar_when_get(opt_pos: Option<Position>) -> ParserError {
        ParserError::NotDefvarWhenGet(opt_pos)
    }

    pub fn cannot_combine_with_previous_signed_declaration_specifier(opt_pos: Option<Position>, pre_pos: Position) -> ParserError {
        ParserError::CannotCombineWithPreviousUnsignedDeclarationSpecifier(opt_pos, pre_pos)
    }

    pub fn cannot_combine_with_previous_unsigned_declaration_specifier(opt_pos: Option<Position>, pre_pos: Position) -> ParserError {
        ParserError::CannotCombineWithPreviousUnsignedDeclarationSpecifier(opt_pos, pre_pos)
    }

    pub fn not_number_signed(opt_pos: Option<Position>, typ: &Type) -> ParserError {
        ParserError::NotNumberSigned(opt_pos, typ.clone())
    }

    pub fn not_number_unsigned(opt_pos: Option<Position>, typ: &Type) -> ParserError {
        ParserError::NotNumberUnsigned(opt_pos, typ.clone())
    }

    pub fn already_type_defined(opt_pos: Option<Position>, typ: &Type, pre_type: &Type, pre_pos: &Position) -> ParserError {
        ParserError::AlreadyTypeDefined { opt_pos, typ: typ.clone(), pre_type: pre_type.clone(), pre_pos: pre_pos.clone() }
    }

    pub fn syntax_error_while_parsing_struct(opt_pos: Option<Position>, sp: &SpecifierQualifier, type_or_variadic: &TypeOrVariadic) -> ParserError {
        ParserError::SyntaxErrorWhileParsingStruct(opt_pos, sp.clone(), type_or_variadic.clone())
    }

    pub fn not_self_after_ref(opt_pos: Option<Position>) -> ParserError {
        ParserError::NotSelfAfterRef(opt_pos)
    }

    pub fn no_constant_expr_parsing_struct_after_colon(opt_pos: Option<Position>) -> ParserError {
        ParserError::NoConstantExprParsingStructAfterColon(opt_pos)
    }

    pub fn not_symbol_parsing_enum(opt_pos: Option<Position>) -> ParserError {
        ParserError::NotSymbolParsingEnum(opt_pos)
    }

    pub fn enum_should_be_int(opt_pos: Option<Position>) -> ParserError {
        ParserError::EnumShouldBeInt(opt_pos)
    }

    pub fn should_be(opt_pos: Option<Position>, token_type_vec: Vec<Token>, typ: &Token) -> ParserError {
        ParserError::ShouldBe(opt_pos, token_type_vec, typ.clone())
    }

    pub fn cannot_to_be_unsigned(opt_pos: Option<Position>, num_type: &NumberType) -> ParserError {
        ParserError::CannotToBeUnsigned(opt_pos, num_type.clone())
    }

    pub fn no_type_for_struct_field(opt_pos: Option<Position>) -> ParserError {
        ParserError::NoTypeForStructField(opt_pos)
    }

    pub fn no_constant_expr_after_case(opt_pos: Option<Position>) -> ParserError {
        ParserError::NoConstantExprAfterCase(opt_pos)
    }

    pub fn labeled_statement_without_function(opt_pos: Option<Position>) -> ParserError {
        ParserError::LabeledStatementWithoutFunction(opt_pos)
    }

    pub fn no_id_for_goto_statement(opt_pos: Option<Position>) -> ParserError {
        ParserError::NoIdForGotoStatement(opt_pos)
    }

    pub fn not_function_define_in_impl(opt_pos: Option<Position>) -> ParserError {
        ParserError::NotFunctionDefineInImpl(opt_pos)
    }

    pub fn not_brace_left_or_for_while_parsing_impl(opt_pos: Option<Position>, tok: &Token) -> ParserError {
        ParserError::NotBraceLeftOrForWhileParsingImpl(opt_pos, tok.clone())
    }

    pub fn no_id_after_for_while_parsing_impl(opt_pos: Option<Position>, tok: &Token) -> ParserError {
        ParserError::NoIdAfterForWhileParsingImpl(opt_pos, tok.clone())
    }

    pub fn not_symbol_while_parsing_impl(opt_pos: Option<Position>) -> ParserError {
        ParserError::NotSymbolWhileParsingImpl(opt_pos)
    }

    pub fn no_such_a_type(opt_pos: Option<Position>, name: &str) -> ParserError {
        ParserError::NoSuchAType { opt_pos: opt_pos, name: name.to_string() }
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
            Self::NotNumber(_opt_pos, _expr) => write!(f, "not a number"),
            Self::NotPointer(_opt_pos, _typ) => write!(f, "not pointer"),
            Self::NotArray(_opt_pos, _typ) => write!(f, "not array"),
            Self::NotSymbol(_pos) => write!(f, "not symbol"),
            Self::NotFunction(_opt_pos, _typ) => write!(f, "not function"),
            Self::NotNumberType(_opt_pos, _typ) => write!(f, "not number type"),
            Self::NotNumberTypeToBeUnsigned(_opt_pos, _typ) => write!(f, "not number type to be unsigned"),
            Self::CannotToBeUnsigned(_opt_pos, _number_type) => write!(f, "cannot to be unsigned"),
            Self::SyntaxError(_opt_pos) => write!(f, "syntax error"),
            Self::IllegalEndOfInput(_opt_pos) => write!(f, "illegal end of input"),
            Self::WithoutExpectedToken{opt_pos: _, expected_token: _, real_token: _} => write!(f, "without expected token"),
            Self::NoSuchAOperator{opt_pos: _, token_type: _} => write!(f, "no such a operator"),
            Self::NeedExpr(_opt_pos) => write!(f, "need expr"),
            Self::NoTypeDefined(_opt_pos) => write!(f, "no type defined"),
            Self::NoExprWhileAccessArray(_opt_pos) => write!(f, "no expr while access array"),
            Self::NoIdAfterDot(_opt_pos) => write!(f, "no id after dot"),
            Self::NoIdAfterArrow(_opt_pos) => write!(f, "no id after arrow"),
            Self::NeedBraceRightOrCommaWhenParsingInitializerList(_opt_pos) => write!(f, "need brace right or comma when parsing initializer list"),
            Self::CannotConvertToUsize(_opt_pos, _const_expr) => write!(f, "cannot convert to usize"),
            Self::CannotApplyOperatorToFloat(_opt_pos, name) => write!(f, "cannot apply operator to float"),
            Self::CannotNotOfFloat(_opt_pos) => write!(f, "cannot not of float"),
            Self::AlreadyVarDefined(_opt_pos, name) => write!(f, "already var '{}' defined", name),
            Self::AlreadyTypeDefinedInEnv(_opt_pos, name) => write!(f, "already type '{}' defined", name),
            Self::AlreadyTypeDefined { opt_pos: _, typ: _, pre_type: _, pre_pos: _ } => write!(f, "already type defined"),
            Self::AccessSelfTypeWithoutImpl(_opt_pos) => write!(f, "access self type without impl"),
            Self::NoSuchAStruct(_opt_pos, name) => write!(f, "no such a struct '{}'", name),
            Self::NoSuchAConstant(_opt_pos, name) => write!(f, "no such a constant '{}'", name),
            Self::IsNotConstant(_opt_pos, _expr) => write!(f, "is not constant"),
            Self::CannotGetBlock(_opt_pos) => write!(f, "cannot get block"),
            Self::NotDefvarWhenGet(_opt_pos) => write!(f, "not defvar when get"),
            Self::CannotCombineWithPreviousSignedDeclarationSpecifier(_opt_pos, _pos) => write!(f, "cannot combine with previous signed declaration specifier"),
            Self::CannotCombineWithPreviousUnsignedDeclarationSpecifier(_opt_pos, _pos) => write!(f, "cannot combine with previous unsigned declaration specifier"),
            Self::NotNumberSigned(_opt_pos, typ) => write!(f, "not number signed"),
            Self::NotNumberUnsigned(_opt_pos, typ) => write!(f, "not number unsigned"),
            Self::SyntaxErrorWhileParsingStruct(_opt_pos, specifier_qualifier, type_or_variadic) => write!(f, "syntax error while parsing struct"),
            Self::NotSelfAfterRef(_opt_pos) => write!(f, "not self after ref"),
            Self::NoConstantExprParsingStructAfterColon(_opt_pos) => write!(f, "no constant expr parsing struct after colon"),
            Self::NotSymbolParsingEnum(_opt_pos) => write!(f, "not symbol parsing enum"),
            Self::EnumShouldBeInt(_opt_pos) => write!(f, "enum should be int"),
            Self::ShouldBe(_opt_pos, token_list, tok) => write!(f, "should be"),
            Self::NoTypeForStructField(_opt_pos) => write!(f, "no type for struct field"),
            Self::NoConstantExprAfterCase(_opt_pos) => write!(f, "no constant expr after case"),
            Self::LabeledStatementWithoutFunction(_opt_pos) => write!(f, "labeled statement without function"),
            Self::NoIdForGotoStatement(_opt_pos) => write!(f, "no id for goto statement"),
            Self::NotFunctionDefineInImpl(_opt_pos) => write!(f, "not function define in impl"),
            Self::NotBraceLeftOrForWhileParsingImpl(_opt_pos, _tok) => write!(f, "not brace left or for while parsing impl"),
            Self::NoIdAfterForWhileParsingImpl(_opt_pos, _tok) => write!(f, "no id after for while parsing impl"),
            Self::NotSymbolWhileParsingImpl(_opt_pos) => write!(f, "not symbol while parsing impl"),
            Self::NoSuchAType{opt_pos: _, name} => write!(f, "no such a type '{}'", name),
        }
    }
}

impl Error for ParserError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}
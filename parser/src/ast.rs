#![allow(dead_code)]

use std::collections::HashMap;
use std::rc::Rc;

use crate::{ParserError, Pattern, Initializer};
use super::{Type, Pointer, ConstExpr, Defines, StructDefinition, EnumDefinition};
use tokenizer::{Token, Position};
use super::initializer::ConstInitializer;

#[derive(Debug, Clone)]
pub struct DefVar {
    def_vars: Vec<(String, Rc<Type>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub body: Vec<AST>,
}

#[derive(Debug, Clone)]
struct BlockAnalyze<'a> {
    def_vars: Vec<(Rc<Type>, String)>,
    inner_blocks: Vec<&'a Block>,
}

impl Block {
    pub fn new_with_block(body: Vec<AST>) -> Block {
        Block {
            body: body,
        }
    }

    fn analyze<'a>(&'a self, pos: &Position) -> BlockAnalyze<'a> {
        let mut def_vars = Vec::new();
        let mut inner_blocks = Vec::new();

        for (_i, ast) in self.body.iter().enumerate() {
            if ast.is_def_var() {
                let (ds, decls) = ast.get_def_var(pos).unwrap();
                let typ = ds.get_type();
                for decl in decls {
                    let name = decl.get_declarator().get_name().to_string();
                    def_vars.push((typ.clone(), name));
                }
            }
            if ast.is_block() {
                inner_blocks.push(ast.get_block(pos).unwrap());
            }
        }

        BlockAnalyze {
            def_vars: def_vars,
            inner_blocks: inner_blocks,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
    Comma,
    ShiftLeft,
    ShiftRight,
    BitAnd,
    BitOr,
    BitXor,
}

impl BinOp {
    pub fn from_token(token_type: &Token, pos: &Position) -> Result<BinOp, ParserError> {
        match token_type {
            Token::Add => Ok(BinOp::Add),
            Token::Sub => Ok(BinOp::Sub),
            Token::Mul => Ok(BinOp::Mul),
            Token::Div => Ok(BinOp::Div),
            Token::Mod => Ok(BinOp::Mod),
            Token::Equal => Ok(BinOp::Equal),
            Token::NotEqual => Ok(BinOp::NotEqual),
            Token::Less => Ok(BinOp::Less),
            Token::LessEqual => Ok(BinOp::LessEqual),
            Token::Greater => Ok(BinOp::Greater),
            Token::GreaterEqual => Ok(BinOp::GreaterEqual),
            Token::And => Ok(BinOp::And),
            Token::Or => Ok(BinOp::Or),
            Token::ShiftLeft => Ok(BinOp::ShiftLeft),
            Token::ShiftRight => Ok(BinOp::ShiftRight),
            Token::BitAnd => Ok(BinOp::BitAnd),
            Token::BitOr => Ok(BinOp::BitOr),
            Token::BitXor => Ok(BinOp::BitXor),
            _ => Err(ParserError::no_such_a_operator(token_type.clone(), pos.clone())),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StorageClassSpecifier {
    auto: bool,
    register: bool,
    static_: bool,
    extern_: bool,
    typedef: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    StructOrUnionSpecifier,
    EnumSpecifier,
    TypedefName,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeQualifier {
    Const,
    Volatile,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpecifierQualifier {
    // storage class specifier
    pub auto: bool,
    pub register: bool,
    pub static_: bool,
    pub extern_: bool,
    pub typedef: bool,

    // type qualifier
    pub const_: bool,
    pub volatile: bool,
}

impl SpecifierQualifier {
    pub fn default() -> Self {
        SpecifierQualifier {
            auto: false,
            register: false,
            static_: false,
            extern_: false,
            typedef: false,
            const_: false,
            volatile: false,
        }
    }

    pub fn default_const() -> Self {
        SpecifierQualifier {
            auto: false,
            register: false,
            static_: false,
            extern_: false,
            typedef: false,
            const_: true,
            volatile: false,
        }
    }

    #[inline]
    pub fn is_const(&self) -> bool {
        self.const_
    }

    #[inline]
    pub fn is_volatile(&self) -> bool {
        self.volatile
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SpecifierQualifierOrVariadic {
    SQ(SpecifierQualifier),
    Variadic,
}

impl SpecifierQualifier {
    pub fn new() -> SpecifierQualifier {
        SpecifierQualifier {
            auto: false,
            register: false,
            static_: false,
            extern_: false,
            typedef: false,
            const_: false,
            volatile: false,
        }
    }

    pub fn set_auto(&mut self) -> Result<(), ParserError> {
        self.auto = true;
        Ok(())
    }

    pub fn set_register(&mut self) -> Result<(), ParserError> {
        self.register = true;
        Ok(())
    }

    pub fn set_static(&mut self) -> Result<(), ParserError> {
        self.static_ = true;
        Ok(())
    }

    pub fn set_extern(&mut self) -> Result<(), ParserError> {
        self.extern_ = true;
        Ok(())
    }

    pub fn set_const(&mut self) -> Result<(), ParserError> {
        self.const_ = true;
        Ok(())
    }

    pub fn set_volatile(&mut self) -> Result<(), ParserError> {
        self.volatile = true;
        Ok(())
    }

    pub fn set_typedef(&mut self) -> Result<(), ParserError> {
        self.typedef = true;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationSpecifier {
    pub typ: Rc<Type>,
    pub specifier_qualifier: SpecifierQualifier,
}

impl DeclarationSpecifier {
    pub fn new(typ: &Rc<Type>, sq: SpecifierQualifier) -> DeclarationSpecifier {
        DeclarationSpecifier {
            typ: Rc::clone(typ),
            specifier_qualifier: sq,
        }
    }

    #[inline]
    pub fn get_type(&self) -> &Rc<Type> {
        &self.typ
    }

    #[inline]
    pub fn set_type(&mut self, typ: &Rc<Type>) {
        self.typ = Rc::clone(typ);
    }

    #[inline]
    pub fn get_specifier_qualifier(&self) -> &SpecifierQualifier {
        &self.specifier_qualifier
    }

    #[inline]
    pub fn is_typedef(&self) -> bool {
        self.specifier_qualifier.typedef
    }

    #[inline]
    pub fn is_const(&self) -> bool {
        self.specifier_qualifier.const_
    }

    #[inline]
    pub fn is_volatile(&self) -> bool {
        self.specifier_qualifier.volatile
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclarationSpecifierOrVariadic {
    DS(DeclarationSpecifier),
    Variadic,
}

impl DeclarationSpecifierOrVariadic {
    #[inline]
    pub fn from_declaration_specifier(ds: DeclarationSpecifier) -> DeclarationSpecifierOrVariadic {
        DeclarationSpecifierOrVariadic::DS(ds)
    }

    #[inline]
    pub fn new_variadic() -> DeclarationSpecifierOrVariadic {
        DeclarationSpecifierOrVariadic::Variadic
    }

    #[inline]
    pub fn is_variadic(&self) -> bool {
        *self == DeclarationSpecifierOrVariadic::Variadic
    }

    #[inline]
    pub fn get_declaration_specifier(&self) -> Option<&DeclarationSpecifier> {
        match self {
            DeclarationSpecifierOrVariadic::DS(ds) => Some(ds),
            _ => None,
        }
    }

    #[inline]
    pub fn get_type(&self) -> Option<&Type> {
        match self {
            DeclarationSpecifierOrVariadic::DS(ds) => Some(ds.get_type()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declarator {
    pointer: Option<Pointer>,
    direct_declarator: Box<DirectDeclarator>,
}

impl Declarator {
    pub fn new(pointer: Option<Pointer>, dd: DirectDeclarator) -> Declarator {
        Declarator {
            pointer: pointer,
            direct_declarator: Box::new(dd),
        }
    }

    #[inline]
    pub fn get_position(&self) -> &Position {
        &self.direct_declarator.get_position()
    }

    #[inline]
    pub fn get_pointer(&self) -> &Option<Pointer> {
        &self.pointer
    }

    #[inline]
    pub fn get_name(&self) -> &str {
        self.direct_declarator.get_name()
    }

    #[inline]
    pub fn get_direct_declarator(&self) -> &DirectDeclarator {
        &*self.direct_declarator
    }

    pub fn make_type(&self, typ: &Rc<Type>) -> Rc<Type> {
        let typ = self.direct_declarator.make_array_type(typ);
        if let Some(p) = self.get_pointer() {
            p.make_type_to(&typ)
        }else{
            typ
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DirectDeclarator {
    Symbol(String, Position),
    Enclosed(Declarator, Position),
    ArrayDef(Box<DirectDeclarator>, Vec<u32>, Position),
    FunctionDef(Box<DirectDeclarator>, Params, Position),
}

impl DirectDeclarator {
    pub fn get_name(&self) -> &str {
        match self {
            Self::Symbol(id, _pos) => &id,
            Self::Enclosed(decl, _pos) => decl.get_name(),
            Self::ArrayDef(decl, _, _pos) => (**decl).get_name(),
            Self::FunctionDef(decl, _, _pos) => (**decl).get_name(),
        }
    }

    pub fn make_array_type(&self, typ: &Rc<Type>) -> Rc<Type> {
        match self {
            Self::ArrayDef(dd, size_list, _pos) => {
                let t = dd.make_array_type(typ);
                Rc::new(Type::Array { name: None, typ: Box::new(t), size_list: size_list.clone() })
            },
            Self::Enclosed(decl, _pos) => {
                decl.make_type(typ)
            },
            _ => typ.clone(),
        }
    }

    pub fn get_position(&self) -> &Position {
        match self {
            Self::ArrayDef(_, _, pos) => pos,
            Self::Enclosed(_, pos) => pos,
            Self::FunctionDef(_, _, pos) => pos,
            Self::Symbol(_, pos) => pos,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    declarator: Declarator,
    init_expr: Option<Initializer>,
}

impl Declaration {
    pub fn new(decl: Declarator, init: Option<Initializer>) -> Declaration {
        Declaration {
            declarator: decl,
            init_expr: init,
        }
    }

    #[inline]
    pub fn get_declarator(&self) -> &Declarator {
        &self.declarator
    }

    #[inline]
    pub fn get_init_expr(&self) -> &Option<Initializer> {
        &self.init_expr
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclarator {
    declarator: Option<Declarator>,
    opt_bit_size: Option<usize>,
}

impl StructDeclarator {
    pub fn new(opt_decl: Option<Declarator>, opt_bit_size: Option<ConstExpr>) -> StructDeclarator {
        let opt_bit_size = if let Some(const_expr) = opt_bit_size {
            Some(const_expr.to_usize().unwrap())
        }else{
            None
        };

        StructDeclarator {
            declarator: opt_decl,
            opt_bit_size: opt_bit_size,
        }
    }

    #[inline]
    pub fn get_declarator(&self) -> &Option<Declarator> {
        &self.declarator
    }

    #[inline]
    pub fn get_name<'a>(&'a self) -> Option<&'a str> {
        if let Some(decl) = &self.declarator {
            Some(decl.get_name())
        }else{
            None
        }
    }

    #[inline]
    pub fn get_bit_size(&self) -> &Option<usize> {
        &self.opt_bit_size
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaration {
    specifier_qualifier: SpecifierQualifier,
    typ: Option<Rc<Type>>,
    declarator_list: Vec<StructDeclarator>,
}

impl StructDeclaration {
    pub fn new(specifier_qualifier: SpecifierQualifier, typ: Option<Rc<Type>>, declarator_list: Vec<StructDeclarator>) -> StructDeclaration {
        StructDeclaration {
            specifier_qualifier,
            typ,
            declarator_list,
        }
    }

    pub fn get_specifier_qualifier(&self) -> &SpecifierQualifier {
        &self.specifier_qualifier
    }

    pub fn get_type(&self) -> &Option<Rc<Type>> {
        &self.typ
    }

    pub fn get_declarator_list(&self) -> &Vec<StructDeclarator> {
        &self.declarator_list
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AbstractDeclarator {
    pointer: Option<Pointer>,
    direct_abstract_declarator: Option<DirectAbstractDeclarator>,
}

impl AbstractDeclarator {
    pub fn new(pointer: Option<Pointer>, direct_abstract_declarator: Option<DirectAbstractDeclarator>) -> AbstractDeclarator {
        AbstractDeclarator {
            pointer: pointer,
            direct_abstract_declarator: direct_abstract_declarator,
        }
    }

    pub fn calc_type(&self, typ: &Rc<Type>) -> Rc<Type> {
        if let Some(_ptr) = &self.pointer {
            let t = Rc::new(Type::Pointer(Pointer::default(), Box::new(Rc::clone(typ))));
            if let Some(d_a_d) = &self.direct_abstract_declarator {
                d_a_d.calc_type(&t)
            }else{
                t
            }

        }else{
            if let Some(d_a_d) = &self.direct_abstract_declarator {
                d_a_d.calc_type(typ)
            }else{
                Rc::clone(typ)
            }
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum DirectAbstractDeclarator {
    Simple(Option<Box<AbstractDeclarator>>),
    FunCall(Box<DirectAbstractDeclarator>, Params),
    ArrayAccess(Box<DirectAbstractDeclarator>, Option<ConstExpr>),
}

impl DirectAbstractDeclarator {
    pub fn new_simple(abs_decl: Option<AbstractDeclarator>) -> DirectAbstractDeclarator {
        if let Some(decl) = abs_decl {
            DirectAbstractDeclarator::Simple(Some(Box::new(decl)))
        }else{
            DirectAbstractDeclarator::Simple(None)
        }
    }

    pub fn new_funcall(abs_decl: DirectAbstractDeclarator, params: Params) -> DirectAbstractDeclarator {
        DirectAbstractDeclarator::FunCall(Box::new(abs_decl), params)
    }

    pub fn new_array_access(abs_decl: DirectAbstractDeclarator, index: Option<ConstExpr>) -> DirectAbstractDeclarator {
        DirectAbstractDeclarator::ArrayAccess(Box::new(abs_decl), index)
    }

    pub fn calc_type(&self, typ: &Rc<Type>) -> Rc<Type> {
        match self {
            Self::Simple(opt_abs_decl) => {
                if let Some(abs_decl) = opt_abs_decl {
                    abs_decl.calc_type(typ)
                }else{
                    typ.clone()
                }
            },
            _ => panic!("cannot calc type of {:?}", self),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub specifiers: DeclarationSpecifier,
    pub declarator: Declarator,
    pub params: Params,
    pub body: Block,
    pub labels: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunProto {
    pub specifiers: DeclarationSpecifier,
    pub declarator: Declarator,
    pub params: Params,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunOrProt {
    Fun(Function),
    Proto(FunProto),
}

impl FunOrProt {
    pub fn get_specifiers(&self) -> &DeclarationSpecifier {
        match self {
            FunOrProt::Fun(f) => &f.specifiers,
            FunOrProt::Proto(p) => &p.specifiers,
        }
    }

    pub fn get_declarator(&self) -> &Declarator {
        match self {
            FunOrProt::Fun(f) => &f.declarator,
            FunOrProt::Proto(p) => &p.declarator,
        }
    }

    pub fn get_params(&self) -> &Params {
        match self {
            FunOrProt::Fun(f) => &f.params,
            FunOrProt::Proto(p) => &p.params,
        }
    }

    pub fn get_body(&self) -> Option<&Block> {
        match self {
            FunOrProt::Fun(f) => Some(&f.body),
            FunOrProt::Proto(_) => None,
        }
    }

    pub fn get_labels(&self) -> Option<&Vec<String>> {
        match self {
            FunOrProt::Fun(f) => Some(&f.labels),
            FunOrProt::Proto(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImplElement {
    FunOrProt(FunOrProt),
    DefVar {
        specifiers: DeclarationSpecifier,
        declaration: Vec<Declaration>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Case {
    cond: ConstExpr,
    stmt: Box<AST>,
    pos: Position,
}

impl Case {
    pub fn new(cond: ConstExpr, stmt: Box<AST>, pos: Position) -> Case {
        Case {
            cond,
            stmt,
            pos,
        }
    }

    pub fn get_cond(&self) -> &ConstExpr {
        &self.cond
    }

    pub fn get_stmt(&self) -> &AST {
        &&self.stmt
    }

    pub fn get_position(&self) -> &Position {
        &self.pos
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Switch {
    cond_expr: Option<Box<ExprAST>>,
    stmt: Option<Box<AST>>,

}

impl Switch {
    pub fn new(cond_expr: Option<Box<ExprAST>>, stmt: Option<AST>) -> Switch {
        Switch {
            cond_expr,
            stmt: match stmt {
                Some(stmt) => Some(Box::new(stmt)),
                None => None,
            }
        }
    }

    pub fn get_cond_expr(&self) -> Option<&ExprAST> {
        if let Some(e) = &self.cond_expr {
            Some(&*e)
        }else{
            None
        }
    }

    pub fn get_stmt(&self) -> Option<&AST> {
        if let Some(stmt) = &self.stmt {
            Some(&*stmt)
        }else{
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StructLiteral {
    NormalLiteral(Rc<Type>, HashMap<String, Box<ExprAST>>, Position),
    ConstLiteral(Rc<Type>, HashMap<String, ConstExpr>, Position),
}

impl StructLiteral {
    pub fn get_position(&self) -> &Position {
        match self {
            StructLiteral::NormalLiteral(_, _, pos) => pos,
            StructLiteral::ConstLiteral(_, _, pos) => pos,
        }
    }

    pub fn get_type(&self) -> &Rc<Type> {
        match self {
            StructLiteral::ConstLiteral(typ, _, _) => typ,
            StructLiteral::NormalLiteral(typ, _, _) => typ,
        }
    }

    pub fn is_const(&self) -> bool {
        match self {
            StructLiteral::ConstLiteral(..) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TupleLiteral {
    NormalLiteral {
        typ: Rc<Type>,
        list: Vec<Box<ExprAST>>,
        pos: Position,
    },
    ConstLiteral {
        typ: Rc<Type>,
        list: Vec<ConstExpr>,
        pos: Position,
    },
}

impl TupleLiteral {
    pub fn new_normal(typ: Rc<Type>, list: Vec<Box<ExprAST>>, pos: Position) -> TupleLiteral {
        TupleLiteral::NormalLiteral {
            typ: typ,
            list: list,
            pos: pos,
        }
    }

    pub fn new_const(typ: Rc<Type>, list: Vec<ConstExpr>, pos: Position) -> TupleLiteral {
        TupleLiteral::ConstLiteral {
            typ: typ,
            list: list,
            pos: pos,
        }
    }

    pub fn get_position(&self) -> &Position {
        match self {
            TupleLiteral::NormalLiteral { pos, .. } => pos,
            TupleLiteral::ConstLiteral { pos, .. } => pos,
        }
    }

    pub fn get_type(&self) -> &Rc<Type> {
        match self {
            TupleLiteral::NormalLiteral { typ, .. } => typ,
            TupleLiteral::ConstLiteral { typ, .. } => typ,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumLiteral {
    // None,
    Struct(StructLiteral),
    Tuple(TupleLiteral),
}

impl EnumLiteral {
    pub fn get_type(&self) -> &Rc<Type> {
        match self {
            // Self::None => &Rc::new(Type::Number(crate::NumberType::Int)),
            Self::Struct(literal) => literal.get_type(),
            Self::Tuple(literal) => literal.get_type(),
        }
    }

    pub fn get_struct_literal(&self) -> Option<&StructLiteral> {
        match self {
            EnumLiteral::Struct(literal) => Some(literal),
            _ => None,
        }
    }

    pub fn get_tuple_literal(&self) -> Option<&TupleLiteral> {
        match self {
            EnumLiteral::Tuple(literal) => Some(literal),
            _ => None,
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq)]
pub enum ExprAST {
    Assign(Box<ExprAST>, Box<ExprAST>, Position),
    OpAssign(BinOp, Box<ExprAST>, Box<ExprAST>, Position),
    PreInc(String, Position, Position),  // (id, id position, '++' position)
    PreDec(String, Position, Position),
    PostInc(String, Position, Position),
    PostDec(String, Position, Position),
    PreIncMemberAccess(Box<ExprAST>, Position),
    PreDecMemberAccess(Box<ExprAST>, Position),
    PostIncMemberAccess(Box<ExprAST>, Position),
    PostDecMemberAccess(Box<ExprAST>, Position),
    Char(char, Position),
    Int(i128, Position),
    Short(i16, Position),
    Long(i64, Position),
    UChar(u8, Position),
    UInt(u32, Position),
    UShort(u16, Position),
    ULong(u64, Position),
    LongLong(i128, Position),
    ULongLong(u128, Position),
    Float(f32, Position),
    Double(f64, Position),
    StringLiteral(String, Position),
    Symbol(String, Position),
    BinExpr(BinOp, Box<ExprAST>, Box<ExprAST>, Position),
    Not(Box<ExprAST>, Position),
    UnaryMinus(Box<ExprAST>, Position),
    UnaryTilda(Box<ExprAST>, Position),
    UnarySizeOfExpr(Box<ExprAST>, Position),
    UnarySizeOfTypeName(Rc<Type>, Position),
    // ExpressionPair(Box<ExprAST>, Box<ExprAST>, Position),
    Cast(Rc<Type>, Box<ExprAST>, Position),
    UnaryGetAddress(Box<ExprAST>, Position),
    UnaryPointerAccess(Box<ExprAST>, Position),  // *pointer
    MemberAccess(Box<ExprAST>, String, Position),
    PointerAccess(Box<ExprAST>, String, Position),
    TernaryOperator(Box<ExprAST>, Box<ExprAST>, Box<ExprAST>, Position),
    ArrayAccess(Box<ExprAST>, Vec<Box<ExprAST>>, Position),
    CallFunction(Box<ExprAST>, Vec<ExprAST>, Position),
    // InitializerList(Vec<ExprAST>, Position),
    DefVar {
        specifiers: DeclarationSpecifier,
        declarations: Vec<Declaration>,
        pos: Position,
    },
    _self(Position),
    SelfStaticSymbol(String, Position),
    StructStaticSymbol(String, String, Position),  // struct_name::feature_name
    StructLiteral(StructLiteral),
    UnionLiteral(Rc<Type>, Vec<(String, Box<ExprAST>)>, Position),
    UnionConstLiteral(Rc<Type>, Vec<(String, ConstExpr)>, Position),
    EnumLiteral(Rc<Type>, u64, EnumLiteral, Position),
    TupleLiteral(Vec<Box<ExprAST>>, Position),
    TupleMemberAccess(Box<ExprAST>, usize, Position),  // tpl.0
    TuplePointerAccess(Box<ExprAST>, usize, Position),  // tpl->0
}

impl ExprAST {
    pub fn is_signed(&self) -> Result<bool, ParserError> {
        match self {
            ExprAST::Char(_, _) | ExprAST::Int(_, _) | ExprAST::Short(_, _) | ExprAST::Long(_, _) => Ok(true),
            ExprAST::UChar(_, _) | ExprAST::UInt(_, _) | ExprAST::UShort(_, _) | ExprAST::ULong(_, _) => Ok(false),
            _ => Err(ParserError::not_number(self, self.get_position().clone())),
        }
    }

    pub fn get_position(&self) -> &Position {
        match self {
            ExprAST::Assign(_left, _right, pos) => pos,
            ExprAST::OpAssign(_op, _l_value, _r_value, pos) => pos,
            ExprAST::Char(_, pos) => pos,
            ExprAST::Int(_, pos) => pos,
            ExprAST::Short(_, pos) => pos,
            ExprAST::Long(_, pos) => pos,
            ExprAST::LongLong(_, pos) => pos,
            ExprAST::UChar(_, pos) => pos,
            ExprAST::UInt(_, pos) => pos,
            ExprAST::UShort(_, pos) => pos,
            ExprAST::ULong(_, pos) => pos,
            ExprAST::ULongLong(_, pos) => pos,
            ExprAST::StringLiteral(_string, pos) => pos,
            ExprAST::Float(_, pos) => pos,
            ExprAST::Double(_, pos) => pos,
            ExprAST::BinExpr(_op, _left, _right, pos) => pos,
            ExprAST::UnaryMinus(_expr, pos) => pos,
            ExprAST::UnaryTilda(_expr, pos) => pos,
            // ExprAST::UnaryNot(expr) => expr.get_type(env),
            ExprAST::UnarySizeOfExpr(_expr, pos) => pos,
            ExprAST::UnarySizeOfTypeName(_typ, pos) => pos,
            ExprAST::ArrayAccess(_expr, _index, pos) => pos,
            ExprAST::Symbol(_name, pos) => pos,
            ExprAST::SelfStaticSymbol(_, pos) => pos,
            ExprAST::StructStaticSymbol(_, _, pos) => pos,
            ExprAST::_self(pos) => pos,
            ExprAST::Not(_expr, pos) => pos,
            // ExprAST::ExpressionPair(_, _right, pos) => pos,
            ExprAST::Cast(_typ, _, pos) => pos,
            ExprAST::PreInc(_id, _sym_pos, pos) => pos,
            ExprAST::PreDec(_id, _sym_pos, pos) => pos,
            ExprAST::PostInc(_id, _sym_pos, pos) => pos,
            ExprAST::PostDec(_id, _sym_pos, pos) => pos,
            ExprAST::PreIncMemberAccess(_, pos) => pos,
            ExprAST::PreDecMemberAccess(_, pos) => pos,
            ExprAST::PostIncMemberAccess(_, pos) => pos,
            ExprAST::PostDecMemberAccess(_, pos) => pos,
            ExprAST::UnaryGetAddress(_boxed_ast, pos) => pos,
            ExprAST::UnaryPointerAccess(_boxed_ast, pos) => pos,  // *pointer
            ExprAST::MemberAccess(_boxed_ast, _field_name, pos) => pos,
            ExprAST::PointerAccess(_boxed_ast, _field_name, pos) => pos,
            ExprAST::TernaryOperator(_, _e1, _, pos) => pos,
            // ExprAST::InitializerList(_, pos) => pos,
            ExprAST::CallFunction(_, _, pos) => pos,
            ExprAST::DefVar { specifiers: _, declarations: _, pos } => pos,
            ExprAST::StructLiteral(literal) => literal.get_position(),
            ExprAST::UnionLiteral(_typ, _map, pos) => pos,
            ExprAST::UnionConstLiteral(_typ, _map, pos) => pos,
            ExprAST::EnumLiteral(_, _, _, pos) => pos,
            ExprAST::TupleLiteral(_, pos) => pos,
            ExprAST::TupleMemberAccess(_, _, pos) => pos,
            ExprAST::TuplePointerAccess(_, _, pos) => pos,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            ExprAST::Symbol(_name, _pos) => true,
            _ => false,
        }
    }

    pub fn is_member_access(&self) -> bool {
        match self {
            ExprAST::MemberAccess(_, _, _) | ExprAST::PointerAccess(_, _, _) => true,
            ExprAST::TupleMemberAccess(_, _, _) | ExprAST::TuplePointerAccess(_, _, _) => true,
            _ => false,
        }
    }

    pub fn get_symbol(&self) -> Result<(&String, &Position), ParserError> {
        match self {
            ExprAST::Symbol(name, pos) => Ok((name, pos)),
            _ => Err(ParserError::NotSymbol(self.get_position().clone()))
        }
    }

    pub fn to_const(&self, defs: &Defines, pos: &Position) -> Result<ConstExpr, ParserError> {
        match self {
            ExprAST::Char(num, _) => Ok(ConstExpr::Int(*num as i64, pos.clone())),
            ExprAST::Int(num, _) => Ok(ConstExpr::Int(*num as i64, pos.clone())),
            ExprAST::Short(num, _) => Ok(ConstExpr::Int(*num as i64, pos.clone())),
            ExprAST::Long(num, _) => Ok(ConstExpr::Int(*num as i64, pos.clone())),
            ExprAST::LongLong(num, _) => Ok(ConstExpr::LongLong(*num as i128, pos.clone())),
            ExprAST::UChar(num, _) => Ok(ConstExpr::Unsigned(*num as u64, pos.clone())),
            ExprAST::UInt(num, _) => Ok(ConstExpr::Unsigned(*num as u64, pos.clone())),
            ExprAST::UShort(num, _) => Ok(ConstExpr::Unsigned(*num as u64, pos.clone())),
            ExprAST::ULong(num, _) => Ok(ConstExpr::Unsigned(*num as u64, pos.clone())),
            ExprAST::ULongLong(num, _) => Ok(ConstExpr::ULongLong(*num as u128, pos.clone())),
            ExprAST::Float(num, _) => Ok(ConstExpr::Double(*num as f64, pos.clone())),
            ExprAST::Double(num, _) => Ok(ConstExpr::Double(*num as f64, pos.clone())),
            ExprAST::BinExpr(op, left, right, _) => {
                match op {
                    BinOp::Add => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        Ok(e1 + e2)
                    },
                    BinOp::Sub => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        Ok(e1 - e2)
                    },
                    BinOp::Mul => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        Ok(e1 * e2)
                    },
                    BinOp::Div => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        Ok(e1 / e2)
                    },
                    BinOp::Mod => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        Ok(e1 % e2)
                    },
                    BinOp::BitAnd => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        Ok((e1 & e2)?)
                    },
                    BinOp::BitOr => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        Ok((e1 | e2)?)
                    },
                    BinOp::BitXor => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        Ok((e1 ^ e2)?)
                    },
                    BinOp::Comma => {
                        let e2 = right.to_const(defs, pos)?;
                        Ok(e2)
                    },
                    BinOp::ShiftLeft => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        Ok((e1 << e2)?)
                    },
                    BinOp::ShiftRight => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        Ok((e1 >> e2)?)
                    },
                    BinOp::Equal => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        if e1 == e2 {
                            Ok(ConstExpr::Int(1, pos.clone()))
                        }else{
                            Ok(ConstExpr::Int(0, pos.clone()))
                        }
                    },
                    BinOp::NotEqual => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        if e1 != e2 {
                            Ok(ConstExpr::Int(1, pos.clone()))
                        }else{
                            Ok(ConstExpr::Int(0, pos.clone()))
                        }
                    },
                    BinOp::Less => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        if e1 < e2 {
                            Ok(ConstExpr::Int(1, pos.clone()))
                        }else{
                            Ok(ConstExpr::Int(0, pos.clone()))
                        }
                    },
                    BinOp::LessEqual => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        if e1 <= e2 {
                            Ok(ConstExpr::Int(1, pos.clone()))
                        }else{
                            Ok(ConstExpr::Int(0, pos.clone()))
                        }
                    },
                    BinOp::Greater => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        if e1 > e2 {
                            Ok(ConstExpr::Int(1, pos.clone()))
                        }else{
                            Ok(ConstExpr::Int(0, pos.clone()))
                        }
                    },
                    BinOp::GreaterEqual => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        if e1 >= e2 {
                            Ok(ConstExpr::Int(1, pos.clone()))
                        }else{
                            Ok(ConstExpr::Int(0, pos.clone()))
                        }
                    },
                    BinOp::And => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        let const2 = ConstExpr::Int(2, pos.clone());
                        let e3 = ((e1 % const2.clone()) & (e2 % const2))?;
                        let flag = e3 != ConstExpr::Int(0, pos.clone());

                        if flag {
                            Ok(ConstExpr::Int(1, pos.clone()))
                        }else{
                            Ok(ConstExpr::Int(0, pos.clone()))
                        }
                    },
                    BinOp::Or => {
                        let e1 = left.to_const(defs, pos)?;
                        let e2 = right.to_const(defs, pos)?;
                        let const2 = ConstExpr::Int(2, pos.clone());
                        let e3 = ((e1 % const2.clone()) | (e2 % const2))?;
                        let flag = e3 != ConstExpr::Int(0, pos.clone());

                        if flag {
                            Ok(ConstExpr::Int(1, pos.clone()))
                        }else{
                            Ok(ConstExpr::Int(0, pos.clone()))
                        }
                    },
                }
            },
            ExprAST::UnaryMinus(expr, _) => expr.to_const(defs, pos),
            ExprAST::Symbol(name, _) => {
                Ok(defs.get_const(name, pos)?)
            },
            ExprAST::Not(expr, _) => Ok((!expr.to_const(defs, pos)?)?),
            // ExprAST::ExpressionPair(_, right, _) => right.to_const(defs, pos),


            _ => Err(ParserError::is_not_constant(self, pos.clone())),
        }
    }

    pub fn is_array_access(&self) -> bool {
        match self {
            ExprAST::ArrayAccess(_, _, _) => true,
            _ => false,
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq)]
pub enum ToplevelAST {
    TypeDef(String, Rc<Type>, Position),
    DefineStruct {
        name: Option<String>,
        fields: StructDefinition,
        pos: Position,
    },
    DefineUnion {
        name: Option<String>,
        fields: StructDefinition,
        pos: Position,
    },
    DefineEnum {
        name: String,
        fields: EnumDefinition,
        pos: Position,
    },
    Impl {
        name: String,
        typ: Rc<Type>,
        for_type: Option<String>,
        defines: Vec<ImplElement>,
        pos: Position,
    },
    GlobalDefVar {
        specifiers: DeclarationSpecifier,
        declaration: Vec<Declaration>,
        pos: Position,
    },
    Function(Function, Position),
    FunProto(FunProto, Position),
}

impl ToplevelAST {
    pub fn new_impl(impl_name: &str, impl_type: Rc<Type>, for_something: Option<String>, defines: Vec<ImplElement>, pos: &Position) -> ToplevelAST {
        ToplevelAST::Impl { name: impl_name.to_string(), typ: impl_type, for_type: for_something, defines: defines, pos: pos.clone() }
    }

    pub fn get_position(&self) -> &Position {
        match self {
            ToplevelAST::DefineEnum { name: _, fields: _, pos } => pos,
            ToplevelAST::DefineStruct { name: _, fields: _, pos } => pos,
            ToplevelAST::DefineUnion { name: _, fields: _, pos } => pos,
            ToplevelAST::FunProto(_, pos) => pos,
            ToplevelAST::Function(_, pos) => pos,
            ToplevelAST::GlobalDefVar { specifiers: _, declaration: _, pos } => pos,
            ToplevelAST::Impl { name: _, typ: _, for_type: _, defines: _, pos } => pos,
            ToplevelAST::TypeDef(_, _, pos) => pos,
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq)]
pub enum AST {
    DefVar {
        specifiers: DeclarationSpecifier,
        declarations: Vec<Declaration>,
        pos: Position,
    },
    Block(Block, Position),
    Expr(Box<ExprAST>, Position),
    Return(Option<Box<ExprAST>>, Position),
    Labeled(String, Option<Box<AST>>, Position),
    Case(Case, Position),
    Default(Box<AST>, Position),
    Switch(Switch, Position),
    If(Box<ExprAST>, Box<AST>, Option<Box<AST>>, Position),
    IfLet {
        pattern_list: Vec<(Box<Pattern>, Position)>,
        pattern_name: Option<String>,
        expr: Box<ExprAST>,
        then: Box<AST>,
        else_: Option<Box<AST>>,
        pos: Position,
    },
    Match {
        expr: Box<ExprAST>,
        pattern_list_list: Vec<((Vec<(Box<Pattern>, Position)>, Option<String>), Box<AST>)>,
        pos: Position,
    },
    Loop {
        init_expr: Option<Box<ExprAST>>,
        pre_condition: Option<Box<ExprAST>>,
        body: Option<Box<AST>>,
        update_expr: Option<Box<ExprAST>>,
        post_condition: Option<Box<ExprAST>>,
        pos: Position,
    },
    Break(Position),
    Continue(Position),
    Goto(String, Position),
    _Self(Position),
    _self(Position),
}

impl AST {
    pub fn is_block(&self) -> bool {
        match self {
            AST::Block(_, _pos) => true,
            _ => false,
        }
    }

    pub fn is_def_var(&self) -> bool {
        match self {
            AST::DefVar{..} => true,
            _ => false,
        }
    }

    pub fn get_def_var(&self, pos: &Position) -> Result<(&DeclarationSpecifier, &Vec<Declaration>), ParserError> {
        match self {
            AST::DefVar{specifiers, declarations: declaration, pos: _} => {
                Ok((specifiers, declaration))
            },
            _ => Err(ParserError::not_defvar_when_get(pos.clone())),
        }
    }

    pub fn get_block(&self, pos: &Position) -> Result<&Block, ParserError> {
        match self {
            AST::Block(blk, _pos) => Ok(blk),
            _ => Err(ParserError::cannot_get_block(pos.clone())),
        }
    }

    pub fn get_position(&self) -> &Position {
        match self {
            AST::Block(_, pos) => pos,
            AST::Break(pos) => pos,
            AST::Case(_, pos) => pos,
            AST::Continue(pos) => pos,
            AST::Default(_, pos) => pos,
            AST::DefVar { specifiers: _, declarations: _, pos } => pos,
            AST::Expr(_, pos) => pos,
            AST::Goto(_, pos) => pos,
            AST::If(_, _, _, pos) => pos,
            AST::IfLet { pattern_list: _, pattern_name: _, expr: _, then: _, else_:_, pos } => pos,
            AST::Match { expr: _, pattern_list_list: _, pos } => pos,
            AST::Labeled(_, _, pos) => pos,
            AST::Loop { init_expr: _, pre_condition: _, body: _, update_expr: _, post_condition: _, pos } => pos,
            AST::Return(_, pos) => pos,
            AST::Switch(_, pos) => pos,
            AST::_Self(pos) => pos,
            AST::_self(pos) => pos,
        }
    }
 }

 #[derive(Debug, Clone, PartialEq)]
 pub struct Param (DeclarationSpecifier, Declarator, Position);

 impl Param {
    pub fn new(ds: DeclarationSpecifier, decl: Declarator, defs: &mut Defines, pos: &Position) -> Result<Param, ParserError> {
        let typ = ds.get_type();
        let name = decl.get_name();
        defs.set_var(name, typ, None, pos)?;
        Ok(Param(ds, decl, pos.clone()))
    }

    pub fn get_type(&self) -> Rc<Type> {
        let typ = self.0.get_type();
        self.1.make_type(typ)
    }

    #[inline]
    pub fn get_declaration_specifier(&self) -> &DeclarationSpecifier {
        &self.0
    }

    #[inline]
    pub fn get_name(&self) -> &str {
        self.1.get_name()
    }

    #[inline]
    pub fn get_position(&self) -> &Position {
        &self.2
    }

    #[inline]
    pub fn is_const(&self) -> bool {
        self.0.is_const()
    }

    #[inline]
    pub fn is_volatile(&self) -> bool {
        self.0.is_volatile()
    }
 }

 #[derive(Debug, Clone, PartialEq)]
pub enum CustSelf {
    Pointer(Rc<Type>),  // *self
    Ref(Rc<Type>),      // &self
    Direct(Rc<Type>),   // self
}

impl CustSelf {
    pub fn get_type(&self) -> &Rc<Type> {
        match self {
            CustSelf::Direct(typ) => typ,
            CustSelf::Pointer(typ) => typ,
            CustSelf::Ref(typ) => typ,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Params {
    _self: Option<CustSelf>,
    params: Vec<Param>,
    has_variadic: bool,
}

impl Params {
    pub fn new(params: Vec<Param>, has_variadic: bool) -> Params {
        Params {
            _self: None,
            params: params,
            has_variadic: has_variadic,
        }
    }

    pub fn new_with_self(_self: Option<CustSelf>, params: Vec<Param>, has_variadic: bool) -> Params {
        Params {
            _self: _self,
            params: params,
            has_variadic: has_variadic,
        }
    }

    pub fn from_vec(params: Vec<(DeclarationSpecifier, Declarator)>, has_variadic: bool, defs: &mut Defines, pos: &Position) -> Result<Params, ParserError> {
        let mut v = Vec::new();
        for (ds, decl) in params {
            v.push(Param::new(ds, decl, defs, pos)?);
        }

        Ok(Params {
            _self: None,
            params: v,
            has_variadic: has_variadic,
        })
    }

    pub fn from_vec_with_self(_self: Option<CustSelf>, params: Vec<(DeclarationSpecifier, Declarator)>, has_variadic: bool, defs: &mut Defines, pos: &Position) -> Result<Params, ParserError> {
        let mut v = Vec::new();
        for (ds, decl) in params {
            v.push(Param::new(ds, decl, defs, pos)?);
        }

        Ok(Params {
            _self: _self,
            params: v,
            has_variadic: has_variadic,
        })
    }

    pub fn len(&self) -> usize {
        self.params.len()
    }

    pub fn has_self(&self) -> bool {
        self._self.is_some()
    }

    pub fn get_self(&self) -> Option<&CustSelf> {
        self._self.as_ref()
    }

    pub fn get_params(&self) -> &Vec<Param> {
        &self.params
    }

    pub fn get_params_type(&self) -> Vec<Rc<Type>> {
        let mut v = Vec::new();

        if let Some(cust_self)  = &self._self {
            v.push(Rc::clone(cust_self.get_type()));
        }

        for param in &self.params {
            v.push(param.get_type());
        }
        v
    }

    pub fn has_variadic(&self) -> bool {
        self.has_variadic
    }

    pub fn set_variadic(&mut self, has_variadic: bool) {
        self.has_variadic = has_variadic;
    }
}

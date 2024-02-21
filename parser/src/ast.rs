#![allow(dead_code)]

use crate::ParserError;
use super::{Type, Pointer, ConstExpr, Defines, StructDefinition, EnumDefinition};
use tokenizer::{Token, Position};

#[derive(Debug, Clone)]
pub struct DefVar {
    def_vars: Vec<(String, Type)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub body: Vec<AST>,
}

#[derive(Debug, Clone)]
struct BlockAnalyze<'a> {
    def_vars: Vec<(Type, String)>,
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
            _ => Err(ParserError::no_such_a_operator(pos.clone(), token_type.clone())),
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
    pub typ: Type,
    pub specifier_qualifier: SpecifierQualifier,
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

impl DeclarationSpecifier {
    pub fn new(typ: Type, sq: SpecifierQualifier) -> DeclarationSpecifier {
        DeclarationSpecifier {
            typ: typ,
            specifier_qualifier: sq,
        }
    }

    #[inline]
    pub fn get_type(&self) -> &Type {
        &self.typ
    }

    #[inline]
    pub fn set_type(&mut self, typ: &Type) {
        self.typ = typ.clone();
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

    pub fn make_type(&self, typ: &Type) -> Type {
        let typ = self.direct_declarator.make_array_type(typ);
        if let Some(p) = self.get_pointer() {
            p.make_type_to(&typ)
        }else{
            typ.clone()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DirectDeclarator {
    Symbol(String, Position),
    Enclosed(Declarator, Position),
    ArrayDef(Box<DirectDeclarator>, Vec<ConstExpr>, Option<Initializer>, Position),
    FunctionDef(Box<DirectDeclarator>, Params, Position),
}

impl DirectDeclarator {
    pub fn get_name(&self) -> &str {
        match self {
            Self::Symbol(id, _pos) => &id,
            Self::Enclosed(decl, _pos) => decl.get_name(),
            Self::ArrayDef(decl, _, _, _pos) => (**decl).get_name(),
            Self::FunctionDef(decl, _, _pos) => (**decl).get_name(),
        }
    }

    pub fn make_array_type(&self, typ: &Type) -> Type {
        match self {
            Self::ArrayDef(dd, size_list, _, _pos) => {
                let t = dd.make_array_type(typ);
                Type::Array { name: None, typ: Box::new(t.clone()), size_list: size_list.clone() }
            },
            Self::Enclosed(decl, _pos) => {
                decl.make_type(typ)
            },
            _ => typ.clone(),
        }
    }

    pub fn get_position(&self) -> &Position {
        match self {
            Self::ArrayDef(_, _, _, pos) => pos,
            Self::Enclosed(_, pos) => pos,
            Self::FunctionDef(_, _, pos) => pos,
            Self::Symbol(_, pos) => pos,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    declarator: Declarator,
    init_expr: Option<Box<Initializer>>,
}

impl Declaration {
    pub fn new(decl: Declarator, init: Option<Box<Initializer>>) -> Declaration {
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
    pub fn get_init_expr(&self) -> &Option<Box<Initializer>> {
        &self.init_expr
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclarator {
    declarator: Option<Declarator>,
    opt_bit_size: Option<u64>,
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
    pub fn get_bit_size(&self) -> &Option<u64> {
        &self.opt_bit_size
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaration {
    specifier_qualifier: SpecifierQualifier,
    typ: Option<Type>,
    declarator_list: Vec<StructDeclarator>,
}

impl StructDeclaration {
    pub fn new(specifier_qualifier: SpecifierQualifier, typ: Option<Type>, declarator_list: Vec<StructDeclarator>) -> StructDeclaration {
        StructDeclaration {
            specifier_qualifier,
            typ,
            declarator_list,
        }
    }

    pub fn get_specifier_qualifier(&self) -> &SpecifierQualifier {
        &self.specifier_qualifier
    }

    pub fn get_type(&self) -> &Option<Type> {
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

    pub fn calc_type(&self, mut typ: &Type) -> Type {
        if let Some(ptr) = &self.pointer {
            let t = Type::Pointer(Pointer::default(), Box::new(typ.clone()));
            if let Some(d_a_d) = &self.direct_abstract_declarator {
                d_a_d.calc_type(&t)
            }else{
                t.clone()
            }

        }else{
            if let Some(d_a_d) = &self.direct_abstract_declarator {
                d_a_d.calc_type(typ)
            }else{
                typ.clone()
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

    pub fn calc_type(&self, typ: &Type) -> Type {
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
pub enum FunOrProto {
    Fun(Function),
    Proto(FunProto),
}

impl FunOrProto {
    pub fn get_specifiers(&self) -> &DeclarationSpecifier {
        match self {
            FunOrProto::Fun(f) => &f.specifiers,
            FunOrProto::Proto(p) => &p.specifiers,
        }
    }

    pub fn get_declarator(&self) -> &Declarator {
        match self {
            FunOrProto::Fun(f) => &f.declarator,
            FunOrProto::Proto(p) => &p.declarator,
        }
    }

    pub fn get_params(&self) -> &Params {
        match self {
            FunOrProto::Fun(f) => &f.params,
            FunOrProto::Proto(p) => &p.params,
        }
    }

    pub fn get_body(&self) -> Option<&Block> {
        match self {
            FunOrProto::Fun(f) => Some(&f.body),
            FunOrProto::Proto(_) => None,
        }
    }

    pub fn get_labels(&self) -> Option<&Vec<String>> {
        match self {
            FunOrProto::Fun(f) => Some(&f.labels),
            FunOrProto::Proto(_) => None,
        }
    }
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
pub enum Initializer {
    Simple(ExprAST, Position),
    ArrayOrStruct(Vec<Box<Initializer>>, Position),
    // Struct(Vec<Box<Initializer>>, Position),
}

impl Initializer {
    pub fn get_position(&self) -> &Position {
        match self {
            Self::ArrayOrStruct(_, pos) => pos,
            Self::Simple(_, pos) => pos,
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
    Char(u32, Position),
    Int(u128, Position),
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
    UnarySizeOfTypeName(Type, Position),
    // ExpressionPair(Box<ExprAST>, Box<ExprAST>, Position),
    Cast(Type, Box<ExprAST>, Position),
    UnaryGetAddress(Box<ExprAST>, Position),
    UnaryPointerAccess(Box<ExprAST>, Position),  // *pointer
    MemberAccess(Box<ExprAST>, String, Position),
    PointerAccess(Box<ExprAST>, String, Position),
    TernaryOperator(Box<ExprAST>, Box<ExprAST>, Box<ExprAST>, Position),
    ArrayAccess(Box<ExprAST>, Box<ExprAST>, Position),
    CallFunction(Box<ExprAST>, Vec<ExprAST>, Position),
    // InitializerList(Vec<ExprAST>, Position),
    DefVar {
        specifiers: DeclarationSpecifier,
        declarations: Vec<Declaration>,
        pos: Position,
    },
    _Self(Position),
    _self(Position),
}

impl ExprAST {
    pub fn is_signed(&self) -> Result<bool, ParserError> {
        match self {
            ExprAST::Char(_, _) | ExprAST::Int(_, _) | ExprAST::Short(_, _) | ExprAST::Long(_, _) => Ok(true),
            ExprAST::UChar(_, _) | ExprAST::UInt(_, _) | ExprAST::UShort(_, _) | ExprAST::ULong(_, _) => Ok(false),
            _ => Err(ParserError::not_number(self.get_position().clone(), self)),
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
            ExprAST::_Self(pos) => pos,
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


            _ => Err(ParserError::is_not_constant(pos.clone(), self)),
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
pub enum AST {
    TypeDef(String, Type, Position),
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
        name: Option<String>,
        fields: EnumDefinition,
        pos: Position,
    },
    Impl {
        name: String,
        typ: Type,
        for_type: Option<String>,
        functions: Vec<FunOrProto>,
        pos: Position,
    },
    DefVar {
        specifiers: DeclarationSpecifier,
        declarations: Vec<Declaration>,
        pos: Position,
    },
    GlobalDefVar {
        specifiers: DeclarationSpecifier,
        declaration: Vec<Declaration>,
        pos: Position,
    },
    Function(Function, Position),
    FunProto(FunProto, Position),
    Block(Block, Position),
    Expr(Box<ExprAST>, Position),
    Return(Option<Box<ExprAST>>, Position),
    Labeled(String, Option<Box<AST>>, Position),
    Case(Case, Position),
    Default(Box<AST>, Position),
    Switch(Switch, Position),
    If(Box<ExprAST>, Box<AST>, Option<Box<AST>>, Position),
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
    pub fn new_impl(impl_name: &str, impl_type: Type, for_something: Option<String>, functions: Vec<FunOrProto>, pos: &Position) -> AST {
        AST::Impl { name: impl_name.to_string(), typ: impl_type, for_type: for_something, functions: functions, pos: pos.clone() }
    }

    pub fn is_block(&self) -> bool {
        match self {
            AST::Block(_, _pos) => true,
            _ => false,
        }
    }

    pub fn get_block(&self, pos: &Position) -> Result<&Block, ParserError> {
        match self {
            AST::Block(blk, _pos) => Ok(blk),
            _ => Err(ParserError::cannot_get_block(pos.clone())),
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

    pub fn get_position(&self) -> &Position {
        match self {
            AST::Block(_, pos) => pos,
            AST::Break(pos) => pos,
            AST::Case(_, pos) => pos,
            AST::Continue(pos) => pos,
            AST::DefVar { specifiers: _, declarations: _, pos } => pos,
            AST::Default(_, pos) => pos,
            AST::DefineEnum { name: _, fields: _, pos } => pos,
            AST::DefineStruct { name: _, fields: _, pos } => pos,
            AST::DefineUnion { name: _, fields: _, pos } => pos,
            AST::Expr(_, pos) => pos,
            AST::FunProto(_, pos) => pos,
            AST::Function(_, pos) => pos,
            AST::GlobalDefVar { specifiers: _, declaration: _, pos } => pos,
            AST::Goto(_, pos) => pos,
            AST::If(_, _, _, pos) => pos,
            AST::Impl { name: _, typ: _, for_type: _, functions: _, pos } => pos,
            AST::Labeled(_, _, pos) => pos,
            AST::Loop { init_expr: _, pre_condition: _, body: _, update_expr: _, post_condition: _, pos } => pos,
            AST::Return(_, pos) => pos,
            AST::Switch(_, pos) => pos,
            AST::_Self(pos) => pos,
            AST::_self(pos) => pos,
            AST::TypeDef(_, _, pos) => pos,
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

    pub fn get_type(&self) -> Type {
        let typ = self.0.get_type();
        self.1.make_type(typ)
    }

    pub fn get_name(&self) -> &str {
        self.1.get_name()
    }

    #[inline]
    pub fn get_position(&self) -> &Position {
        &self.2
    }
 }

 #[derive(Debug, Clone, PartialEq)]
pub enum CustSelf {
    Pointer(Type),  // *self
    Ref(Type),      // &self
    Direct(Type),   // self
}

impl CustSelf {
    pub fn get_type(&self) -> &Type {
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

    pub fn get_params_type(&self) -> Vec<Type> {
        let mut v = Vec::new();

        if let Some(cust_self)  = &self._self {
            v.push(cust_self.get_type().clone());
        }

        for param in &self.params {
            v.push(param.get_type().clone());
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

#![allow(dead_code)]

use super::{Token, TokenType};
use super::{AST, ExprAST, BinOp, DeclarationSpecifier, DeclarationSpecifierOrVariadic, SpecifierQualifier};
use super::{AbstractDeclarator, DirectAbstractDeclarator, Declaration, ConstExpr, StructDeclaration, StructDeclarator, Declarator};
use super::Defines;
use super::ParserError;
use super::{Type, NumberType, TypeOrVariadic, StructDefinition, EnumDefinition, Pointer, Enumerator};
use super::{DirectDeclarator, Param, Params, CustSelf};
use crate::Position;
use std::iter::Peekable;
use std::slice::Iter;

pub struct Parser;

impl Parser {
    pub fn new() -> Self {
        Parser
    }

    fn parse(self, token_list: Vec<Token>) -> Result<Vec<AST>, ParserError> {
        let mut iter = token_list.iter().peekable();
        let mut defs = Defines::new();

        self.parse_translation_unit(&mut iter, &mut defs)
    }

    fn parse_translation_unit(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines) -> Result<Vec<AST>, ParserError> {
        let mut declarations = Vec::new();
        // let mut labels = Vec::new();

        // while let Some(_) = iter.peek() {
        //     if let Some(mut decl) = self.parse_external_declaration(iter, defs, &mut Some(&mut labels))? {
        //         match decl {
        //             AST::DefVar {specifiers, declarations: declaration} => {
        //                 decl = AST::GlobalDefVar{specifiers, declaration};
        //             },
        //             _ => {
        //             },
        //         }
        //         declarations.push(decl);
        //     }
        // }

        Ok(declarations)
    }

    fn make_global_def_var(&self, ds: DeclarationSpecifier, declarations: Vec<Declaration>, defs: &mut Defines) -> Result<AST, ParserError> {
        let typ = ds.get_type();

        for declaration in &declarations {
            let decl = declaration.get_declarator();
            let name = decl.get_name();
            let init = if let Some(expr) = declaration.get_init_expr() {
                Some((**expr).clone())
            }else{
                None
            };
            defs.set_var(name, typ, init)?;
        }

        Ok(AST::GlobalDefVar {
            specifiers: ds,
            declaration: declarations,
        })
    }

    fn make_global_def_array(&self, ds: DeclarationSpecifier, declarations: Vec<Declaration>, opt_size_list: &Vec<Option<ConstExpr>>, defs: &mut Defines) -> Result<AST, ParserError> {
        let typ = ds.get_type();

        for declaration in &declarations {
            let decl = declaration.get_declarator();
            let name = decl.get_name();
            let array_type = Type::Array { name: Some(name.to_string()), typ: Box::new(typ.clone()), opt_size_list: opt_size_list.clone() };
            let init = if let Some(expr) = declaration.get_init_expr() {
                Some((**expr).clone())
            }else{
                None
            };
            defs.set_var(name, &array_type, init)?;
        }

        Ok(AST::GlobalDefVar {
            specifiers: ds,
            declaration: declarations,
        })
    }

    fn parse_def_var(&self, ds: DeclarationSpecifier, declarations: Vec<Declaration>, defs: &mut Defines) -> Result<(DeclarationSpecifier, Vec<Declaration>), ParserError> {
        let typ = ds.get_type();

        for declaration in &declarations {
            let decl = declaration.get_declarator();
            let name = decl.get_name();
            let init = if let Some(expr) = declaration.get_init_expr() {
                Some((**expr).clone())
            }else{
                None
            };
            defs.set_var(name, typ, init)?;
        }

        Ok((ds, declarations))
    }

    fn parse_declaration_specifier(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<DeclarationSpecifierOrVariadic, ParserError> {
        let (sq, type_or_variadic) = self.parse_type_specifier_qualifier(iter, defs, labels)?;

        match type_or_variadic {
            TypeOrVariadic::Type(typ) => {
                let ds = DeclarationSpecifier::new(typ, sq);
                Ok(DeclarationSpecifierOrVariadic::DS(ds))
            },
            TypeOrVariadic::Variadic => Ok(DeclarationSpecifierOrVariadic::Variadic),
        }
    }

    fn parse_type_specifier_qualifier(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(SpecifierQualifier, TypeOrVariadic), ParserError> {
        let mut sq = SpecifierQualifier::new();
        let mut opt_signed: Option<(bool, Location)> = None;
        let mut opt_unsigned: Option<(bool, Location)> = None;
        let mut opt_type: Option<(Type, Location)> = None;

        loop {
            if let Some(tok) = iter.peek() {
                match tok.get_type() {
                    TokenType::Equal | TokenType::SemiColon | TokenType::ParenLeft | TokenType::BracketLeft | TokenType::Comma | TokenType::ParenRight => {
                        break;
                    },

                    //
                    // storage class specifiers
                    //
                    TokenType::Auto => {
                        iter.next();
                        sq.set_auto()?;
                    },
                    TokenType::Register => {
                        iter.next();
                        sq.set_register()?;
                    }
                    TokenType::Static => {
                        iter.next();
                        sq.set_static()?;
                    },
                    TokenType::Extern => {
                        iter.next();
                        sq.set_extern()?;
                    },
                    TokenType::Typedef => {
                        iter.next();
                        sq.set_typedef()?;
                    },

                    //
                    // type qualifiers
                    //
                    TokenType::Const => {
                        iter.next();
                        sq.set_const()?;
                    },
                    TokenType::Volatile => {
                        iter.next();
                        sq.set_volatile()?;
                    },
                    TokenType::TripleDot => {
                        iter.next();  // skip '...'
                        return Ok((sq, TypeOrVariadic::Variadic));
                    },

                    //
                    // Type
                    //
                    TokenType::Symbol(name) => {
                        if opt_type.is_some() {
                            break;
                        }

                        if let Some(t) = defs.get_type(name) {
                            iter.next();  // skip Symbol
                            opt_type = Some((t.clone(), tok.get_location().clone()));
                        }else{
                            break;
                        }
                    },
                    TokenType::_Self => {
                        if opt_type.is_some() {
                            break;
                        }

                        if let Some(t) = defs.get_type("Self") {
                            iter.next();  // skip 'Self'
                            opt_type = Some((t.clone(), tok.get_location().clone()));
                        }else{
                            break;
                        }
                    },
                    TokenType::Struct => {
                        iter.next();  // skip 'struct'

                        let tok2 = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(tok.get_location().clone())))?;
                        match tok2.get_type() {
                            TokenType::Symbol(name) => {
                                iter.next();  // skip Symbol

                                let tok3 = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(tok.get_location().clone())))?;
                                match tok3.get_type() {
                                    TokenType::BraceLeft => {
                                        iter.next();  // skip '{'

                                        let declaration = self.parse_struct_declaration_list(iter, defs, labels)?;
                                        let definition = StructDefinition::try_new(Some(name.clone()), Some(declaration))?;
                                        let type_struct = Type::struct_from_struct_definition(Some(name.clone()), definition.clone());
                                        defs.set_struct(name, definition)?;

                                        opt_type = Some((
                                            type_struct,
                                            tok.get_location().clone()
                                        ));

                                        self.parse_expected_token(iter, TokenType::BraceRight)?;
                                    },
                                    _ => {
                                        let type_struct = if let Some(t) = defs.get_type(&name) {
                                            t.clone()
                                        }else{
                                            let definition = StructDefinition::try_new(Some(name.clone()), None)?;
                                            Type::struct_from_struct_definition(Some(name.clone()), definition)
                                        };

                                        opt_type = Some((
                                            type_struct,
                                            tok.get_location().clone()
                                        ));
                                    },
                                }
                            },
                            TokenType::BraceLeft => {
                                iter.next();  // skip '{'
                                let declaration = self.parse_struct_declaration_list(iter, defs, labels)?;
                                let definition = StructDefinition::try_new(None, Some(declaration))?;
                                let type_struct = Type::struct_from_struct_definition(None, definition);
                                opt_type = Some((
                                    type_struct,
                                    tok.get_location().clone()
                                ));

                                self.parse_expected_token(iter, TokenType::BraceRight)?;
                            },
                            _ => {
                                return Err(ParserError::syntax_error(Some(tok2.get_location().clone()), file!(), line!(), column!()));
                            }
                        }
                    },
                    TokenType::Union => {
                        iter.next();  // skip 'union'

                        let tok2 = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(tok.get_location().clone())))?;
                        match tok2.get_type() {
                            TokenType::Symbol(name) => {
                                iter.next();  // skip Symbol

                                let tok3 = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(tok.get_location().clone())))?;
                                match tok3.get_type() {
                                    TokenType::BraceLeft => {
                                        iter.next();  // skip '{'

                                        let declaration = self.parse_struct_declaration_list(iter, defs, labels)?;
                                        let definition = StructDefinition::try_new(Some(name.clone()), Some(declaration))?;
                                        let type_union = Type::union_from_struct_definition(Some(name.clone()), definition.clone());
                                        defs.set_union(name, definition)?;

                                        opt_type = Some((
                                            type_union,
                                            tok.get_location().clone()
                                        ));

                                        self.parse_expected_token(iter, TokenType::BraceRight)?;
                                    },
                                    _ => {
                                        let type_union = if let Some(t) = defs.get_type(&name) {
                                            t.clone()
                                        }else{
                                            let definition = StructDefinition::try_new(Some(name.clone()), None)?;
                                            Type::union_from_struct_definition(Some(name.clone()), definition)
                                        };

                                        // let definition = StructDefinition::try_new(Some(name.clone()), None)?;
                                        // let type_union = Type::union_from_struct_definition(Some(name.clone()), definition);

                                        opt_type = Some((
                                            type_union,
                                            tok.get_location().clone()
                                        ));
                                    },
                                }
                            },
                            TokenType::BraceLeft => {
                                iter.next();  // skip '{'
                                let declaration = self.parse_struct_declaration_list(iter, defs, labels)?;
                                let definition = StructDefinition::try_new(None, Some(declaration))?;
                                let type_struct = Type::union_from_struct_definition(None, definition);
                                opt_type = Some((
                                    type_struct,
                                    tok.get_location().clone()
                                ));

                                self.parse_expected_token(iter, TokenType::BraceRight)?;
                            },
                            _ => {
                                return Err(ParserError::syntax_error(Some(tok2.get_location().clone()), file!(), line!(), column!()));
                            }
                        }
                    },
                    TokenType::Enum => {
                        iter.next();  // skip 'enum'

                        let tok2 = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(tok.get_location().clone())))?;
                        match tok2.get_type() {
                            TokenType::Symbol(name) => {
                                iter.next();  // skip Symbol
                
                                let tok3 = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(tok.get_location().clone())))?;
                                match tok3.get_type() {
                                    TokenType::BraceLeft => {
                                        iter.next();  // skip '{'
                
                                        let enum_list = self.parse_enumerator_list(iter, defs, labels)?;
                                        let definition = EnumDefinition::new(Some(name.clone()), Some(enum_list));
                                        let type_struct = Type::enum_from_enum_definition(Some(name.clone()), definition.clone());
                                        defs.set_enum(name, definition)?;
                
                                        opt_type = Some((
                                            type_struct,
                                            tok.get_location().clone()
                                        ));
                
                                        self.parse_expected_token(iter, TokenType::BraceRight)?;
                                    },
                                    _ => {
                                        let type_struct = if let Some(t) = defs.get_type(&name) {
                                            t.clone()
                                        }else{
                                            let definition = EnumDefinition::new(Some(name.clone()), None);
                                            Type::enum_from_enum_definition(Some(name.clone()), definition)
                                        };
                
                                        opt_type = Some((
                                            type_struct,
                                            tok.get_location().clone()
                                        ));
                                    },
                                }
                            },
                            TokenType::BraceLeft => {
                                iter.next();  // skip '{'
                                let enum_list = self.parse_enumerator_list(iter, defs, labels)?;
                                let definition = EnumDefinition::new(None, Some(enum_list));
                                let type_struct = Type::enum_from_enum_definition(None, definition);
                                opt_type = Some((
                                    type_struct,
                                    tok.get_location().clone()
                                ));
                
                                self.parse_expected_token(iter, TokenType::BraceRight)?;
                            },
                            _ => {
                                return Err(ParserError::syntax_error(Some(tok2.get_location().clone()), file!(), line!(), column!()));
                            }
                        }
                    },
                    TokenType::Signed => {
                        iter.next();
                        if let Some((true, pre_loc)) = opt_unsigned {
                            return Err(ParserError::cannot_combine_with_previous_unsigned_declaration_specifier(Some(tok.get_location().clone()), pre_loc.clone()));
                        }
                        if let Some((typ, _pos)) = &opt_type {
                            if ! typ.can_sign() {
                                return Err(ParserError::not_number_signed(Some(tok.get_location().clone()), &typ));
                            }
                        }
                        opt_signed = Some((true, tok.get_location().clone()));
                    },
                    TokenType::Unsigned => {
                        iter.next();
                        if let Some((true, pre_loc)) = opt_signed {
                            return Err(ParserError::cannot_combine_with_previous_signed_declaration_specifier(Some(tok.get_location().clone()), pre_loc.clone()));
                        }
                        if let Some((typ, _pos)) = &opt_type {
                            if ! typ.can_sign() {
                                return Err(ParserError::not_number_unsigned(Some(tok.get_location().clone()), &typ));
                            }
                        }
                        opt_unsigned = Some((true, tok.get_location().clone()));
                    },
                    TokenType::Void => {
                        iter.next();
                        if let Some((pre_type, pre_loc)) = &opt_type {
                            return Err(ParserError::already_type_defined(Some(tok.get_location().clone()), &Type::Void, &pre_type, pre_loc.clone()));
                        }
                        opt_type = Some((Type::Void, tok.get_location().clone()));
                    },
                    TokenType::_Bool => {
                        iter.next();
                        if let Some((pre_type, pre_loc)) = &opt_type {
                            return Err(ParserError::already_type_defined(Some(tok.get_location().clone()), &Type::Number(NumberType::_Bool), &pre_type, pre_loc.clone()));
                        }
                        opt_type = Some((Type::Number(NumberType::_Bool), tok.get_location().clone()));
                    }
                    TokenType::Char => {
                        iter.next();
                        if let Some((pre_type, pre_loc)) = &opt_type {
                            return Err(ParserError::already_type_defined(Some(tok.get_location().clone()), &Type::Number(NumberType::Char), &pre_type, pre_loc.clone()));
                        }
                        opt_type = Some((Type::Number(NumberType::Char), tok.get_location().clone()));
                    },
                    TokenType::Short => {
                        iter.next();
                        if let Some((pre_type, pre_loc)) = &opt_type {
                            match pre_type {
                                Type::Number(NumberType::Int) => {
                                    opt_type = Some((Type::Number(NumberType::Short), pre_loc.clone()));
                                    continue;
                                },
                                _ => {
                                    return Err(ParserError::already_type_defined(Some(tok.get_location().clone()), &Type::Number(NumberType::Short), &pre_type, pre_loc.clone()));
                                },
                            }
                        }
                        opt_type = Some((Type::Number(NumberType::Short), tok.get_location().clone()));
                    },
                    TokenType::Int => {
                        iter.next();
                        if let Some((pre_type, pre_loc)) = &opt_type {
                            match pre_type {
                                Type::Number(NumberType::Short) | Type::Number(NumberType::Long) | Type::Number(NumberType::LongLong) => {
                                    continue;
                                },
                                _ => {
                                    return Err(ParserError::already_type_defined(Some(tok.get_location().clone()), &Type::Number(NumberType::Int), &pre_type, pre_loc.clone()));
                                },
                            }
                        }
                        opt_type = Some((Type::Number(NumberType::Int), tok.get_location().clone()));
                    },
                    TokenType::Long => {
                        iter.next();
                        if let Some((pre_type, pre_loc)) = &opt_type {
                            match pre_type {
                                Type::Number(NumberType::Int) => {
                                    opt_type = Some((Type::Number(NumberType::Long), pre_loc.clone()));
                                    continue;
                                },
                                Type::Number(NumberType::Long) => {
                                    opt_type = Some((Type::Number(NumberType::LongLong), pre_loc.clone()));
                                    continue;
                                },
                                _ => {
                                    return Err(ParserError::already_type_defined(Some(tok.get_location().clone()), &Type::Number(NumberType::Long), &pre_type, pre_loc.clone()));
                                },
                            }
                        }
                        opt_type = Some((Type::Number(NumberType::Long), tok.get_location().clone()));
                    },
                    TokenType::Float => {
                        iter.next();
                        if let Some((pre_type, pre_loc)) = &opt_type {
                            return Err(ParserError::already_type_defined(Some(tok.get_location().clone()), &Type::Number(NumberType::Float), &pre_type, pre_loc.clone()));
                        }
                        opt_type = Some((Type::Number(NumberType::Float), tok.get_location().clone()));
                    },
                    TokenType::Double => {
                        iter.next();
                        if let Some((pre_type, pre_loc)) = &opt_type {
                            return Err(ParserError::already_type_defined(Some(tok.get_location().clone()), &Type::Number(NumberType::Double), &pre_type, pre_loc.clone()));
                        }
                        opt_type = Some((Type::Number(NumberType::Double), tok.get_location().clone()));
                    },
                    _ => {
                        break;
                    },
                }

            }else{
                break;
            }
        }

        let (typ, _pos) = opt_type.ok_or(ParserError::no_type_defined(None))?;
        let typ = if let Some((true, _)) = opt_unsigned {
            typ.to_unsigned()?
        }else{
            typ
        };

        Ok((sq, TypeOrVariadic::Type(typ)))
    }



    fn parse_type_name(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(SpecifierQualifier, TypeOrVariadic, Option<AbstractDeclarator>), ParserError> {
        let (sq, type_or_variadic) = self.parse_type_specifier_qualifier(iter, defs, labels)?;
        let opt_abstract_decl = self.parse_abstract_declarator(iter, defs, labels)?;
        Ok((sq, type_or_variadic, opt_abstract_decl))
    }

    fn parse_abstract_declarator(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AbstractDeclarator>, ParserError> {
        let pointer = self.parse_pointer(iter, defs)?;
        let direct_abs_decl = self.parse_direct_abstract_declarator(iter, defs, labels)?;

        if pointer.is_none() && direct_abs_decl.is_none(){
            Ok(None)
        }else{
            Ok(Some(AbstractDeclarator::new(pointer, direct_abs_decl)))
        }
    }

    fn parse_direct_abstract_declarator(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<DirectAbstractDeclarator>, ParserError> {
        let tok = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        if *tok.get_type() == TokenType::ParenLeft {
            iter.next();  // skip '('
            let abs_decl = self.parse_abstract_declarator(iter, defs, labels)?;
            self.parse_expected_token(iter, TokenType::ParenRight)?;  // skip ')'
            Ok(Some(self.parse_direct_abstract_declarator_sub(abs_decl, iter, defs, labels)?))

        }else{
            Ok(None)
        }
    }

    fn parse_direct_abstract_declarator_sub(&self, abs_decl: Option<AbstractDeclarator>, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<DirectAbstractDeclarator, ParserError> {
        let tok = iter.next().ok_or(ParserError::illegal_end_of_input(None))?;
        let typ = tok.get_type();

        let mut decl = DirectAbstractDeclarator::new_simple(abs_decl);
        loop {
            match typ {
                TokenType::ParenLeft => {
                    let param_list = self.parse_parameter_type_list(iter, defs, labels)?;
                    self.parse_expected_token(iter, TokenType::ParenRight)?;  // skip ')'

                    decl = DirectAbstractDeclarator::new_funcall(
                        decl,
                        param_list
                    );
                },
                TokenType::BracketLeft => {
                    let const_expr = self.parse_constant_expression(iter, defs, labels)?;
                    self.parse_expected_token(iter, TokenType::BracketRight)?;

                    decl = DirectAbstractDeclarator::new_array_access(
                        decl,
                        const_expr
                    );
                },
                _ => {
                    break Ok(decl);
                }
            }
        }
    }

    // fn parse_typedef_name(&self, _iter: &mut Peekable<Iter<(Token, Position)>>, _defs: &mut Defines) -> Result<Option<TypeSpecifier>, ParserError> {
    // }

    fn parse_declaration(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        let ds = self.parse_declaration_specifier(iter, defs, labels)?;
        let ds = ds.get_declaration_specifier().unwrap();

        // parse init_declarator
        let mut v = Vec::new();
        loop {
            let decl = self.parse_declarator(iter, defs, labels)?;
            let name = decl.get_name();

            if defs.exists_var(&name) {
                return Err(ParserError::already_var_defined(None, &name));
            }

            let tok = iter.next().ok_or(ParserError::illegal_end_of_input(None))?;

            match tok.get_type() {
                TokenType::SemiColon => {
                    let declaration = Declaration::new(decl, None);
                    v.push(declaration);
                    break;
                },
                TokenType::Comma => {
                    let declaration = Declaration::new(decl, None);
                    v.push(declaration);
                    continue;
                },
                TokenType::Assign => {
                    let init_expr = self.parse_initializer(iter, defs, labels)?;
                    let declaration = Declaration::new(decl, Some(Box::new(init_expr)));
                    v.push(declaration);

                    let tok3 = iter.next().ok_or(ParserError::illegal_end_of_input(None))?;
                    match tok3.get_type() {
                        TokenType::SemiColon => {
                            break;
                        },
                        TokenType::Comma => {
                            continue;
                        },
                        _ => {
                            break;
                        }
                    }
                },
                _ => {
                    return Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()));
                }
            }

            // check exists next init_declarator
            // let (tok, pos) = iter.next().ok_or(ParserError::illegal_end_of_input(None))?;
            // match tok {
            //
            //     _ => break;
            // }
        }

        let (ds, declarations) = self.parse_def_var(ds.clone(), v, defs)?;
        let ast = AST::DefVar { specifiers: ds, declarations };
        Ok(Some(ast))
    }

    // fn parse_init_declarator(&self, _typ: &Type, _iter: &mut Peekable<Iter<(Token, Position)>>, _defs: &mut Defines, _labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
    // }

    fn parse_initializer(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<ExprAST, ParserError> {
        let tok = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let init_expr;
        if *tok.get_type() == TokenType::BraceLeft {
            iter.next();  // skip '{'
            init_expr = self.parse_initializer_list(iter, defs, labels)?;
        }else{
            init_expr = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::need_expr(Some(tok.get_location().clone())))?;
        }
        Ok(init_expr)
    }

    fn parse_initializer_list(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<ExprAST, ParserError> {
        let mut list: Vec<ExprAST> = Vec::new();

        loop {
            let initializer = self.parse_initializer(iter, defs, labels)?;
            list.push(initializer);

            let tok2 = iter.next().ok_or(ParserError::illegal_end_of_input(None))?;
            match tok2.get_type() {
                TokenType::BraceRight => {
                    break;
                },
                TokenType::Comma => {
                    let tok3 = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
                    if *tok3.get_type() == TokenType::BraceRight {
                        iter.next();  // skip '}'
                        break;
                    }
                },
                _ => {
                    return Err(ParserError::need_brace_right_or_comma_when_parsing_initializer_list(Some(tok2.get_location().clone())));
                }
            }
        }

        Ok(ExprAST::InitializerList(list))
    }


    fn parse_struct_declaration_list(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Vec<StructDeclaration>, ParserError> {
        let mut list: Vec<StructDeclaration> = Vec::new();

        loop {
            let tok = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            if *tok.get_type() == TokenType::BraceRight {
                break;
            }

            let declaration = self.parse_struct_declaration(iter, defs, labels)?;
            list.push(declaration);
        }

        Ok(list)
    }

    fn parse_struct_declaration(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<StructDeclaration, ParserError> {
        let mut list: Vec<StructDeclarator> = Vec::new();

        let (sq, type_or_variadic) = self.parse_type_specifier_qualifier(iter, defs, labels)?;

        //
        // error check:
        //        check <storage-class-specifier>
        //        check variadic parameter
        //
        if sq.auto || sq.register || sq.static_ || sq.extern_ || sq.typedef || type_or_variadic.is_variadic() {
            return Err(ParserError::syntax_error_while_parsing_struct(None, &sq, &type_or_variadic));
        }

        let typ = type_or_variadic.get_type().unwrap();

        let tok = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        if *tok.get_type() == TokenType::BraceRight {
            return Ok(StructDeclaration::new(sq, Some(typ.clone()), list));
        }

        loop {
            let decl = self.parse_struct_declarator(iter, defs, labels)?;
            list.push(decl);

            let next_token = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            if *next_token.get_type() != TokenType::Comma {
                break;
            }else{
                iter.next();  // skip ','
            }
        }

        self.parse_expected_token(iter, TokenType::SemiColon)?;

        Ok(StructDeclaration::new(sq, Some(typ.clone()), list))
    }

    fn parse_struct_declarator(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<StructDeclarator, ParserError> {
        let tok = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        if *tok.get_type() == TokenType::Colon {
            iter.next();  // skip ':'

            let const_expr = self.parse_constant_expression(iter, defs, labels)?.ok_or(ParserError::no_constant_expr_parsing_struct_after_colon(Some(tok.get_location().clone())))?;
            Ok(StructDeclarator::new(None, Some(const_expr)))

        }else{
            let decl = self.parse_declarator(iter, defs, labels)?;

            let tok = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            if *tok.get_type() == TokenType::Colon {
                iter.next();  // skip ':'

                let const_expr = self.parse_constant_expression(iter, defs, labels)?.ok_or(ParserError::no_constant_expr_parsing_struct_after_colon(Some(tok.get_location().clone())))?;
                Ok(StructDeclarator::new(Some(decl), Some(const_expr)))

            }else{
                Ok(StructDeclarator::new(Some(decl), None))
            }
        }
    }

    fn parse_enumerator_list(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Vec<Enumerator>, ParserError> {
        let mut list: Vec<Enumerator> = Vec::new();
        let mut value: u32 = 0;

        loop {
            let tok = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            if *tok.get_type() == TokenType::BraceRight {
                // iter.next();  // skip '}'
                break;
            }

            let next_token = iter.next().ok_or(ParserError::illegal_end_of_input(None))?;
            let name = if let TokenType::Symbol(id) = next_token.get_type() {
                id
            }else{
                return Err(ParserError::not_symbol_parsing_enum(Some(tok.get_location().clone())));
            };

            let enumerator: Enumerator;
            let tok2 = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            match tok2.get_type() {
                TokenType::BraceRight => {
                    enumerator = Enumerator::new(name, value);
                },
                TokenType::Assign => {
                    iter.next();  // skip '='
                    let const_val = self.parse_constant_expression(iter, defs, labels)?.ok_or(ParserError::enum_should_be_int(None))?;
                    let int_val = const_val.as_u32_value();
                    enumerator = Enumerator::new(name, int_val);
                    value = int_val;

                    let tok3 = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
                    if *tok3.get_type() == TokenType::Comma {
                        iter.next();  // skip ','
                    }
                },
                TokenType::Comma => {
                    iter.next();  // skip ','
                    enumerator = Enumerator::new(name, value);
                },
                _ => {
                    return Err(ParserError::should_be(Some(tok2.get_location().clone()), vec![TokenType::BraceRight, TokenType::Assign], tok2.get_type()));
                }
            }

            value += 1;
            list.push(enumerator);
        }

        Ok(list)
    }

    fn parse_declarator(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Declarator, ParserError> {
        let opt_pointer = self.parse_pointer(iter, defs)?;
        let direct = self.parse_direct_declarator(iter, defs, labels)?;

        Ok(Declarator::new(opt_pointer, direct))
    }

    #[allow(irrefutable_let_patterns)]
    fn parse_pointer(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines) -> Result<Option<Pointer>, ParserError> {
        let tok = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;

        if *tok.get_type() != TokenType::Mul {
            return Ok(None);
        }

        iter.next();  // skip '*'

        let mut is_const = false;
        let mut is_volatile = false;
        while let tok2 = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(tok.get_location().clone())))? {
            match tok2.get_type() {
                TokenType::Const => {
                    iter.next();  // skip 'const'
                    is_const = true;
                },
                TokenType::Volatile => {  // skip 'volatile'
                    iter.next();
                    is_volatile = true;
                },
                TokenType::Mul => {  // '*'
                    break;
                },
                _ => {
                    // if is_const || is_volatile {
                    //     return Err(ParserError::syntax_error(Some(pos2.clone()), file!(), line!(), column!()));
                    // }
                    break;
                },
            }
        }

        let mut pointer = Pointer::new(is_const, is_volatile);

        if self.is_next_token(iter, &TokenType::Mul)? {
            let next_pointer = self.parse_pointer(iter, defs)?.unwrap();
            pointer.set_next_pointer(next_pointer);
        }

        Ok(Some(pointer))
    }

    fn is_next_token(&self, iter: &mut Peekable<Iter<Token>>, token_type: &TokenType) -> Result<bool, ParserError> {
        let tok2 = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        Ok(token_type == tok2.get_type())
    }

    pub fn parse_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_assignment_expression(iter, defs, labels)? {
            if let Some(tok) = iter.peek() {
                let token_type = tok.get_type();
                match token_type {
                    TokenType::Comma => {
                        if let Some(code) = self.parse_expression_sub(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }

            Ok(Some(ast))
        }else{  // None
            Ok(None)
        }
    }

    fn parse_expression_sub<'a>(&'a self, iter: &mut Peekable<Iter<Token>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some(tok) = iter.peek() {
                let typ = tok.get_type();
                match typ {
                    TokenType::Comma => {
                        iter.next(); // skip ','

                        if let Some(right) = self.parse_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::Comma, Box::new(left), Box::new(right)));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::Comma, Box::new(ast.clone()), Box::new(right)));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_direct_declarator(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<DirectDeclarator, ParserError> {
        let tok = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;

        let decl;
        match tok.get_type() {
            TokenType::Symbol(id) => {
                iter.next();
                decl = DirectDeclarator::Symbol(id.to_string());
            },
            TokenType::ParenLeft => {
                iter.next();  // skip '('

                let d = self.parse_declarator(iter, defs, labels)?;
                self.parse_expected_token(iter, TokenType::ParenRight)?;
                decl = DirectDeclarator::Enclosed(d);
            },
            _ => {
                return Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()))
            }
        }

        self.parse_direct_declarator_sub(decl, iter, defs, labels)
    }

    fn parse_direct_declarator_sub(&self, decl: DirectDeclarator, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<DirectDeclarator, ParserError> {
        let tok = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let cust_self;
        match tok.get_type() {
            TokenType::Mul => {  // '*'
                iter.next();  // skip '*'

                let tok2 = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(tok.get_location().clone())))?;
                if *tok2.get_type() != TokenType::_self {
                    return Err(ParserError::syntax_error(Some(tok2.get_location().clone()), file!(), line!(), column!()));
                }

                cust_self = Some(CustSelf::Pointer(defs.get_self_type()?.clone()));
            },
            _ => {
                cust_self = None;
            },
        }

        let mut result = decl;
        loop {
            let tok = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            match tok.get_type() {
                TokenType::ParenLeft => {  // define function
                    // read parameter type list
                    iter.next();  // skip '('

                    let next_tok = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(tok.get_location().clone())))?;
                    defs.add_new_function_local();
                    let param_type_list = if *next_tok.get_type() == TokenType::ParenRight {
                        iter.next();
                        Params::new_with_self(cust_self.clone(), Vec::new(), false)
                    }else{
                        let param_type_list = self.parse_parameter_type_list(iter, defs, labels)?;
                        self.parse_expected_token(iter, TokenType::ParenRight)?;  // skip ')'
                        param_type_list
                    };

                    result = DirectDeclarator::FunctionDef(Box::new(result), param_type_list);
                },
                TokenType::BracketLeft => {  // define array
                    iter.next();  // skip '['

                    let mut dimension = Vec::new();

                    let opt_const_expr = self.parse_constant_expression(iter, defs, labels)?;
                    dimension.push(opt_const_expr);
                    self.parse_expected_token(iter, TokenType::BracketRight)?;  // skip ']'

                    loop {
                        let tok2 = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(tok.get_location().clone())))?;

                        if *tok2.get_type() != TokenType::BracketLeft {
                            break;
                        }

                        iter.next();  // skip '['

                        let opt_const_expr = self.parse_constant_expression(iter, defs, labels)?;
                        dimension.push(opt_const_expr);
                        self.parse_expected_token(iter, TokenType::BracketRight)?;  // skip ']'
                    }

                    result =  DirectDeclarator::ArrayDef(Box::new(result), dimension);
                },
                _ => {
                    break Ok(result);
                },
            }
        }
    }

    fn parse_constant_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ConstExpr>, ParserError> {
        if let Some(expr) = self.parse_conditional_expression(iter, defs, labels)? {
            Ok(Some(expr.to_const(defs)?))
        }else{
            Ok(None)
        }
    }

    fn parse_conditional_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(expr) = self.parse_logical_or_expression(iter, defs, labels)? {
            if let Some(tok) = iter.peek() {
                if *tok.get_type() == TokenType::Question {
                    iter.next();  // skip '?'
                    let then_expr = self.parse_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(None, file!(), line!(), column!()))?;

                    self.parse_expected_token(iter, TokenType::Colon)?;
                    let else_expr = self.parse_conditional_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(None, file!(), line!(), column!()))?;

                    Ok(Some(ExprAST::TernaryOperator(Box::new(expr), Box::new(then_expr), Box::new(else_expr))))

                }else{
                    Ok(Some(expr))
                }
            }else{
                Ok(Some(expr))
            }

        }else{
            Ok(None)
        }
    }

    fn parse_logical_or_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_logical_and_expression(iter, defs, labels)? {
            if let Some(tok) = iter.peek() {
                match tok.get_type() {
                    TokenType::Or => {
                        if let Some(code) = self.parse_logical_or_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_logical_or_expression2(&self, iter: &mut Peekable<Iter<Token>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some(tok) = iter.peek() {
                let typ = tok.get_type();
                match typ {
                    TokenType::Or => {
                        let op = typ;
                        iter.next(); // skip '||'

                        if let Some(right) = self.parse_logical_and_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(left), Box::new(right)));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(ast.clone()), Box::new(right)));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_logical_and_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_inclusive_or_expression(iter, defs, labels)? {
            if let Some(tok) = iter.peek() {
                match tok.get_type() {
                    TokenType::And => {
                        if let Some(code) = self.parse_logical_and_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_logical_and_expression2(&self, iter: &mut Peekable<Iter<Token>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some(tok) = iter.peek() {
                let typ = tok.get_type();
                match typ {
                    TokenType::And => {
                        let op = typ;
                        iter.next(); // skip '&&'

                        if let Some(right) = self.parse_inclusive_or_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(left), Box::new(right)));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(ast.clone()), Box::new(right)));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_inclusive_or_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_exclusive_or_expression(iter, defs, labels)? {
            if let Some(tok) = iter.peek() {
                match tok.get_type() {
                    TokenType::BitOr => {
                        if let Some(code) = self.parse_inclusive_or_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_inclusive_or_expression2(&self, iter: &mut Peekable<Iter<Token>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some(tok) = iter.peek() {
                let typ = tok.get_type();
                match typ {
                    TokenType::BitOr => {
                        let op = typ;
                        iter.next(); // skip '|'

                        if let Some(right) = self.parse_exclusive_or_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(left), Box::new(right)));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(ast.clone()), Box::new(right)));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_exclusive_or_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_and_expression(iter, defs, labels)? {
            if let Some(tok) = iter.peek() {
                match tok.get_type() {
                    TokenType::BitXor => {
                        if let Some(code) = self.parse_exclusive_or_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_exclusive_or_expression2(&self, iter: &mut Peekable<Iter<Token>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some(tok) = iter.peek() {
                let typ = tok.get_type();
                match typ {
                    TokenType::BitXor => {
                        let op = typ;
                        iter.next(); // skip '^'

                        if let Some(right) = self.parse_and_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(left), Box::new(right)));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(ast.clone()), Box::new(right)));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_assignment_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_logical_or_expression(iter, defs, labels)? {
            if let Some(tok) = iter.peek() {
                match tok.get_type() {
                    TokenType::Question => {
                        iter.next();  // skip '?'
                        let then_expr = self.parse_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(None, file!(), line!(), column!()))?;

                        self.parse_expected_token(iter, TokenType::Colon)?;
                        let else_expr = self.parse_conditional_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(None, file!(), line!(), column!()))?;
    
                        return Ok(Some(ExprAST::TernaryOperator(Box::new(ast), Box::new(then_expr), Box::new(else_expr))));
                    },
                    TokenType::Assign | TokenType::AddAssign | TokenType::SubAssign | TokenType::MulAssign | TokenType::DivAssign | TokenType::ModAssign 
                     | TokenType:: ShiftLeftAssign | TokenType::ShiftRightAssign | TokenType::BitAndAssign | TokenType::BitOrAssign | TokenType::BitXorAssign =>
                    {
                        // if let Some(code) = self.parse_assignment_expression2(iter, ast.clone())? {
                        //     ast = code;
                        // }
                        ast = self.parse_assignment_expression2(iter, ast, defs, labels)?;
 
                    },
                    _ => (),  // do nothing
                }

                Ok(Some(ast))

            }else{
                Ok(Some(ast))
            }

        }else{  // None
            Ok(None)
        }
    }
        
    fn parse_assignment_expression2(&self, iter: &mut Peekable<Iter<Token>>, l_value: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<ExprAST, ParserError> {
        let mut result = l_value;
        loop {
            if let Some(tok) = iter.peek() {
                match tok.get_type() {
                    TokenType::Assign => {
                        iter.next();  // skip '='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::need_expr(Some(tok.get_location().clone())))?;

                        result = ExprAST::Assign(Box::new(result), Box::new(r_value));
                    },
                    TokenType::AddAssign => {
                        iter.next(); // skip '+='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()))?;

                        let add = ExprAST::BinExpr(BinOp::Add, Box::new(result.clone()), Box::new(r_value));
                        result = ExprAST::Assign(Box::new(result), Box::new(add));
                    },
                    TokenType::SubAssign => {
                        iter.next(); // skip '-='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()))?;

                        let sub = ExprAST::BinExpr(BinOp::Sub, Box::new(result.clone()), Box::new(r_value));
                        result = ExprAST::Assign(Box::new(result), Box::new(sub));
                    },
                    TokenType::MulAssign => {
                        iter.next(); // skip '*='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()))?;

                        let mul = ExprAST::BinExpr(BinOp::Mul, Box::new(result.clone()), Box::new(r_value));
                        result = ExprAST::Assign(Box::new(result), Box::new(mul));
                    },
                    TokenType::DivAssign => {
                        iter.next(); // skip '/='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()))?;

                        let div = ExprAST::BinExpr(BinOp::Div, Box::new(result.clone()), Box::new(r_value));
                        result = ExprAST::Assign(Box::new(result), Box::new(div));
                    },
                    TokenType::ModAssign => {
                        iter.next(); // skip '%='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()))?;

                        let res = ExprAST::BinExpr(BinOp::Mod, Box::new(result.clone()), Box::new(r_value));
                        result = ExprAST::Assign(Box::new(result), Box::new(res));
                    },
                    TokenType::ShiftLeftAssign => {
                        iter.next(); // skip '%='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()))?;

                        let res = ExprAST::BinExpr(BinOp::ShiftLeft, Box::new(result.clone()), Box::new(r_value));
                        result = ExprAST::Assign(Box::new(result), Box::new(res));
                    },
                    TokenType::ShiftRightAssign => {
                        iter.next(); // skip '%='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()))?;

                        let res = ExprAST::BinExpr(BinOp::ShiftRight, Box::new(result.clone()), Box::new(r_value));
                        result = ExprAST::Assign(Box::new(result), Box::new(res));
                    },
                    TokenType::BitAndAssign => {
                        iter.next(); // skip '%='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()))?;

                        let res = ExprAST::BinExpr(BinOp::BitAnd, Box::new(result.clone()), Box::new(r_value));
                        result = ExprAST::Assign(Box::new(result), Box::new(res));
                    },
                    TokenType::BitOrAssign => {
                        iter.next(); // skip '%='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()))?;

                        let res = ExprAST::BinExpr(BinOp::BitOr, Box::new(result.clone()), Box::new(r_value));
                        result = ExprAST::Assign(Box::new(result), Box::new(res));
                    },
                    TokenType::BitXorAssign => {
                        iter.next(); // skip '%='
                        let r_value = self.parse_assignment_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()))?;

                        let res = ExprAST::BinExpr(BinOp::BitXor, Box::new(result.clone()), Box::new(r_value));
                        result = ExprAST::Assign(Box::new(result), Box::new(res));
                    },

                    _ => break,
                }
            }else{
                break;
            }
        }
        Ok(result)
    }

    fn parse_primary_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {

        if let Some(tok) = iter.peek() {
            match &*tok.get_type() {
                TokenType::Symbol(name) => {
                    iter.next();  // skip symbol
                    Ok(Some(ExprAST::Symbol(name.clone())))
                },
                TokenType::_Self => {
                    iter.next();  // skip 'Self'
                    Ok(Some(ExprAST::_Self))
                },
                TokenType::_self => {
                    iter.next();  // skip 'self'
                    Ok(Some(ExprAST::_self))
                },
                TokenType::ParenLeft => {
                    iter.next(); // skip '('
                    let result = self.parse_expression(iter, defs, labels)?;
                    self.parse_expected_token(iter, TokenType::ParenRight)?;

                    Ok(result)
                },
                _ => {
                    self.parse_constant(iter, defs)
                },
            }
        }else{
            Ok(None)
        }
    }

    fn parse_expected_token(&self, iter: &mut Peekable<Iter<Token>>, expected: TokenType) -> Result<Location, ParserError> {
        if let Some(tok) = iter.next() {
            let typ = tok.get_type();
            if *typ == expected {
                Ok(tok.get_location().clone())
            }else{
                Err(ParserError::without_expected_token(Some(tok.get_location().clone()), expected, typ.clone()))
            }
        }else{
            Err(ParserError::illegal_end_of_input(None))
        }
    }

    fn parse_and_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_equality_expression(iter, defs, labels)? {
            if let Some(tok) = iter.peek() {
                match tok.get_type() {
                    TokenType::BitAnd => {
                        if let Some(code) = self.parse_and_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_and_expression2(&self, iter: &mut Peekable<Iter<Token>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some(tok) = iter.peek() {
                let typ = tok.get_type();
                match typ {
                    TokenType::BitAnd => {
                        let op = typ;
                        iter.next(); // skip '&'

                        if let Some(right) = self.parse_equality_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(left), Box::new(right)));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(ast.clone()), Box::new(right)));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_equality_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_relational_expression(iter, defs, labels)? {
            if let Some(tok) = iter.peek() {
                match tok.get_type() {
                    TokenType::Equal | TokenType::NotEqual => {
                        if let Some(code) = self.parse_equality_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_equality_expression2(&self, iter: &mut Peekable<Iter<Token>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some(tok) = iter.peek() {
                let typ = tok.get_type();
                match typ {
                    TokenType::Equal | TokenType::NotEqual => {
                        let op = typ;
                        iter.next(); // skip '==', '!='

                        if let Some(right) = self.parse_relational_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(left), Box::new(right)));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(ast.clone()), Box::new(right)));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_relational_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_shift_expression(iter, defs, labels)? {
            if let Some(tok) = iter.peek() {
                match tok.get_type() {
                    TokenType::Less | TokenType::LessEqual | TokenType::Greater | TokenType::GreaterEqual => {
                        if let Some(code) = self.parse_relational_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_relational_expression2(&self, iter: &mut Peekable<Iter<Token>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some(tok) = iter.peek() {
                let typ = tok.get_type();
                match typ {
                    TokenType::Less | TokenType::LessEqual | TokenType::Greater | TokenType::GreaterEqual => {
                        let op = typ;
                        iter.next(); // skip '<', '<=', '>', '>='

                        if let Some(right) = self.parse_shift_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(left), Box::new(right)));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(ast.clone()), Box::new(right)));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_shift_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_additive_expression(iter, defs, labels)? {
            if let Some(tok) = iter.peek() {
                match tok.get_type() {
                    TokenType::ShiftLeft | TokenType::ShiftRight => {
                        if let Some(code) = self.parse_shift_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_shift_expression2(&self, iter: &mut Peekable<Iter<Token>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some(tok) = iter.peek() {
                let typ = tok.get_type();
                match typ {
                    TokenType::ShiftLeft | TokenType::ShiftRight => {
                        let op = typ;
                        iter.next(); // skip '<<' or '>>'

                        if let Some(right) = self.parse_additive_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(left), Box::new(right)));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(ast.clone()), Box::new(right)));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_additive_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_multiplicative_expression(iter, defs, labels)? {
            if let Some(tok) = iter.peek() {
                match tok.get_type() {
                    TokenType::Add | TokenType::Sub => {
                        if let Some(code) = self.parse_additive_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))

        }else{  // None
            Ok(None)
        }
    }

    fn parse_additive_expression2(&self, iter: &mut Peekable<Iter<Token>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some(tok) = iter.peek() {
                let typ = tok.get_type();
                match typ {
                    TokenType::Add | TokenType::Sub => {
                        let op = typ;
                        iter.next(); // skip '+' or '-'

                        if let Some(right) = self.parse_multiplicative_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(left), Box::new(right)));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(ast.clone()), Box::new(right)));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_multiplicative_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_cast_expression(iter, defs, labels)? {
            if let Some(tok) = iter.peek() {
                match tok.get_type() {
                    TokenType::Mul | TokenType::Div | TokenType::Mod => {
                        if let Some(code) = self.parse_multiplicative_expression2(iter, ast.clone(), defs, labels)? {
                            ast = code;
                        }
                    },
                    _ => (),  // do nothing
                }
            }
            Ok(Some(ast))
        }else{  // None
            Ok(None)
        }
    }

    fn parse_multiplicative_expression2(&self, iter: &mut Peekable<Iter<Token>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some(tok) = iter.peek() {
                let typ = tok.get_type();
                match typ {
                    TokenType::Mul | TokenType::Div | TokenType::Mod => {
                        let op = typ;
                        iter.next(); // skip '+' or '-'

                        if let Some(right) = self.parse_cast_expression(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(left), Box::new(right)));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::from_token_type(op)?, Box::new(ast.clone()), Box::new(right)));
                            }
                        }else{
                            return Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()));
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok(result)
    }

    fn parse_cast_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let opt_expr = self.parse_unary_expression(iter, defs, labels)?;
        if opt_expr.is_some() {
            Ok(opt_expr)

        }else{
            let tok = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
            if *tok.get_type() == TokenType::ParenLeft {
                iter.next();  // skip '('
                let (_sq, type_or_variadic, _opt_abstract_decl) = self.parse_type_name(iter, defs, labels)?;
                let cast_type = type_or_variadic.get_type().ok_or(ParserError::no_type_defined(None))?;
                self.parse_expected_token(iter, TokenType::ParenRight)?;
                let expr = self.parse_cast_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(None, file!(), line!(), column!()))?;

                Ok(Some(ExprAST::Cast(cast_type.clone(), Box::new(expr))))
            }else{
                Ok(None)
            }
        }
    }

    fn parse_unary_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(ast) = self.parse_postfix_expression(iter, defs, labels)? {
            Ok(Some(ast))

        }else{
            let tok = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;

            match tok.get_type() {
                TokenType::Inc => {
                    iter.next();  // skip '++'

                    let expr = self.parse_unary_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()))?;
                    let one = ExprAST::Int(1);
                    let add = ExprAST::BinExpr(BinOp::Add, Box::new(expr.clone()), Box::new(one));

                    let inc = ExprAST::Assign(Box::new(expr), Box::new(add));
                    Ok(Some(inc))
                },
                TokenType::Dec => {
                    iter.next();  // skip '--'

                    let expr = self.parse_unary_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()))?;
                    let one = ExprAST::Int(1);
                    let add = ExprAST::BinExpr(BinOp::Sub, Box::new(expr.clone()), Box::new(one));

                    let inc = ExprAST::Assign(Box::new(expr), Box::new(add));
                    Ok(Some(inc))

                },
                TokenType::Add => {      // '+'
                    iter.next();  // skip '+'
                    self.parse_cast_expression(iter, defs, labels)
                },
                TokenType::Sub => {      // '-'
                    iter.next();  // skip '-'
                    if let Some(expr) = self.parse_cast_expression(iter, defs, labels)? {
                        Ok(Some(ExprAST::UnaryMinus(Box::new(expr))))
                    }else{
                        Ok(None)
                    }
                },
                TokenType::BitAnd => {   // '&'. get address
                    iter.next();  // skip '&'
                    if let Some(expr) = self.parse_cast_expression(iter, defs, labels)? {
                        Ok(Some(ExprAST::UnaryGetAddress(Box::new(expr))))
                    }else{
                        Ok(None)
                    }
                },
                TokenType::Mul => {      // '*'. pointer access
                    iter.next();  // skip '*'
                    if let Some(expr) = self.parse_cast_expression(iter, defs, labels)? {
                        Ok(Some(ExprAST::UnaryPointerAccess(Box::new(expr))))
                    }else{
                        Ok(None)
                    }
                },
                TokenType::Tilda => {    // '~'
                    iter.next();  // skip '~'
                    if let Some(expr) = self.parse_cast_expression(iter, defs, labels)? {
                        Ok(Some(ExprAST::UnaryTilda(Box::new(expr))))
                    }else{
                        Ok(None)
                    }
                },
                TokenType::Not => {      // '!'
                    iter.next();  // skip '!'
                    if let Some(expr) = self.parse_cast_expression(iter, defs, labels)? {
                        Ok(Some(ExprAST::Not(Box::new(expr))))
                    }else{
                        Ok(None)
                    }
                },
                TokenType::Sizeof => {
                    iter.next();  // skip 'sizeof'
                    if let Some(expr) = self.parse_unary_expression(iter, defs, labels)? {
                        Ok(Some(ExprAST::UnarySizeOfExpr(Box::new(expr))))
                    }else{
                        let (_sq, type_or_variadic, _opt_abstract_decl) = self.parse_type_name(iter, defs, labels)?;
                        let type_name = type_or_variadic.get_type().ok_or(ParserError::no_type_defined(None))?;
                        Ok(Some(ExprAST::UnarySizeOfTypeName(type_name.clone())))
                    }
                },
                TokenType::ParenRight => {
                    Ok(None)
                },
                _ => {
                    Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()))
                },
            }
        }
    }

    fn parse_postfix_expression(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_primary_expression(iter, defs, labels)? {
            loop {
                if let Some(tok) = iter.peek() {
                    match tok.get_type() {
                        TokenType::BracketLeft => {
                            iter.next();  // skip '['
                            let expr = self.parse_expression(iter, defs, labels)?.ok_or(ParserError::no_expr_while_access_array(None))?;
                            self.parse_expected_token(iter, TokenType::BracketRight)?;
                            ast = ExprAST::ArrayAccess(Box::new(ast), Box::new(expr));
                        },
                        TokenType::ParenLeft => {
                            iter.next();  // skip '('
                            let exprs = self.parse_expression(iter, defs, labels)?;
                            self.parse_expected_token(iter, TokenType::ParenRight)?;
                            let args: Vec<ExprAST> = self.exprs_to_vec(exprs)?;
                            ast = ExprAST::CallFunction(Box::new(ast), args);
                        },
                        TokenType::Dot => {
                            iter.next();  // skip '.'

                            let tok2 = iter.next().ok_or(ParserError::illegal_end_of_input(Some(tok.get_location().clone())))?;
                            match tok2.get_type() {
                                TokenType::Symbol(id) => {
                                    ast = ExprAST::MemberAccess(Box::new(ast), id.clone());
                                },
                                _ => return Err(ParserError::no_id_after_dot(Some(tok2.get_location().clone()))),
                            }
                        },
                        TokenType::MemberSelection => {
                            iter.next();  // skip '->'

                            let tok2 = iter.next().ok_or(ParserError::illegal_end_of_input(Some(tok.get_location().clone())))?;
                            match tok2.get_type() {
                                TokenType::Symbol(id) => {
                                    ast = ExprAST::PointerAccess(Box::new(ast), id.clone());
                                },
                                _ => return Err(ParserError::no_id_after_arrow(Some(tok2.get_location().clone()))),
                            }

                        },
                        TokenType::Inc => {
                            iter.next();  // skip '++'
                            // ast = ExprAST::Inc(Box::new(ast));

                            let one = ExprAST::Int(1);
                            let add = ExprAST::BinExpr(BinOp::Add, Box::new(ast.clone()), Box::new(one));
                            ast = ExprAST::Assign(Box::new(ast), Box::new(add));
                        },
                        TokenType::Dec => {
                            iter.next();  // skip '--'
                            // ast = ExprAST::Dec(Box::new(ast));

                            let one = ExprAST::Int(1);
                            let add = ExprAST::BinExpr(BinOp::Sub, Box::new(ast.clone()), Box::new(one));
                            ast = ExprAST::Assign(Box::new(ast), Box::new(add));
                        },
    
                        _ => break,
                    }

                }else{
                    break;
                }
        
            }

            Ok(Some(ast))
        }else{  // None
            Ok(None)
        }
    }

    fn exprs_to_vec(&self, exprs: Option<ExprAST>) -> Result<Vec<ExprAST>, ParserError> {
        let mut exprs = exprs;
        let mut result: Vec<ExprAST> = Vec::new();

        while let Some(expr) = exprs {
            match expr {
                ExprAST::BinExpr(BinOp::Comma, left, right) => {
                    result.push(*left);
                    exprs = Some(*right);
                },
                _ => {
                    result.push(expr);
                    break;
                },
            }
        }

        Ok(result)
    }

    fn parse_constant(&self, iter: &mut Peekable<Iter<Token>>, _defs: &mut Defines) -> Result<Option<ExprAST>, ParserError> {
        if let Some(tok) = iter.peek() {

            match &*tok.get_type() {
                TokenType::CharLiteral(ch) => {
                    iter.next();
                    Ok(Some(ExprAST::Char(*ch)))
                },
                TokenType::IntLiteral(num) => {
                    iter.next();
                    Ok(Some(ExprAST::Int(*num)))
                },
                TokenType::DoubleLiteral(num) => {
                    iter.next();
                    Ok(Some(ExprAST::Double(*num)))
                },
                TokenType::StringLiteral(s) => {
                    iter.next();
                    Ok(Some(ExprAST::StringLiteral(s.clone())))
                },
                // TODO: enumeration-constant


                _ => Ok(None),
            }
        }else{
            Ok(None)
        }
    }

    fn parse_parameter_type_list(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Params, ParserError> {
        let tok = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
        let cust_self;
        if *tok.get_type() == TokenType::BitAnd {  // '&'
            iter.next();  // skip '&'

            let tok2 = iter.next().ok_or(ParserError::illegal_end_of_input(Some(tok.get_location().clone())))?;
            if *tok2.get_type() != TokenType::_self {
                return Err(ParserError::not_self_after_ref(Some(tok2.get_location().clone())));
            }

            cust_self = Some(CustSelf::Ref(defs.get_self_type()?.clone()));

            let tok3 = iter.peek().ok_or(ParserError::illegal_end_of_input(Some(tok.get_location().clone())))?;
            if *tok3.get_type() == TokenType::ParenRight {
                let params = Params::new_with_self(cust_self, Vec::new(), false);
                return Ok(params);
            }

        }else{
            cust_self = None;
        }

        let (param_type_list, mut has_variadic) = self.parse_parameter_list(iter, defs, labels)?;
        if let Some(tok) = iter.peek() {
            match *tok.get_type() {
                TokenType::Comma => {
                    iter.next();  // skip ','

                    let tok2 = iter.next().ok_or(ParserError::illegal_end_of_input(Some(tok.get_location().clone())))?;
                    if *tok2.get_type() == TokenType::TripleDot {
                        has_variadic = true;
                    }else{
                        return Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()));
                    }
                },
                TokenType::ParenRight => {

                    // do nothing


                },
                _ => {
                    // do nothing
                }
            }
        }

        let params = Params::new_with_self(cust_self, param_type_list, has_variadic);
        Ok(params)
    }

    fn parse_parameter_list(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(Vec<Param>, bool), ParserError> {
        self.parse_parameter_declaration(iter, defs, labels)
    }

    fn parse_parameter_declaration(&self, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(Vec<Param>, bool), ParserError> {
        let mut list = Vec::new();
        let mut has_variadic = false;
        let ds_or_variadic = self.parse_declaration_specifier(iter, defs, labels)?;

        match ds_or_variadic {
            DeclarationSpecifierOrVariadic::DS(ds) => {
                let decl = self.parse_declarator(iter, defs, &mut None)?;
                list.push(Param::new(ds, decl, defs)?);
        
                let tok = iter.peek().ok_or(ParserError::illegal_end_of_input(None))?;
                match tok.get_type() {
                    TokenType::Comma => {
                        (list, has_variadic) = self.parse_parameter_declaration2(list, iter, defs, labels)?;
                    },
                    TokenType::ParenRight => (),  // do nothing
                    _ => return Err(ParserError::syntax_error(Some(tok.get_location().clone()), file!(), line!(), column!()))
                }
            },
            DeclarationSpecifierOrVariadic::Variadic => {
                has_variadic = true;
            }
        }

        Ok((list, has_variadic))
    }

    fn parse_parameter_declaration2(&self, mut list: Vec<Param>, iter: &mut Peekable<Iter<Token>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<(Vec<Param>, bool), ParserError> {
        let mut has_variadic = false;
        loop {
            if let Some(tok) = iter.peek() {
                match tok.get_type() {
                    TokenType::Comma => {
                        iter.next(); // skip ','

                        let ds_or_variadic = self.parse_declaration_specifier(iter, defs, labels)?;
                        match ds_or_variadic {
                            DeclarationSpecifierOrVariadic::DS(ds) => {
                                let decl2 = self.parse_declarator(iter, defs, &mut None)?;
                                list.push(Param::new(ds, decl2, defs)?);
                            },
                            DeclarationSpecifierOrVariadic::Variadic => {
                                has_variadic = true;
                                break;
                            },
                        }
                    },
                    _ => break,
                }
            }else{
                break;
            }
        }

        Ok((list, has_variadic))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::Tokenizer;

    fn parse_constant_from_str(src: &str) -> Result<Option<ExprAST>, ParserError> {
        let tokenizer = Tokenizer::new();
        let token_list = tokenizer.tokenize(src).unwrap();
        let mut iter = token_list.iter().peekable();
        let parser = Parser::new();
        let mut defs = Defines::new();
        parser.parse_constant(&mut iter, &mut defs)
    }

    #[test]
    fn parse_constant() {
        let src = "'a'";
        let ast = parse_constant_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::Char('a' as u32)
        );

        let src = "123";
        let ast = parse_constant_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::Int(123)
        );

        let src = "1.2";
        let ast = parse_constant_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::Double(1.2)
        );

        let src = "\"Hello world!\"";
        let ast = parse_constant_from_str(src).unwrap().unwrap();

        assert_eq!(
            ast,
            ExprAST::StringLiteral("Hello world!".into())
        );
    }
}
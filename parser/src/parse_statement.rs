use super::{Position, Token};
use super::ParserError;
use super::ast::{AST, ExprAST, BinOp, Block};
use super::defines::*;
use super::parse::Parser;
use super::{Switch, Case};

use std::slice::Iter;
use std::iter::Peekable;

impl Parser {
    pub fn parse_statement(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        if let Some((tok, pos)) = iter.peek() {
            match tok {
                // empty statement
                Token::SemiColon => {
                    iter.next();  // skip ';'
                    Ok(None)
                },
                // compound statement
                Token::BraceLeft => {
                    let block = self.parse_compound_statement(iter, defs, labels)?;
                    Ok(Some(AST::Block(block, pos.clone())))
                },
                // selection statement
                Token::If => {
                    iter.next();  // skip 'if'

                    let (next_tok, next_pos) = iter.peek().unwrap();
                    if next_tok.is_symbol() {
                        let sym_name = next_tok.get_symbol_name().unwrap();

                        if sym_name == "let" {
                            iter.next();  // skip 'let'


                            self.parse_expected_token(iter, Token::ParenLeft)?;  // skip '('

                            let (pattern_list, _name) = self.parse_pattern(iter, defs, labels)?;

                            self.parse_expected_token(iter, Token::Assign)?;  // skip '='

                            let expr = self.parse_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                            self.parse_expected_token(iter, Token::ParenRight)?;  // skip ')'

                            let then = self.parse_statement(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;
    
                            if let Some((tok2, pos2)) = iter.peek() {
                                if *tok2 == Token::Else {
                                    iter.next();  // skip 'else'
    
                                    let _else = self.parse_statement(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos2.clone()))?;
                                    let ast = AST::IfLet {
                                        pattern_list,
                                        expr: Box::new(expr),
                                        then: Box::new(then),
                                        else_: Some(Box::new(_else)),
                                        pos: pos.clone(),
                                    };
                                    Ok(Some(ast))
                                }else{
                                    let ast = AST::IfLet {
                                        pattern_list,
                                        expr: Box::new(expr),
                                        then: Box::new(then),
                                        else_: None,
                                        pos: pos.clone(),
                                    };
                                    Ok(Some(ast))
                                }
                            }else{
                                let ast = AST::IfLet {
                                    pattern_list,
                                    expr: Box::new(expr),
                                    then: Box::new(then),
                                    else_: None,
                                    pos: pos.clone(),
                                };
                                Ok(Some(ast))
                            }

                        }else{
                            // println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                            return Err(ParserError::syntax_error(next_pos.clone()));
                        }
                    } else {
                        self.parse_expected_token(iter, Token::ParenLeft)?;  // skip '('

                        let cond = self.parse_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                        self.parse_expected_token(iter, Token::ParenRight)?;  // skip ')'

                        let then = self.parse_statement(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                        if let Some((tok2, pos2)) = iter.peek() {
                            if *tok2 == Token::Else {
                                iter.next();  // skip 'else'

                                let _else = self.parse_statement(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos2.clone()))?;
                                Ok(Some(AST::If(Box::new(cond), Box::new(then), Some(Box::new(_else)), pos.clone())))
                            }else{
                                Ok(Some(AST::If(Box::new(cond), Box::new(then), None, pos.clone())))
                            }
                        }else{
                            Ok(Some(AST::If(Box::new(cond), Box::new(then), None, pos.clone())))
                        }
                    }
                },
                Token::Switch => {
                    iter.next();  // skip 'switch'
                    self.parse_expected_token(iter, Token::ParenLeft)?;
                    let expr = self.parse_expression(iter, defs, labels)?;
                    self.parse_expected_token(iter, Token::ParenRight)?;

                    let stmt = self.parse_statement(iter, defs, labels)?;

                    let cond_expr = match expr {
                        Some(ast) => Some(Box::new(ast)),
                        None => None,
                    };
                    let switch = Switch::new(cond_expr, stmt);

                    Ok(Some(AST::Switch(switch, pos.clone())))
                },
                // iteration statement
                Token::While => {
                    iter.next();  // skip 'while'
                    self.parse_expected_token(iter, Token::ParenLeft)?;  // skip '('

                    let condition = self.parse_expression(iter, defs, labels)?;

                    self.parse_expected_token(iter, Token::ParenRight)?;  // skip ')'

                    let stmt = self.parse_statement(iter, defs, labels)?;

                    Ok(Some(AST::Loop {
                        init_expr: None,
                        pre_condition: if let Some(cond) = condition {
                            Some(Box::new(cond))
                        }else{
                            None
                        },
                        body: if let Some(stmt) = stmt {
                            Some(Box::new(stmt))
                        }else{
                            None
                        },
                        update_expr: None,
                        post_condition: None,
                        pos: pos.clone(),
                    }))
                },
                Token::Do => {
                    iter.next();                                           // skip 'do'

                    let (next_tok, next_pos) = iter.peek().unwrap();
                    if next_tok.is_symbol() {
                        let sym_name = next_tok.get_symbol_name().unwrap();

                        if sym_name == "match" {
                            iter.next();  // skip 'match'

                            self.parse_expected_token(iter, Token::ParenLeft)?;  // skip '('
                            let expr = self.parse_expression(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;
                            self.parse_expected_token(iter, Token::ParenRight)?;  // skip ')'

                            self.parse_expected_token(iter, Token::BraceLeft)?;  // skip '{'

                            let mut list = Vec::new();
                            loop {
                                let (tok2, _pos2) = iter.peek().unwrap();

                                if *tok2 == Token::BraceRight {
                                    iter.next();  // skip '}'
                                    break;
                                }

                                let (pattern_list, _name) = self.parse_pattern(iter, defs, labels)?;
 
                                self.parse_expected_token(iter, Token::WhenMatch)?;  // skip '=>'

                                let then = self.parse_statement(iter, defs, labels)?.ok_or(ParserError::syntax_error(pos.clone()))?;

                                list.push((pattern_list, Box::new(then)));

                                let (tok3, _pos3) = iter.peek().unwrap();
                                match tok3 {
                                    Token::Comma => {
                                        iter.next();  // skip '<'
                                    },
                                    Token::BraceRight => {},  // do nothing
                                    _ => {
                                        return Err(ParserError::syntax_error(next_pos.clone()));
                                    }
                                }
                            }

                            let ast = AST::Match {
                                expr: Box::new(expr),
                                pattern_list_list: list,
                                pos: pos.clone(),
                            };
                            Ok(Some(ast))

                        }else{
                            // println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                            return Err(ParserError::syntax_error(next_pos.clone()));
                        }
                    } else {
  
                        let stmt = self.parse_statement(iter, defs, labels)?;

                        self.parse_expected_token(iter, Token::While)?;        // skip 'while'
                        self.parse_expected_token(iter, Token::ParenLeft)?;    // skip '('

                        let cond = self.parse_expression(iter, defs, labels)?;

                        self.parse_expected_token(iter, Token::ParenRight)?;    // skip ')'
                        self.parse_expected_token(iter, Token::SemiColon)?;     // skip ';'

                        Ok(Some(AST::Loop {
                            init_expr: None,
                            pre_condition: None,
                            body: if let Some(stmt) = stmt {
                                Some(Box::new(stmt))
                            }else{
                                None
                            },
                            update_expr: None,
                            post_condition: if let Some(expr) = cond {
                                Some(Box::new(expr))
                            }else{
                                None
                            },
                            pos: pos.clone(),
                        }))
                    }
                },
                Token::For => {
                    self.parse_for(iter, defs, &pos, labels)
                },

                // jump statement
                Token::Goto => {
                    iter.next();  // skip 'goto'

                    let (id, pos2) = iter.next().unwrap();
                    if id.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

                    if let Token::Symbol(name) = id {
                        Ok(Some(AST::Goto(name.to_string(), pos.clone())))
                    }else{
                        Err(ParserError::no_id_for_goto_statement(pos2.clone()))
                    }
                },
                Token::Continue => {
                    iter.next();  // skip 'continue'
                    self.parse_expected_token(iter, Token::SemiColon)?;  // skip ';'

                    Ok(Some(AST::Continue(pos.clone())))
                },
                Token::Break => {
                    iter.next();  // skip 'break'
                    self.parse_expected_token(iter, Token::SemiColon)?;  // skip ';'

                    Ok(Some(AST::Break(pos.clone())))
                },
                Token::Return => {
                    iter.next();  // skip 'return'
                    // let (t, _pos2) = iter.peek().ok_or(ParserError::illegal_end_of_input(pos.clone()))?;
                    let (t, pos2) = iter.peek().unwrap();
                    if t.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

                    if *t == Token::SemiColon {
                        iter.next(); // skip ';'
                        Ok(Some(AST::Return(None, pos.clone())))
                    }else{
                        if let Some(ast) = self.parse_expression(iter, defs, labels)? {
                            self.parse_expected_token(iter, Token::SemiColon)?;
                            Ok(Some(AST::Return(Some(Box::new(ast)), pos.clone())))
                        }else{
                            Err(ParserError::illegal_end_of_input(pos.clone()))
                        }
                    }
                },
                // labeled-statement
                Token::Case => {
                    self.parse_case_labeled_statement(iter, defs, labels, pos)
                },
                Token::Default => {
                    self.parse_default_labeled_statement(iter, defs, labels)
                },
                // labeled-statement or expression-statement
                Token::Symbol(id) => {
                    if defs.exists_var(id) {
                        self.parse_expression_statement(iter, defs, labels)
                    }else if defs.exists_type(id) {
                        let decl = self.parse_declaration(iter, defs, labels, pos)?;
                        Ok(decl)
                    }else{
                        iter.next();
                        self.parse_labeled_statement(id, iter, defs, labels, pos)
                    }
                },
                Token::Auto | Token::Register | Token::Static | Token::Extern | Token::Typedef |
                Token::Void | Token::Char | Token::Short | Token::Int | Token::Long | Token::Float |
                Token::Double | Token::Signed | Token::Unsigned | Token::Struct | Token::Enum | Token::TupleTypeStart =>
                {
                    self.parse_declaration(iter, defs, labels, pos)
                },
                _ => {
                    self.parse_expression_statement(iter, defs, labels)
                },
            }

        }else{
            Ok(None)
        }
    }

    fn parse_for(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, pos: &Position, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        iter.next();  // skip 'for'
        self.parse_expected_token(iter, Token::ParenLeft)?; // '('
        defs.new_local();

        let (tok2, pos2) = iter.peek().unwrap();
        if tok2.is_eof() { return Err(ParserError::illegal_end_of_input(pos2.clone())); }

        let init_expr = if *tok2 == Token::SemiColon {
            None
        }else{
            self.parse_for_init(iter, defs, labels)?
        };

        self.parse_expected_token(iter, Token::SemiColon)?; // ';'

        let (tok3, pos3) = iter.peek().unwrap();
        if tok3.is_eof() { return Err(ParserError::illegal_end_of_input(pos3.clone())); }

        let cond = if *tok3 == Token::SemiColon {
            None
        }else{
            self.parse_expression(iter, defs, labels)?
        };

        self.parse_expected_token(iter, Token::SemiColon)?; // ';'

        let (tok4, pos4) = iter.peek().unwrap();
        if tok4.is_eof() { return Err(ParserError::illegal_end_of_input(pos4.clone())); }

        let step = if *tok4 == Token::ParenRight {
            None
        }else{
            self.parse_expression(iter, defs, labels)?
        };

        self.parse_expected_token(iter, Token::ParenRight)?; // ')'

        let body = self.parse_statement(iter, defs, labels)?;

        defs.remove_local();
        Ok(Some(AST::Loop {
            init_expr: if let Some(expr) = init_expr {
                Some(Box::new(expr))
            }else{
                None
            },
            pre_condition: if let Some(expr) = cond {
                Some(Box::new(expr))
            }else{
                None
            },
            body: if let Some(expr) = body {
                Some(Box::new(expr))
            }else{
                None
            },
            update_expr: if let Some(expr) = step {
                Some(Box::new(expr))
            }else{
                None
            },
            post_condition: None,
            pos: pos.clone(),
        }))
    }

    pub fn parse_compound_statement(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Block, ParserError> {
        self.parse_expected_token(iter, Token::BraceLeft)?;
        defs.new_local();

        let mut body = Vec::new();
        loop {
            let (tok, pos) = iter.peek().unwrap();
            if tok.is_eof() { return Err(ParserError::illegal_end_of_input(pos.clone())); }

            if *tok == Token::BraceRight {
                break;
            }
            if *tok == Token::SemiColon {
                iter.next();  // skip ';'
                continue;
            }

            if let Some(ast) = self.parse_statement(iter, defs, labels)? {
                body.push(ast);
            }else{
                // do nothing
            }
        }

        defs.remove_local();

        self.parse_expected_token(iter, Token::BraceRight)?;

        let block = Block::new_with_block(body);
        Ok(block)
    }


    fn parse_for_init(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        if let Some(mut ast) = self.parse_simple_declaration_or_expression(iter, defs, labels)? {
            if let Some((tok, _pos)) = iter.peek() {
                match tok {
                    Token::Comma => {
                        if let Some(code) = self.parse_for_init_sub(iter, ast.clone(), defs, labels)? {
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

    fn parse_for_init_sub<'a>(&'a self, iter: &mut Peekable<Iter<(Token, Position)>>, ast: ExprAST, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<ExprAST>, ParserError> {
        let mut result = None;

        loop {
            if let Some((tok, pos)) = iter.peek() {
                match tok {
                    Token::Comma => {
                        iter.next(); // skip ','

                        if let Some(right) = self.parse_for_init(iter, defs, labels)? {
                            if let Some(left) = result {
                                result = Some(ExprAST::BinExpr(BinOp::Comma, Box::new(left), Box::new(right), ast.get_position().clone()));
                            }else{
                                result = Some(ExprAST::BinExpr(BinOp::Comma, Box::new(ast.clone()), Box::new(right), ast.get_position().clone()));
                            }
                        }else{
                            // println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
                            return Err(ParserError::syntax_error(pos.clone()));
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


    fn parse_labeled_statement(&self, id: &str, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>, pos: &Position) -> Result<Option<AST>, ParserError> {
        self.parse_expected_token(iter, Token::Colon).or(Err(ParserError::undefined_symbol(id, pos.clone())))?;
        let stmt = self.parse_statement(iter, defs, labels)?;

        if let Some(v) = labels {
            v.push(id.to_string());
        }else{
            return Err(ParserError::labeled_statement_without_function(iter.peek().unwrap().1.clone()));
        }

        if let Some(s) = stmt {
            Ok(Some(AST::Labeled(id.to_string(), Some(Box::new(s)), pos.clone())))
        }else{
            Ok(Some(AST::Labeled(id.to_string(), None, pos.clone())))
        }
    }

    fn parse_case_labeled_statement(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>, pos: &Position) -> Result<Option<AST>, ParserError> {
        self.parse_expected_token(iter, Token::Case)?;
        let constant_condition = if let Some(cond) = self.parse_constant_expression(iter, defs, labels)? {
            cond
        }else{
            return Err(ParserError::no_constant_expr_after_case(iter.peek().unwrap().1.clone()));
        };

        self.parse_expected_token(iter, Token::Colon)?;

        let stmt = if let Some(s) = self.parse_statement(iter, defs, labels)? {
            s
        }else{
            // println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
            return Err(ParserError::syntax_error(iter.peek().unwrap().1.clone()));
        };
        let case = Case::new(constant_condition, Box::new(stmt), pos.clone());
        Ok(Some(AST::Case(case, pos.clone())))
    }

    fn parse_default_labeled_statement(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        let pos = self.parse_expected_token(iter, Token::Default)?;
        self.parse_expected_token(iter, Token::Colon)?;

        let stmt = if let Some(s) = self.parse_statement(iter, defs, labels)? {
            s
        }else{
            // println!("Syntax Error. {}:{}:{}", file!(), line!(), column!());
            return Err(ParserError::syntax_error(iter.peek().unwrap().1.clone()))?;
        };
        Ok(Some(AST::Default(Box::new(stmt), pos.clone())))
    }

    fn parse_expression_statement(&self, iter: &mut Peekable<Iter<(Token, Position)>>, defs: &mut Defines, labels: &mut Option<&mut Vec<String>>) -> Result<Option<AST>, ParserError> {
        if let Some((tok, pos)) = iter.peek() {
            match tok {
                Token::SemiColon => {
                    iter.next();
                    Ok(None)
                },
                _ => {
                    let expr = self.parse_expression(iter, defs, labels)?;
                    self.parse_expected_token(iter, Token::SemiColon)?;
                    if let Some(e) = expr {
                        Ok(Some(AST::Expr(Box::new(e), pos.clone())))
                    }else{
                        Ok(None)
                    }
                }
            }

        }else{
            Ok(None)
        }
    }

}
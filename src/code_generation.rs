#![allow(dead_code)]
use crate::semantically_analyze::*;
use crate::token::*;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::{cell::RefCell, rc::Rc, rc::Weak};

// value not useful but used for self documentation
const TEMP: u8 = 0x00;
// const are 1 byte and mem are 2 bytes
const LOAD_ACCUM_CONST: u8 = 0xA9;
const LOAD_ACCUM_MEM: u8 = 0xAD;
const STORE_ACCUM_MEM: u8 = 0x8D;
const ADD_CARRY: u8 = 0x6D;
const LOAD_X_CONST: u8 = 0xA2;
const LOAD_X_MEMORY: u8 = 0xAE;
const LOAD_Y_CONST: u8 = 0xA0;
const LOAD_Y_MEM: u8 = 0xAC;
// const BREAK: u8 = 0x00;
const COMPARE_MEM_X_Z: u8 = 0xEC;
const BRANCH_Z_0: u8 = 0xD0;
const PRINT_Y_OR_MEM: u8 = 0xFF;

const ASSEMBLY_SIZE: usize = 256;

#[derive(Clone, Copy)]
enum Byte<'a> {
    Code(u8),
    Reference(&'a u8),
}

pub struct OpCodes<'a, T>
where
    T: Iterator<Item = &'a Token>,
{
    // the codes are lazy in that they won't all be correct
    //    until the end of the program when they are realized
    lazy_codes: [Byte<'a>; ASSEMBLY_SIZE],
    variable_name_scope_to_address_type: HashMap<(char, Vec<u8>), (u8, DataType)>,
    // can be used like a stack to fill in the last jump
    unrealized_jumps_index: Vec<usize>,
    last_code_index: usize,
    last_heap_index: usize,
    marker: PhantomData<T>,
}

impl<'a, T> OpCodes<'a, T>
where
    T: Iterator<Item = &'a Token>,
{
    fn new() -> OpCodes<'a, T> {
        let op_codes = OpCodes {
            lazy_codes: [Byte::Code(0); ASSEMBLY_SIZE],
            variable_name_scope_to_address_type: HashMap::new(),
            unrealized_jumps_index: vec![],
            last_code_index: 0,
            last_heap_index: ASSEMBLY_SIZE - 1,
            marker: PhantomData,
        };
        return op_codes;
    }

    fn generate_block(&self, abstract_node: AbstractNodeEnum<'a>, current_scope: Scope<'a>) {}

    fn do_var_decl(
        &mut self,
        decl_production_strong: Rc<RefCell<AbstractProduction<'a>>>,
        current_scope_strong: Rc<RefCell<Scope<'a>>>,
    ) {
        let decl_production = decl_production_strong.borrow();
        let mut type_var_nodes = decl_production.children.iter();
        let _ignored_first_child_type = type_var_nodes.next();
        let variable_name = type_var_nodes.next().unwrap();
        let name = match variable_name {
            AbstractNodeEnum::Terminal(t) => match &t.kind {
                TokenKind::Id(id) => id.name,
                _ => panic!("Expected token"),
            },
            AbstractNodeEnum::AbstractProduction(_) => panic!("Expected terminal"),
        };
        let current_scope = current_scope_strong.borrow();
        let flat_scope = current_scope.flat_scopes.clone();
        let variable = current_scope
            .variables
            .get(&name)
            .expect("variable should've exists")
            .clone();
        self.variable_name_scope_to_address_type
            .insert((name, flat_scope), (TEMP, variable.data_type));
    }

    fn do_assignment(
        &mut self,
        assignment_production_strong: Rc<RefCell<AbstractProduction<'a>>>,
        current_scope_strong: Rc<RefCell<Scope<'a>>>,
    ) {
        let decl_production = assignment_production_strong.borrow();
        let mut type_var_nodes = decl_production.children.iter();
        let variable_name = type_var_nodes.next().unwrap();
        let name = match variable_name {
            AbstractNodeEnum::Terminal(t) => match &t.kind {
                TokenKind::Id(id) => {
                    id.name;
                }
                _ => panic!("Expected token"),
            },
            AbstractNodeEnum::AbstractProduction(_) => panic!("Expected terminal"),
        };
        let current_scope = current_scope_strong.borrow();

        let flat_scope = current_scope.flat_scopes.clone();
        let right_hand = type_var_nodes.next().unwrap();
        match right_hand {
            AbstractNodeEnum::AbstractProduction(abstract_production_strong) => {
                let abstract_production = abstract_production_strong.borrow();
                match &abstract_production.abstract_type {
                    AbstractProductionType::StringExpr(string) => match string.kind {
                        TokenKind::StringLiteral => {}
                        _ => panic!("expected string literal"),
                    },
                    AbstractProductionType::Add => todo!(),
                    AbstractProductionType::Boolop(_) => todo!(),
                    _ => panic!("unexpected production"),
                }
            }
            AbstractNodeEnum::Terminal(t) => match &t.kind {
                TokenKind::Id(id) => {
                    // NEED TO CHECK FOR THE FIRST FLAT SCOPE THAT WORKS
                    // general scopes start at [0] so len 1
                    let mut running_flat_scope = flat_scope.clone();
                    let mut address_type = None;
                    loop {
                        address_type = self
                            .variable_name_scope_to_address_type
                            .get(&(id.name.clone(), running_flat_scope.clone()));
                        if address_type != None {
                            break;
                        }
                        let length = running_flat_scope.len();
                        if length == 0 {
                            panic!("variable not found!!")
                        }
                        running_flat_scope = running_flat_scope[..running_flat_scope.len() - 1].to_vec();
                    }
                    let address = address_type.unwrap().0;
                }
                TokenKind::Symbol(_) => todo!(),
                TokenKind::Digit(_) => todo!(),
                TokenKind::Char(_) => todo!(),
                TokenKind::StringLiteral => todo!(),
                TokenKind::Keyword(_) => todo!(),
            },
        }
    }

    fn do_expression_to_memory(&mut self) {}

    fn create_string(&mut self) {}
}

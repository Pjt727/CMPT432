#![allow(dead_code)]
use crate::semantically_analyze::*;
use crate::token::*;
use std::collections::HashMap;
use std::{cell::RefCell, rc::Rc, rc::Weak};

// const are 1 byte and mem are 2 bytes
const LOAD_ACCUM_CONST: u8 = 0xA9;
const LOAD_ACCUM_MEM: u8 = 0xAD;
const STORE_ACCUM_MEM: u8 = 0x8D;
const ADD_CARRY: u8 = 0x6D;
const LOAD_X_CONST: u8 = 0xA2;
const LOAD_X_MEMORY: u8 = 0xAE;
const LOAD_Y_CONST: u8 = 0xA0;
const LOAD_Y_MEM: u8 = 0xAC;
const BREAK: u8 = 0x00;
const COMPARE_MEM_X_TO_Z: u8 = 0xEC;
const BRANCH_Z_0: u8 = 0xD0;
const PRINT_Y_MEM: u8 = 0xFF;

const ASSEMBLY_SIZE: usize = 256;
// CHANGE FOR STRINGS
const RESERVED_MEM1: u8 = 255;

#[derive(Clone, Copy)]
enum Byte {
    Code(u8),
    AddressIndex(usize),
    JumpForward,
}

pub struct OpCodes<'a> {
    // the codes are lazy in that they won't all be correct
    //    until the end of the program when they are realized
    lazy_codes: [Byte; ASSEMBLY_SIZE],
    codes: [u8; ASSEMBLY_SIZE],
    strings_to_address: HashMap<String, u8>,
    unrealized_addresses: Vec<(Variable<'a>, Vec<u8>)>,
    // can be used like a stack to fill in the last jump
    unrealized_jumps_index: Vec<usize>,
    next_code_index: usize,
    end_heap: usize,
}

impl<'a> OpCodes<'a> {
    fn new(
        root_production: Rc<RefCell<AbstractProduction<'a>>>,
        root_scope: Rc<RefCell<Scope<'a>>>,
    ) -> OpCodes<'a> {
        let mut op_codes = OpCodes {
            lazy_codes: [Byte::Code(0); ASSEMBLY_SIZE],
            codes: [0; ASSEMBLY_SIZE],
            unrealized_addresses: vec![],
            strings_to_address: HashMap::new(),
            unrealized_jumps_index: vec![],
            next_code_index: 0,
            // -1 because I use the last address booleans
            end_heap: ASSEMBLY_SIZE - 1,
        };
        // memory space to keep a temp value for in const addition
        //    and boolean ops
        op_codes.generate_block(root_production, root_scope);
        op_codes.generate_codes();
        return op_codes;
    }

    pub fn print_op_codes(&self) {
        for (i, code) in self.codes.iter().enumerate() {
            print!("{:02X}  ", code);
            if (i + 1) % 16 == 0 {
                println!();
            }
        }
    }

    fn generate_codes(&mut self) {
        let mut stack_addresses = vec![];
        self.add_to_code(Byte::Code(BREAK));
        for _address in &self.unrealized_addresses {
            stack_addresses.push(self.next_code_index as u8);
            self.next_code_index += 1;
        }
        for (i, byte) in self.lazy_codes.iter().enumerate() {
            match byte {
                Byte::Code(val) => self.codes[i] = *val,
                Byte::AddressIndex(index) => {
                    self.codes[i] = stack_addresses[*index];
                }
                Byte::JumpForward => panic!("jumps should all be resolved"),
            }
        }
    }

    fn generate_block(
        &mut self,
        abstract_node_strong: Rc<RefCell<AbstractProduction<'a>>>,
        current_scope_strong: Rc<RefCell<Scope<'a>>>,
    ) {
        let abstract_node = abstract_node_strong.borrow();
        let current_scope = current_scope_strong.borrow();
        let mut children_scopes = current_scope.children.iter();
        for abstract_node_enum in &abstract_node.children {
            match abstract_node_enum {
                AbstractNodeEnum::AbstractProduction(abstract_production_strong) => {
                    let abstract_production = abstract_production_strong.borrow();
                    match abstract_production.abstract_type {
                        AbstractProductionType::Block => {
                            self.generate_block(
                                abstract_production_strong.clone(),
                                children_scopes.next().unwrap().clone(),
                            );
                        }
                        AbstractProductionType::PrintStatement => self.do_print(
                            abstract_production_strong.clone(),
                            current_scope_strong.clone(),
                        ),
                        AbstractProductionType::AssignmentStatement => self.do_assignment(
                            abstract_production_strong.clone(),
                            current_scope_strong.clone(),
                        ),
                        AbstractProductionType::VarDecl => self.do_var_decl(
                            abstract_production_strong.clone(),
                            current_scope_strong.clone(),
                        ),
                        AbstractProductionType::WhileStatement => todo!(),
                        AbstractProductionType::IfStatement => self.do_if_statement(
                            abstract_production_strong.clone(),
                            current_scope_strong.clone(),
                            children_scopes.next().unwrap().clone(),
                        ),
                        AbstractProductionType::Add => panic!("main block should not get add"),
                        AbstractProductionType::StringExpr(_) => {
                            panic!("main block should not get string")
                        }
                        AbstractProductionType::Boolop(_) => {
                            panic!("main block should not get boolop")
                        }
                    }
                }
                AbstractNodeEnum::Terminal(_) => panic!("main block should not get token"),
            }
        }
    }

    fn do_if_statement(
        &mut self,
        if_production_strong: Rc<RefCell<AbstractProduction<'a>>>,
        current_scope_strong: Rc<RefCell<Scope<'a>>>,
        if_scope_strong: Rc<RefCell<Scope<'a>>>,
    ) {
        let if_production = if_production_strong.borrow();
        let current_scope = current_scope_strong.borrow();

        let children = &if_production.children;
        let first_child = &children[0];
        let block_child = &children[1];

        match first_child {
            AbstractNodeEnum::AbstractProduction(strong_abstract_production) => {
                let abstract_production = strong_abstract_production.borrow();
                match abstract_production.abstract_type {
                    AbstractProductionType::Boolop(_) => {
                        // could do some optimizations here bc z flag is
                        //    always copied to mem for simplicity
                        //    even if that's the z flag we want
                        let byte = self.do_boolean_to_memory(
                            strong_abstract_production.clone(),
                            current_scope_strong.clone(),
                        );
                        self.add_to_code(Byte::Code(LOAD_X_CONST));
                        self.add_to_code(Byte::Code(1));
                        self.add_to_code(Byte::Code(COMPARE_MEM_X_TO_Z));
                        self.add_to_code(byte);
                        self.add_to_code(Byte::Code(0));
                        self.add_to_code(Byte::Code(BRANCH_Z_0));
                        self.add_to_code(Byte::JumpForward);
                    }
                    _ => panic!("expected bool op"),
                }
            }
            AbstractNodeEnum::Terminal(t) => {
                match &t.kind {
                    TokenKind::Keyword(k) => match k {
                        Keyword::True => todo!(),
                        Keyword::False => todo!(),
                        _ => panic!("expected true/false"),
                    },
                    TokenKind::Id(id) => {
                        // load true to x register to set up compare
                        self.add_to_code(Byte::Code(LOAD_X_CONST));
                        self.add_to_code(Byte::Code(1));
                        self.add_to_code(Byte::Code(COMPARE_MEM_X_TO_Z));
                        self.add_variable_reference(id.name, &current_scope.flat_scopes);
                        self.add_to_code(Byte::Code(BRANCH_Z_0));
                        self.add_to_code(Byte::JumpForward);
                    }
                    _ => panic!("expected boolean"),
                }
            }
        }

        match block_child {
            AbstractNodeEnum::AbstractProduction(strong_abstract_production) => {
                let abstract_production = strong_abstract_production.borrow();
                match abstract_production.abstract_type {
                    AbstractProductionType::Block => {
                        self.generate_block(
                            strong_abstract_production.clone(),
                            if_scope_strong.clone(),
                        );
                    }
                    _ => panic!("expected block"),
                }
            }
            AbstractNodeEnum::Terminal(_) => panic!("expected block"),
        }
        // does the if scope
        self.resolve_jump_here();
    }

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
        let variable = current_scope
            .variables
            .get(&name)
            .expect("variable should've exists")
            .clone();

        self.unrealized_addresses
            .push((variable, current_scope.flat_scopes.clone()))
    }

    fn do_assignment(
        &mut self,
        assignment_production_strong: Rc<RefCell<AbstractProduction<'a>>>,
        current_scope_strong: Rc<RefCell<Scope<'a>>>,
    ) {
        let decl_production = assignment_production_strong.borrow();
        let mut type_var_nodes = decl_production.children.iter();
        let variable_name = type_var_nodes.next().unwrap();
        let left_hand_id = match variable_name {
            AbstractNodeEnum::Terminal(t) => match &t.kind {
                TokenKind::Id(id) => id.name,
                _ => panic!("Expected token"),
            },
            AbstractNodeEnum::AbstractProduction(_) => panic!("Expected terminal"),
        };
        let current_scope = current_scope_strong.borrow();

        let right_hand = type_var_nodes.next().unwrap();
        match right_hand {
            AbstractNodeEnum::AbstractProduction(abstract_production_strong) => {
                let abstract_production = abstract_production_strong.borrow();
                match &abstract_production.abstract_type {
                    AbstractProductionType::StringExpr(string) => match string.kind {
                        TokenKind::StringLiteral => {
                            let address = self.add_to_heap(&string.representation);
                            self.add_to_code(Byte::Code(LOAD_ACCUM_CONST));
                            self.add_to_code(Byte::Code(address));
                            self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                            self.add_variable_reference(left_hand_id, &current_scope.flat_scopes);
                        }
                        _ => panic!("expected string literal"),
                    },
                    AbstractProductionType::Add => {
                        self.do_add_to_agg(
                            abstract_production_strong.clone(),
                            current_scope_strong.clone(),
                            true,
                        );
                        self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                        self.add_variable_reference(left_hand_id, &current_scope.flat_scopes);
                    }
                    AbstractProductionType::Boolop(_) => {
                        // do the operation and use the memory address to store
                        //    that value into the variable
                        let byte = self.do_boolean_to_memory(
                            abstract_production_strong.clone(),
                            current_scope_strong.clone(),
                        );
                        self.add_to_code(Byte::Code(LOAD_ACCUM_MEM));
                        self.add_to_code(byte);
                        self.add_to_code(Byte::Code(0));
                        self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                        self.add_variable_reference(left_hand_id, &current_scope.flat_scopes);
                    }
                    _ => panic!("unexpected production"),
                }
            }
            AbstractNodeEnum::Terminal(t) => match &t.kind {
                TokenKind::Id(right_hand_id) => {
                    // assign var | ex:   a = b
                    self.add_to_code(Byte::Code(LOAD_ACCUM_MEM));
                    self.add_variable_reference(right_hand_id.name, &current_scope.flat_scopes);
                    self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                    self.add_variable_reference(left_hand_id, &current_scope.flat_scopes);
                }
                TokenKind::Digit(digit) => {
                    // assign const | a = 1
                    self.add_to_code(Byte::Code(LOAD_ACCUM_CONST));
                    self.add_to_code(Byte::Code(digit.value));
                    self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                    self.add_variable_reference(left_hand_id, &current_scope.flat_scopes);
                }
                TokenKind::Keyword(k) => match k {
                    Keyword::True => {
                        self.add_to_code(Byte::Code(LOAD_ACCUM_CONST));
                        self.add_to_code(Byte::Code(1));
                        self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                        self.add_variable_reference(left_hand_id, &current_scope.flat_scopes);
                    }
                    Keyword::False => {
                        self.add_to_code(Byte::Code(LOAD_ACCUM_CONST));
                        self.add_to_code(Byte::Code(0));
                        self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                        self.add_variable_reference(left_hand_id, &current_scope.flat_scopes);
                    }
                    _ => panic!("unexpected token"),
                },
                _ => panic!("unexpected token"),
            },
        }
    }

    fn do_print(
        &mut self,
        print_production_strong: Rc<RefCell<AbstractProduction<'a>>>,
        current_scope_strong: Rc<RefCell<Scope<'a>>>,
    ) {
        let current_scope = current_scope_strong.borrow();
        // there always only one child to print
        let print_production = print_production_strong.borrow();
        let child_node = print_production.children.iter().next().unwrap();
        match child_node {
            AbstractNodeEnum::AbstractProduction(abstract_production_strong) => {
                let abstract_production = abstract_production_strong.borrow();
                match &abstract_production.abstract_type {
                    AbstractProductionType::StringExpr(string) => match string.kind {
                        TokenKind::StringLiteral => {
                            // print literal string
                            let address = self.add_to_heap(&string.representation);
                            self.add_to_code(Byte::Code(LOAD_Y_CONST));
                            self.add_to_code(Byte::Code(address));
                            self.add_to_code(Byte::Code(LOAD_X_CONST));
                            self.add_to_code(Byte::Code(2));
                            self.add_to_code(Byte::Code(PRINT_Y_MEM));
                        }
                        _ => panic!("expected string literal"),
                    },
                    AbstractProductionType::Add => {
                        self.do_add_to_agg(
                            abstract_production_strong.clone(),
                            current_scope_strong.clone(),
                            true,
                        );
                        self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                        self.add_to_code(Byte::Code(RESERVED_MEM1));
                        self.add_to_code(Byte::Code(0));
                        self.add_to_code(Byte::Code(LOAD_Y_MEM));
                        self.add_to_code(Byte::Code(RESERVED_MEM1));
                        self.add_to_code(Byte::Code(0));
                        self.add_to_code(Byte::Code(LOAD_X_CONST));
                        self.add_to_code(Byte::Code(1));
                        self.add_to_code(Byte::Code(PRINT_Y_MEM));
                    }
                    AbstractProductionType::Boolop(_) => {
                        let byte = self.do_boolean_to_memory(
                            abstract_production_strong.clone(),
                            current_scope_strong.clone(),
                        );
                        self.add_to_code(Byte::Code(LOAD_Y_MEM));
                        self.add_to_code(byte);
                        self.add_to_code(Byte::Code(0));
                        self.add_to_code(Byte::Code(LOAD_X_CONST));
                        self.add_to_code(Byte::Code(1));
                        self.add_to_code(Byte::Code(PRINT_Y_MEM));
                    }
                    _ => panic!("unexpected production"),
                }
            }
            AbstractNodeEnum::Terminal(t) => match &t.kind {
                TokenKind::Id(id) => {
                    // print variable
                    let var_index = self.get_unrealized_index(id.name, &current_scope.flat_scopes);
                    // I don't really understand why I need to clone here but it prvents
                    //     immutable and mutable burrows of self with 'a or something
                    let (variable, _scope) = self.unrealized_addresses[var_index].clone();
                    self.add_to_code(Byte::Code(LOAD_Y_MEM));
                    self.add_variable_reference(id.name, &current_scope.flat_scopes);
                    self.add_to_code(Byte::Code(LOAD_X_CONST));
                    match &variable.data_type {
                        DataType::String => self.add_to_code(Byte::Code(2)),
                        _ => self.add_to_code(Byte::Code(1)),
                    }
                    self.add_to_code(Byte::Code(PRINT_Y_MEM));
                }
                TokenKind::Digit(digit) => {
                    // print const | print(1)
                    self.add_to_code(Byte::Code(LOAD_Y_CONST));
                    self.add_to_code(Byte::Code(digit.value));
                    self.add_to_code(Byte::Code(LOAD_X_CONST));
                    self.add_to_code(Byte::Code(1));
                    self.add_to_code(Byte::Code(PRINT_Y_MEM));
                }
                TokenKind::Keyword(k) => match k {
                    Keyword::True => {
                        self.add_to_code(Byte::Code(LOAD_Y_CONST));
                        self.add_to_code(Byte::Code(1));
                        self.add_to_code(Byte::Code(LOAD_X_CONST));
                        self.add_to_code(Byte::Code(1));
                        self.add_to_code(Byte::Code(PRINT_Y_MEM));
                    }
                    Keyword::False => {
                        self.add_to_code(Byte::Code(LOAD_Y_CONST));
                        self.add_to_code(Byte::Code(0));
                        self.add_to_code(Byte::Code(LOAD_X_CONST));
                        self.add_to_code(Byte::Code(1));
                        self.add_to_code(Byte::Code(PRINT_Y_MEM));
                    }
                    _ => panic!("unexpected token"),
                },
                _ => panic!("unexpected token"),
            },
        }
    }

    fn do_add_to_agg(
        &mut self,
        add_production_strong: Rc<RefCell<AbstractProduction<'a>>>,
        current_scope_strong: Rc<RefCell<Scope<'a>>>,
        is_first_call: bool,
    ) {
        let add_production = add_production_strong.borrow();
        let current_scope = current_scope_strong.borrow();

        let children = &add_production.children;
        let first_child = &children[0];
        let second_child = &children[1];

        match &first_child {
            AbstractNodeEnum::AbstractProduction(_) => todo!(),
            AbstractNodeEnum::Terminal(t) => match &t.kind {
                // first + is always a digit as id's cant appear there
                TokenKind::Digit(digit) => {
                    if is_first_call {
                        self.add_to_code(Byte::Code(LOAD_ACCUM_CONST));
                        self.add_to_code(Byte::Code(digit.value));
                    } else {
                        self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                        self.add_to_code(Byte::Code(RESERVED_MEM1));
                        self.add_to_code(Byte::Code(0));
                        self.add_to_code(Byte::Code(LOAD_ACCUM_CONST));
                        self.add_to_code(Byte::Code(digit.value));
                        self.add_to_code(Byte::Code(ADD_CARRY));
                        self.add_to_code(Byte::Code(RESERVED_MEM1));
                        self.add_to_code(Byte::Code(0));
                    }
                }
                _ => panic!("expected digit"),
            },
        }

        match &second_child {
            AbstractNodeEnum::AbstractProduction(abstract_production_strong) => {
                let abstract_production = abstract_production_strong.borrow();
                match abstract_production.abstract_type {
                    AbstractProductionType::Add => self.do_add_to_agg(
                        abstract_production_strong.clone(),
                        current_scope_strong.clone(),
                        false,
                    ),
                    _ => panic!("expected add"),
                }
            }
            AbstractNodeEnum::Terminal(t) => match &t.kind {
                TokenKind::Id(id) => {
                    self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                    self.add_to_code(Byte::Code(RESERVED_MEM1));
                    self.add_to_code(Byte::Code(0));
                    self.add_to_code(Byte::Code(LOAD_ACCUM_MEM));
                    self.add_variable_reference(id.name, &current_scope.flat_scopes);
                    self.add_to_code(Byte::Code(ADD_CARRY));
                    self.add_to_code(Byte::Code(RESERVED_MEM1));
                    self.add_to_code(Byte::Code(0));
                }
                TokenKind::Digit(digit) => {
                    self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                    self.add_to_code(Byte::Code(RESERVED_MEM1));
                    self.add_to_code(Byte::Code(0));
                    self.add_to_code(Byte::Code(LOAD_ACCUM_CONST));
                    self.add_to_code(Byte::Code(digit.value));
                    self.add_to_code(Byte::Code(ADD_CARRY));
                    self.add_to_code(Byte::Code(RESERVED_MEM1));
                    self.add_to_code(Byte::Code(0));
                }
                _ => panic!("expected number"),
            },
        }
    }

    // return the memory address which has the result
    fn do_boolean_to_memory(
        &mut self,
        bool_op_production_strong: Rc<RefCell<AbstractProduction<'a>>>,
        current_scope_strong: Rc<RefCell<Scope<'a>>>,
    ) -> Byte {
        let operation_production = bool_op_production_strong.borrow();
        let current_scope = current_scope_strong.borrow();

        // should only be two children
        let children = &operation_production.children;
        let first_child = &children[0];
        let second_child = &children[1];

        match &operation_production.abstract_type {
            AbstractProductionType::Add => todo!(),
            AbstractProductionType::Boolop(t) => match &t.kind {
                TokenKind::Symbol(s) => {
                    let byte = self.get_byte_of_node(first_child, &current_scope.flat_scopes);
                    self.get_node_to_x_register(second_child, &current_scope.flat_scopes);
                    self.add_to_code(Byte::Code(COMPARE_MEM_X_TO_Z));
                    self.add_to_code(byte);
                    self.add_to_code(Byte::Code(0));
                    match s {
                        Symbol::CheckEquality => {
                            // store false to memory address
                            self.add_to_code(Byte::Code(LOAD_ACCUM_CONST));
                            self.add_to_code(Byte::Code(0));
                            self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                            // TODO: change this to a pool of values for nested boolean
                            self.add_to_code(Byte::Code(RESERVED_MEM1));
                            self.add_to_code(Byte::Code(0));
                            // store true if I dont branch back over it
                            self.add_to_code(Byte::Code(BRANCH_Z_0));
                            self.add_to_code(Byte::JumpForward);

                            self.add_to_code(Byte::Code(LOAD_ACCUM_CONST));
                            self.add_to_code(Byte::Code(1));
                            self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                            // TODO: change this to a pool of values for nested boolean
                            self.add_to_code(Byte::Code(RESERVED_MEM1));
                            self.add_to_code(Byte::Code(0));
                            self.resolve_jump_here();
                        }
                        Symbol::CheckInequality => {
                            // store true to memory address
                            self.add_to_code(Byte::Code(LOAD_ACCUM_CONST));
                            self.add_to_code(Byte::Code(1));
                            self.add_to_code(Byte::Code(STORE_ACCUM_MEM));

                            // TODO: change this to a pool of values for nested boolean
                            self.add_to_code(Byte::Code(RESERVED_MEM1));
                            self.add_to_code(Byte::Code(0));
                            // store false if I dont branch back over it
                            self.add_to_code(Byte::Code(BRANCH_Z_0));
                            self.add_to_code(Byte::JumpForward);

                            self.add_to_code(Byte::Code(LOAD_ACCUM_CONST));
                            self.add_to_code(Byte::Code(0));
                            self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                            // TODO: change this to a pool of values for nested boolean
                            self.add_to_code(Byte::Code(RESERVED_MEM1));
                            self.add_to_code(Byte::Code(0));
                            self.resolve_jump_here();
                        }
                        _ => panic!("expected bool op component"),
                    }
                }
                _ => panic!("execpeted bool op component"),
            },
            _ => panic!("expected operation"),
        }
        return Byte::Code(RESERVED_MEM1);
    }

    // returns the memory reference where the value is
    fn get_byte_of_node(&mut self, child: &AbstractNodeEnum<'a>, flat_scope: &Vec<u8>) -> Byte {
        match child {
            AbstractNodeEnum::AbstractProduction(abstract_production_strong) => {
                let abstract_production = abstract_production_strong.borrow();
                match &abstract_production.abstract_type {
                    AbstractProductionType::StringExpr(t) => {
                        self.add_to_code(Byte::Code(LOAD_ACCUM_MEM));
                        let heap_address = self.add_to_heap(&t.representation);
                        // mem addresses need be two bytes
                        self.add_to_code(Byte::Code(heap_address));
                        self.add_to_code(Byte::Code(0));

                        self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                        self.add_to_code(Byte::Code(RESERVED_MEM1));
                        self.add_to_code(Byte::Code(0));
                        return Byte::Code(RESERVED_MEM1);
                    }
                    AbstractProductionType::Boolop(_) => {
                        todo!()
                    }
                    _ => panic!(),
                }
            }
            AbstractNodeEnum::Terminal(t) => match &t.kind {
                TokenKind::Id(id) => {
                    let index = self.get_unrealized_index(id.name, flat_scope);
                    return Byte::AddressIndex(index);
                }
                TokenKind::Keyword(k) => {
                    self.add_to_code(Byte::Code(LOAD_ACCUM_CONST));
                    match k {
                        Keyword::True => self.add_to_code(Byte::Code(1)),
                        Keyword::False => self.add_to_code(Byte::Code(0)),
                        _ => panic!("expected true/ false"),
                    };
                    self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                    self.add_to_code(Byte::Code(RESERVED_MEM1));
                    self.add_to_code(Byte::Code(0));
                    return Byte::Code(RESERVED_MEM1);
                }
                TokenKind::Digit(digit) => {
                    self.add_to_code(Byte::Code(LOAD_ACCUM_CONST));
                    self.add_to_code(Byte::Code(digit.value as u8));
                    self.add_to_code(Byte::Code(STORE_ACCUM_MEM));
                    self.add_to_code(Byte::Code(RESERVED_MEM1));
                    self.add_to_code(Byte::Code(0));
                    return Byte::Code(RESERVED_MEM1);
                }
                _ => panic!("expected literal or reference"),
            },
        }
    }

    fn get_node_to_x_register(&mut self, child: &AbstractNodeEnum<'a>, flat_scope: &Vec<u8>) {
        match child {
            AbstractNodeEnum::AbstractProduction(abstract_production_strong) => {
                let abstract_production = abstract_production_strong.borrow();
                match &abstract_production.abstract_type {
                    AbstractProductionType::StringExpr(t) => {
                        self.add_to_code(Byte::Code(LOAD_X_MEMORY));
                        let heap_address = self.add_to_heap(&t.representation);
                        self.add_to_code(Byte::Code(heap_address));
                        self.add_to_code(Byte::Code(0));
                    }
                    AbstractProductionType::Boolop(_) => {
                        todo!()
                    }
                    _ => panic!(),
                }
            }
            AbstractNodeEnum::Terminal(t) => match &t.kind {
                TokenKind::Id(id) => {
                    let index = self.get_unrealized_index(id.name, flat_scope);
                    self.add_to_code(Byte::Code(LOAD_X_MEMORY));
                    self.add_to_code(Byte::AddressIndex(index));
                    self.add_to_code(Byte::Code(0));
                }
                TokenKind::Keyword(k) => {
                    self.add_to_code(Byte::Code(LOAD_X_CONST));
                    match k {
                        Keyword::True => self.add_to_code(Byte::Code(1)),
                        Keyword::False => self.add_to_code(Byte::Code(0)),
                        _ => panic!("expected true/ false"),
                    };
                }
                TokenKind::Digit(digit) => {
                    self.add_to_code(Byte::Code(LOAD_X_CONST));
                    self.add_to_code(Byte::Code(digit.value as u8));
                }
                _ => panic!("expected literal or reference"),
            },
        }
    }

    // want to make sure I never go out of sync
    fn add_to_code(&mut self, byte: Byte) {
        self.lazy_codes[self.next_code_index] = byte;
        self.next_code_index += 1;
    }

    fn add_variable_reference(&mut self, name: char, reference_flat_scope: &Vec<u8>) {
        let index = self.get_unrealized_index(name, reference_flat_scope);
        self.add_to_code(Byte::AddressIndex(index));
        self.add_to_code(Byte::Code(0));
    }

    // returns the memory address of the first byte of the string
    //   creating one if needed
    fn add_to_heap(&mut self, string: &String) -> u8 {
        if let Some(existing_address) = self.strings_to_address.get(string) {
            return existing_address.clone();
        }
        let length = string.len();
        dbg!(self.end_heap, length);
        // leave a 0
        self.end_heap -= length + 1;
        for (i, char) in string.chars().enumerate() {
            let byte = Byte::Code(char as u8);
            self.lazy_codes[i + self.end_heap] = byte;
        }
        let new_address = self.end_heap as u8;
        self.strings_to_address.insert(string.clone(), new_address);
        return new_address;
    }

    // makes the last jump go to this the next code index
    fn resolve_jump_here(&mut self) {
        let mut i = self.next_code_index - 1;
        loop {
            if matches!(self.lazy_codes[i], Byte::JumpForward) {
                self.lazy_codes[i] = Byte::Code((self.next_code_index - i) as u8 - 1);
                break;
            }
            i -= 1;
        }
    }

    fn add_jump_stack(&mut self) {}

    // using my flat scope to get the correct variable in scope
    //    I am not really convinced this is easier than how I did in in SE
    //    but the alternative would be added more things to the scope class i thinck
    fn get_unrealized_index(&self, name: char, reference_flat_scope: &Vec<u8>) -> usize {
        //  im sure there is a better way
        let mut max_index = 69;
        let mut max_shared_scopes = -420;
        for (i, (variable, init_scope)) in self.unrealized_addresses.iter().enumerate() {
            // cannot reference a variable in a child scope or that does not share a name
            if init_scope.len() > reference_flat_scope.len() || variable.get_name() != name {
                continue;
            }
            let mut running_count = 0;
            // since there is no order for init we have to check all
            let zipped = reference_flat_scope.iter().zip(init_scope.iter());
            for (ref_scope, init_scope) in zipped {
                if ref_scope == init_scope {
                    running_count += 1;
                } else {
                    running_count = -1;
                    break;
                }
            }
            if max_shared_scopes < running_count {
                max_shared_scopes = running_count;
                max_index = i;
            }
        }

        return max_index;
    }
}

#[cfg(test)]
mod generation_tests {
    use super::*;
    use crate::lex::*;
    use crate::parse::*;
    use std::path::Path;
    use std::slice::Iter;

    fn helper_get_cst<'a>(tokens: Iter<'a, Token>) -> ConcreteSyntaxTree<'a, Iter<'a, Token>> {
        let cst = ConcreteSyntaxTree::new(tokens);
        cst.show_parse_steps();
        cst.show();
        if cst.is_err() {
            panic!("Error duing parse!!");
        }
        return cst;
    }

    fn helper_get_tokens(path_str: &str) -> Vec<Token> {
        let path = Path::new(&path_str);
        let lexemes = get_lexemes(path);
        let mut tokens = vec![];
        for lexeme in lexemes {
            match lexeme {
                Ok(token) => tokens.push(token),
                Err(lex_problem) => match lex_problem {
                    LexProblem::LexError(_) => panic!("Error during lex!!"),
                    LexProblem::LexWarning(_) => continue,
                },
            }
        }
        return tokens;
    }

    fn general_helper(path_str: &str) {
        let tokens = helper_get_tokens(path_str);
        let cst = helper_get_cst(tokens.iter());
        let ast = AbstractSyntaxTree::new(cst);
        let semantic_checks = SemanticChecks::new(&ast);
        println!("Showing AST:");
        println!();
        ast.show();
        println!("Showing Scope table");
        println!();
        println!();
        semantic_checks.show();
        let op_codes: OpCodes = OpCodes::new(ast.root, semantic_checks.scope_root);
        op_codes.print_op_codes();
    }

    #[test]
    fn hello_generation() {
        general_helper("test_cases/code-gen-edge-cases/changing");
    }
}

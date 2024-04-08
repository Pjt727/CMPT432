#![allow(suspicious_double_ref_op)]
#![allow(dead_code)]
use crate::parse::*;
use crate::token::*;
use colored::Colorize;
use std::cell::Ref;
use std::collections::HashMap;
use std::fmt;
use std::marker::PhantomData;
use std::{cell::RefCell, rc::Rc, rc::Weak};

enum SemanticWarning {
    UnusedVariable,
}
enum SematincError {
    UndeclaredVariable,
}

enum AbstractProductionType {
    Block,
    PrintStatement,
    AssignmentStatement,
    VarDecl,
    WhileStatement,
    IfStatement,
    Add,                // Would need change to same way boolop is implemented for other intops
    StringExpr(Token), // sort of a mismatch because string expr will have no children but whtever
    Boolop(Token),
}

impl fmt::Display for AbstractProductionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AbstractProductionType::Block => write!(f, "Block"),
            AbstractProductionType::PrintStatement => write!(f, "Print Statement"),
            AbstractProductionType::AssignmentStatement => write!(f, "Assignment Statement"),
            AbstractProductionType::VarDecl => write!(f, "Var Declaration"),
            AbstractProductionType::WhileStatement => write!(f, "While Statement"),
            AbstractProductionType::IfStatement => write!(f, "If Statement"),
            AbstractProductionType::Add => write!(f, "Add"),
            AbstractProductionType::StringExpr(expression) => {
                write!(f, "String Expression \"{}\"", expression.representation)
            }
            AbstractProductionType::Boolop(token) => {
                write!(f, "Boolean Operation \"{}\"", token.representation)
            }
        }
    }
}

enum AbstractNodeEnum<'a> {
    AbstractProduction(Rc<RefCell<AbstractProduction<'a>>>),
    Terminal(&'a Token),
}

// done like this because tokens
struct AbstractProduction<'a> {
    abstract_type: AbstractProductionType,
    children: Vec<AbstractNodeEnum<'a>>,
    parent: Option<Weak<RefCell<AbstractProduction<'a>>>>,
}

pub struct AbstractSyntaxTree<'a, T> {
    root: Rc<RefCell<AbstractProduction<'a>>>,
    last_production: Weak<RefCell<AbstractProduction<'a>>>,
    // only to make the compiler happy ig, but I think the documentation does make me think
    //     this is the correct pattern
    marker: PhantomData<T>,
}

impl<'a, T> AbstractSyntaxTree<'a, T>
where
    T: Iterator<Item = &'a Token>,
{
    pub fn new(cst: ConcreteSyntaxTree<'a, T>) -> Self {
        let root_node = Rc::new(RefCell::new(AbstractProduction {
            abstract_type: AbstractProductionType::Block,
            children: vec![],
            parent: None,
        }));

        let mut ast = AbstractSyntaxTree {
            root: root_node.clone(),
            last_production: Rc::downgrade(&root_node),
            marker: PhantomData,
        };
        let root = cst.get_root();
        match root {
            Some(node) => ast.build(node),
            None => panic!("No valid root for ast??"),
        }

        return ast;
    }

    fn build(&mut self, root_week: Weak<RefCell<Production<'a>>>) {
        let root_strong = root_week.upgrade().unwrap();
        // because of the current rules of our grammar validly parsed code
        //    will always have a program then a block which we can skip over
        //    since we dont care about program and we init a block
        let root = root_strong.borrow();
        let first_child = root.children.first().unwrap();
        let root_block;
        match first_child {
            NodeEnum::Production(production) => root_block = production.borrow(),
            NodeEnum::Terminal(_) => panic!("Unexpected cst structure"),
        }
        self.add_subtree(root_block)
    }

    fn add_subtree(&mut self, root: Ref<Production<'a>>) {
        for child in root.children.iter() {
            let added_production = self.add_node(child);
            match child {
                NodeEnum::Production(production_strong) => {
                    self.add_subtree(production_strong.borrow());
                }
                // do not need to add any more productions
                NodeEnum::Terminal(_) => {}
            }
            if added_production {
                self.up_root();
            }
        }
    }

    fn add_node(&mut self, node: &NodeEnum<'a>) -> bool {
        let mut added_production = false;
        match node {
            NodeEnum::Production(production_strong) => {
                let production = production_strong.borrow();
                added_production = true;
                match production.rule {
                    ProductionRule::Block => self.add_production(AbstractProductionType::Block),
                    ProductionRule::PrintStatement => {
                        self.add_production(AbstractProductionType::PrintStatement)
                    }
                    ProductionRule::AssignmentStatement => {
                        self.add_production(AbstractProductionType::AssignmentStatement)
                    }
                    ProductionRule::VarDecl => self.add_production(AbstractProductionType::VarDecl),
                    ProductionRule::WhileStatement => {
                        self.add_production(AbstractProductionType::WhileStatement)
                    }
                    ProductionRule::IfStatement => {
                        self.add_production(AbstractProductionType::IfStatement)
                    }

                    // Int expression are always just addition
                    ProductionRule::IntExpr => added_production = self.add_int_expr(production),
                    ProductionRule::StringExpr => {
                        self.add_production(AbstractProductionType::StringExpr(Token {
                            kind: TokenKind::StringLiteral,
                            start_end_position: (0,0), // do not know yet
                            line: 0,
                            representation: "".to_string(),
                        }))
                    }
                    // boolean expression shall store the operation ( == != )
                    ProductionRule::BooleanExpr => {
                        // have to add a dummy token that will be overwritten
                        let dummy_token = Token {
                            kind: TokenKind::Char(Char { letter: 'x' }),
                            start_end_position: (0, 0),
                            line: 0,
                            representation: "x".to_string(),
                        };
                        self.add_production(AbstractProductionType::Boolop(dummy_token));
                    }
                    // productions just for the derivation
                    _ => added_production = false,
                }
            }
            // I choose to be explicit here to see all the terminals I am not adding
            NodeEnum::Terminal(terminal) => match &terminal.kind {
                // all keywords can just get add as is
                TokenKind::Keyword(keyword) => {
                    match keyword {
                        Keyword::Boolean => self.add_terminal(terminal),
                        Keyword::String => self.add_terminal(terminal),
                        Keyword::Int => self.add_terminal(terminal),
                        Keyword::True => self.add_terminal(terminal),
                        Keyword::False => self.add_terminal(terminal),
                        // gotten through the statements
                        Keyword::LoopOnTrue => {}
                        Keyword::If => {}
                        Keyword::Print => {}
                    }
                }
                TokenKind::Id(_) => self.add_terminal(terminal),
                TokenKind::Symbol(symbol) => match symbol {
                    Symbol::CheckEquality => self.add_boolop(terminal),
                    Symbol::CheckInequality => self.add_boolop(terminal),
                    // terminals just for parse
                    Symbol::Addition => {} // this one does not matter because all intop are
                    // addition
                    Symbol::Assignment => {}
                    Symbol::QuotatioinMark => {}
                    Symbol::EndProgram => {}
                    Symbol::OpenBlock => {}
                    Symbol::CloseBlock => {}
                    Symbol::OpenParenthesis => {}
                    Symbol::CloseParenthesis => {}
                },
                TokenKind::Digit(_) => self.add_terminal(terminal),
                TokenKind::Char(char) => self.add_char(char.letter, terminal),
                // token only used for string literal
                TokenKind::StringLiteral => {}
            },
        }
        return added_production;
    }

    fn up_root(&mut self) {
        let last_production_weak = &self.last_production;
        let last_production_strong = last_production_weak.upgrade().unwrap();
        let last_production = last_production_strong.borrow();
        match &last_production.parent {
            Some(parent) => self.last_production = parent.clone(),
            None => panic!("cannot uproot when there is no parent"),
        }
    }

    fn add_production(&mut self, abstract_type: AbstractProductionType) {
        let new_production = Rc::new(RefCell::new(AbstractProduction {
            abstract_type,
            children: vec![],
            parent: Some(self.last_production.clone()),
        }));
        let production_weak = &self.last_production;
        let production_strong = production_weak.upgrade().unwrap();
        let mut last_production = production_strong.borrow_mut();
        let new_node = AbstractNodeEnum::AbstractProduction(new_production.clone());
        last_production.children.push(new_node);
        self.last_production = Rc::downgrade(&new_production);
    }

    fn add_terminal(&mut self, token: &'a Token) {
        let new_node = AbstractNodeEnum::Terminal(token);
        let production_weak = &self.last_production;
        let production_strong = production_weak.upgrade().unwrap();
        let mut last_production = production_strong.borrow_mut();
        last_production.children.push(new_node);
    }

    // expects last_production to be StringExpr
    fn add_char(&mut self, ch: char, token: &Token) {
        let temp_strong = &self.last_production.upgrade().unwrap();
        let mut last_production = temp_strong.borrow_mut();
        match &last_production.abstract_type {
            AbstractProductionType::StringExpr(original_token) => {
                let mut new_token = original_token.clone();
                if new_token.start_end_position.0 == 0 {
                    new_token.start_end_position.0 = token.start_end_position.0;
                }
                new_token.start_end_position.1 = token.start_end_position.1;
                new_token.representation = (new_token.representation + &ch.to_string()).to_string();
                last_production.abstract_type = AbstractProductionType::StringExpr(
                    new_token
                );
            }
            _ => panic!("Expected last production to be string expr"),
        }
    }

    fn add_boolop(&mut self, token: &'a Token) {
        let temp_strong = &self.last_production.upgrade().unwrap();
        let mut current_production = temp_strong.borrow_mut();
        match &current_production.abstract_type {
            AbstractProductionType::Boolop(__dummy_token) => {
                current_production.abstract_type = AbstractProductionType::Boolop(token.clone());
            }
            _ => panic!("Expected last production to be boolop"),
        }
    }

    // int expr should be an int op (in this case always add) if
    //    the int expr has more than one child
    // this is because int op expr can be digit or digit + expr
    fn add_int_expr(&mut self, production: Ref<Production>) -> bool {
        if production.children.len() == 1 {
            return false;
        }
        self.add_production(AbstractProductionType::Add);
        return true;
    }

    pub fn show(&self) {
        let root_mut = self.root.borrow_mut();
        let node_enum = &root_mut.children;
        println!(" <{}>", root_mut.abstract_type);
        self.traverse(node_enum, 1);
    }

    fn traverse(&self, children: &Vec<AbstractNodeEnum<'a>>, depth: usize) {
        let indent = "-".repeat(depth);
        for child in children {
            match child {
                AbstractNodeEnum::AbstractProduction(p) => {
                    let p_mut = p.borrow_mut();
                    println!("{} <{}>", indent.blue(), p_mut.abstract_type);
                    self.traverse(&p_mut.children, depth + 1);
                }
                AbstractNodeEnum::Terminal(t) => {
                    println!("{} [{}]", indent.blue(), t.representation)
                }
            }
        }
    }
}

#[derive(Clone, Copy)]
enum DataType {
    Int,
    String,
    Boolean,
}

impl DataType {
    fn to_string(&self) -> String {
        match &self {
            DataType::Int => "Int".to_string(),
            DataType::String => "String".to_string(),
            DataType::Boolean => "Boolean".to_string(),
        }
    }

    // converts token to type if it can be
    fn from_token(token: &Token) -> DataType {
        match &token.kind {
            TokenKind::Keyword(k) => {
                match k {
                    Keyword::True => DataType::Boolean,
                    Keyword::False => DataType::Boolean,
                    _ => panic!("Not a type")
                }
            },
            TokenKind::Id(_) => panic!("Use from reference for references"),
            TokenKind::Digit(_) => DataType::Int,
            _ => panic!("Not a type"),
        }
    }

    fn from_reference(token: &Token, scope: Weak<RefCell<Scope<'_>>>) -> Option<DataType> {
        let name = match &token.kind {
            TokenKind::Id(id) => id.name,
            _ => panic!("expected id"),
        };

        let strong_scope = scope.upgrade().unwrap();
        let scope = strong_scope.borrow();
        if let Some(variable) = scope.variables.get(&name) {
            return Some(variable.data_type);
        }
        if let Some(parent_scope) = &scope.parent {
            return DataType::from_reference(token, parent_scope.clone())
        }
        return None;
    }
}

impl PartialEq for DataType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (DataType::Int, DataType::Int) => true,
            (DataType::Boolean, DataType::Boolean) => true,
            (DataType::String, DataType::String) => true,
            _ => false,
        }
    }
}

#[derive(Clone)]
struct Reference<'a> {
    token: &'a Token,
    scope: Weak<RefCell<Scope<'a>>>,
}

struct RedeclarationError<'a> {
    first_variable: Variable<'a>,
    second_variable: Variable<'a>,
    scope: Weak<RefCell<Scope<'a>>>,
}

// is_init is not included bc there would just be an error if the variable not found
#[derive(Clone)]
struct Variable<'a> {
    token: &'a Token,
    right_of_assignment: Vec<&'a Token>,
    is_init: bool,
    is_used: bool,
    data_type: DataType,
}

struct Scope<'a> {
    // A very simple perfect hash exists for char's and in our care
    //   we only would need to make 26 spots in the array to implement a perfect hash
    // I will still leave this as a HashMap type to make Alan happy though
    variables: HashMap<char, Variable<'a>>,
    children: Vec<Rc<RefCell<Scope<'a>>>>,
    parent: Option<Weak<RefCell<Scope<'a>>>>,
    // represents the path of a variable down the tree could be used to check scope
    // in a flat way
    flat_scopes: Vec<u8>,
}

struct MismatchedTypeError<'a> {
    token: Token,
    scope: Weak<RefCell<Scope<'a>>>,
    data_type: DataType,
    expected_data_type: DataType,
}

pub struct SemanticChecks<'a, T> {
    root: Rc<RefCell<Scope<'a>>>,
    // I was too lazy to do my errors as types (I do kinda reget)
    undeclared_references: Vec<Reference<'a>>,
    uninitialized_reference: Vec<Reference<'a>>,
    redeclared_variables: Vec<RedeclarationError<'a>>,
    mismatched_types: Vec<MismatchedTypeError<'a>>,
    right_of_assignment: Vec<&'a Token>,
    variable_counter: usize,
    marker: PhantomData<T>,
}

impl<'a, T> SemanticChecks<'a, T>
where
    T: Iterator<Item = &'a Token>,
{
    pub fn new(ast: &AbstractSyntaxTree<'a, T>) -> Self {
        let root_node = Rc::new(RefCell::new(Scope {
            variables: HashMap::new(),
            children: vec![],
            parent: None,
            flat_scopes: vec![0],
        }));

        let mut semantically_checked = SemanticChecks {
            root: root_node.clone(),
            undeclared_references: vec![],
            redeclared_variables: vec![],
            uninitialized_reference: vec![],
            mismatched_types: vec![],
            right_of_assignment: vec![],
            variable_counter: 0,
            marker: PhantomData,
        };

        let first_scopes: Vec<u8> = vec![0, 0];
        semantically_checked.add_variables_in_scope(
            ast.root.clone(),
            Rc::downgrade(&root_node),
            &mut first_scopes.clone(),
            None,
            false,
            false,
        );
        semantically_checked.propagate_all_used(0);
        return semantically_checked;
    }

    // for match variables you introduce a match type for 
    //     assignment statements
    //     int operation/ int expression
    //     boolean operation /boolean expression
    // and yes big ugly function that could probably have a couple of methods off of it
    fn add_variables_in_scope(
        &mut self,
        start_production_strong: Rc<RefCell<AbstractProduction<'a>>>,
        scope: Weak<RefCell<Scope<'a>>>,
        flat_scopes: &mut Vec<u8>,
        match_type: Option<DataType>,
        in_assignment: bool,
        skip_first_child: bool,
    ) {
        let start_production = start_production_strong.borrow();
        let children;
        if skip_first_child {
            children = start_production.children.iter().skip(1);
        } else {
            children = start_production.children.iter().skip(0);
        }
        for child in children {
            let production_strong = match child {
                AbstractNodeEnum::AbstractProduction(p) => p,
                AbstractNodeEnum::Terminal(t) => match &t.kind {
                    TokenKind::Id(_) => {
                        if let Some(to_match) = match_type {
                            if let Some(variable_data_type) = DataType::from_reference(t, scope.clone()) {
                                if to_match != variable_data_type {
                                    self.mismatched_types.push(MismatchedTypeError {
                                        token: t.clone().clone(),
                                        scope: scope.clone(),
                                        data_type: DataType::Boolean,
                                        expected_data_type: to_match,
                                    })
                                }
                            }
                        }
                        self.use_variable(&scope, t, in_assignment, &scope);
                        continue;
                    }
                    TokenKind::Keyword(k) => match k {
                        Keyword::True => {
                            if let Some(to_match) = match_type {
                                if !matches!(to_match, DataType::Boolean) {
                                    self.mismatched_types.push(MismatchedTypeError {
                                        token: t.clone().clone(),
                                        scope: scope.clone(),
                                        data_type: DataType::Boolean,
                                        expected_data_type: to_match,
                                    })
                                }
                            }
                            continue;
                        },
                        Keyword::False => {
                            if let Some(to_match) = match_type {
                                if !matches!(to_match, DataType::Boolean) {
                                    self.mismatched_types.push(MismatchedTypeError {
                                        token: t.clone().clone(),
                                        scope: scope.clone(),
                                        data_type: DataType::Boolean,
                                        expected_data_type: to_match,
                                    })
                                }
                            }
                            continue;
                        },
                        _ => continue,
                    },
                    TokenKind::Digit(_) => {
                        if let Some(to_match) = match_type {
                            if !matches!(to_match, DataType::Int) {
                                self.mismatched_types.push(MismatchedTypeError {
                                    token: t.clone().clone(),
                                    scope: scope.clone(),
                                    data_type: DataType::Int,
                                    expected_data_type: to_match,
                                })
                            }
                        }
                        continue;
                    }
                    _ => continue,
                },
            };
            let production = production_strong.borrow();
            match &production.abstract_type {
                AbstractProductionType::Block => {
                    // create a new scope owner for this scope and all siblings
                    let new_scopes = &mut flat_scopes.clone();
                    new_scopes.push(0);
                    let new_scope = self.add_scope(&flat_scopes.clone(), scope.clone());
                    let last_scope = flat_scopes.last_mut().expect("scope list was empty??");
                    *last_scope += 1;
                    self.add_variables_in_scope(
                        production_strong.clone(),
                        new_scope.clone(),
                        new_scopes,
                        match_type,
                        false,
                        false,
                    );
                }
                AbstractProductionType::VarDecl => {
                    let mut var_children = production.children.iter();
                    let variable_type = match var_children.next().unwrap() {
                        AbstractNodeEnum::Terminal(t) => match &t.kind {
                            TokenKind::Keyword(k) => match k {
                                Keyword::Boolean => DataType::Boolean,
                                Keyword::String => DataType::String,
                                Keyword::Int => DataType::Int,
                                _ => panic!("expected type"),
                            },
                            _ => panic!("expected type"),
                        },
                        AbstractNodeEnum::AbstractProduction(_) => panic!("expected type"),
                    };
                    let variable_token = match var_children.next().unwrap() {
                        AbstractNodeEnum::Terminal(t) => match &t.kind {
                            TokenKind::Id(_) => *t,
                            _ => panic!("expected id"),
                        },
                        AbstractNodeEnum::AbstractProduction(_) => panic!("expected id"),
                    };

                    let variable = Variable {
                        token: variable_token,
                        data_type: variable_type,
                        right_of_assignment: vec![],
                        is_used: false,
                        is_init: false,
                    };
                    self.add_variable(variable, scope.clone());
                }
                AbstractProductionType::AssignmentStatement => {
                    let inited_node = production.children.first().unwrap();
                    let inited_token = match inited_node {
                        AbstractNodeEnum::Terminal(t) => t,
                        AbstractNodeEnum::AbstractProduction(_) => panic!("expected id token"),
                    };
                    let needed_data_type = DataType::from_reference(inited_token, scope.clone());
                    self.right_of_assignment = vec![];
                    // process the right
                    self.add_variables_in_scope(
                        production_strong.clone(),
                        scope.clone(),
                        flat_scopes,
                        needed_data_type,
                        true,
                        true,
                    );

                    self.init_variable(&scope, inited_token, self.right_of_assignment.clone());
                }
                AbstractProductionType::Add => {
                    self.add_variables_in_scope(
                        production_strong.clone(),
                        scope.clone(),
                        flat_scopes,
                        Some(DataType::Int),
                        in_assignment,
                        false,
                    );
                },
                AbstractProductionType::StringExpr(t) => {
                    if let Some(to_match) = match_type {
                            if !matches!(to_match, DataType::Int) {
                                self.mismatched_types.push(MismatchedTypeError {
                                    token: t.clone(),
                                    scope: scope.clone(),
                                    data_type: DataType::Int,
                                    expected_data_type: to_match,
                                })
                            }
                        }
                    // the only "production without children so no need to recursive step"
                }
                AbstractProductionType::Boolop(_) => {
                    let mut var_children = production.children.iter();
                    let first_child_token = match var_children.next().unwrap() {
                        AbstractNodeEnum::Terminal(t) => t,
                        AbstractNodeEnum::AbstractProduction(_) => panic!("expected token"),
                    };
                    // silly to evaluate it in the case that we dont use it but I wanted
                    //    to use the cool looking syntax
                    let operation_data_type = match first_child_token.kind {
                        TokenKind::Id(_) => 
                            DataType::from_reference(first_child_token, scope.clone()),
                        _ => Some(DataType::from_token(first_child_token)),
                    };
                    self.add_variables_in_scope(
                        production_strong.clone(),
                        scope.clone(),
                        flat_scopes,
                        match_type.or_else(|| operation_data_type),
                        in_assignment,
                        false,
                    );
                },
                _ => self.add_variables_in_scope(
                    production_strong.clone(),
                    scope.clone(),
                    flat_scopes,
                    match_type,
                    in_assignment,
                    false,
                ),
            }
        }
    }

    fn init_variable(
        &mut self,
        scope: &Weak<RefCell<Scope<'a>>>,
        token: &'a Token,
        right_of_assignment: Vec<&'a Token>,
    ) {
        let name = match &token.kind {
            TokenKind::Id(id) => id.name,
            _ => panic!("expected id"),
        };
        let running_scope_weak = scope; // only should happen on the first
        let running_scope_strong = running_scope_weak.upgrade().unwrap();
        let mut running_scope = running_scope_strong.borrow_mut();
        if let Some(variable) = running_scope.variables.get_mut(&name) {
            // dont add it as a reference her because it will be added later
            variable.is_init = true;
            variable.right_of_assignment = right_of_assignment;
            return
        } 
        if let Some(scope) = &running_scope.parent {
            self.init_variable(&scope, token, right_of_assignment)
        } else {
            let reference = Reference {
                token,
                scope: scope.clone(),
            };
            self.undeclared_references.push(reference);
        }
    }

    fn use_variable(
        &mut self,
        scope: &Weak<RefCell<Scope<'a>>>,
        token: &'a Token,
        in_assignment: bool,
        first_scope: &Weak<RefCell<Scope<'a>>>,
    ) {
        let name = match &token.kind {
            TokenKind::Id(id) => id.name,
            _ => panic!("expected id"),
        };

        // look up the tree from the current position
        let running_scope_weak = scope; // only should happen on the first
        let running_scope_strong = running_scope_weak.upgrade().unwrap();
        let mut running_scope = running_scope_strong.borrow_mut();
        if let Some(variable) = running_scope.variables.get_mut(&name) {
            if !variable.is_init {
                let reference = Reference {
                    scope: first_scope.clone(),
                    token,
                };
                self.uninitialized_reference.push(reference);
                return;
            }
            if in_assignment {
                self.right_of_assignment.push(&token);
            } else {
                variable.is_used = true;
            }
            return;
        }
        if let Some(parent_scope_weak) = &running_scope.parent {
            self.use_variable(parent_scope_weak, token, in_assignment, first_scope);
        } else {
            let reference = Reference {
                scope: first_scope.clone(),
                token,
            };
            self.undeclared_references.push(reference);
        }
    }

    fn add_variable(&mut self, variable: Variable<'a>, scope: Weak<RefCell<Scope<'a>>>) {
        self.variable_counter += 1;
        let name = match &variable.token.kind {
            TokenKind::Id(id) => id.name,
            _ => panic!("expected id"),
        };
        let curret_scope_strong = scope.upgrade().unwrap();
        let mut current_scope = curret_scope_strong.borrow_mut();
        if let Some(first_variable) = current_scope.variables.get(&name) {
            let redeclaration = RedeclarationError {
                first_variable: first_variable.clone(),
                second_variable: variable,
                scope: scope.clone(),
            };
            self.redeclared_variables.push(redeclaration);
        } else {
            current_scope.variables.insert(name, variable);
        }
    }

    fn add_scope(
        &self,
        flat_scopes: &Vec<u8>,
        scope: Weak<RefCell<Scope<'a>>>,
    ) -> Weak<RefCell<Scope<'a>>> {
        let last_scope_strong = scope.upgrade().unwrap();
        let mut last_scope = last_scope_strong.borrow_mut();
        let new_scope = Scope {
            parent: Some(Rc::downgrade(&last_scope_strong)),
            variables: HashMap::new(),
            children: vec![],
            flat_scopes: flat_scopes.clone(),
        };
        let new_scope_ref = Rc::new(RefCell::new(new_scope));
        let new_scope_weak = Rc::downgrade(&new_scope_ref);
        last_scope.children.push(new_scope_ref);
        return new_scope_weak;
    }

    // the idea for this algorithm is to well propagate the used to all
    //    variables. This reminds of Dijkstra's algorithm
    // this is done because if a variable
    fn propagate_all_used(&self, depth: usize) {
        // +2 if depth starts at 0 bc we need to do this variable -1 times and 
        //    depth is 0 indexed whil counter is 1
        if depth >= self.variable_counter + 2 {
            return;
        }
        self.propagate_used(self.root.clone());
        self.propagate_all_used(depth + 1);
    }

    fn propagate_used(&self, scope_strong: Rc<RefCell<Scope>>) {
        let scope = scope_strong.borrow();
        let variables: Vec<Variable> = scope.variables.values().cloned().collect();
        let children_scopes = scope.children.clone();
        drop(scope); // need to allow make_variable_used to have a mutable borrow
        for variable in variables {
            if variable.is_used {
                for token in &variable.right_of_assignment {
                    self.make_variable_used(scope_strong.clone(), token)
                }
            }
        }
        for child_scope in children_scopes {
            self.propagate_used(child_scope)
        }
    }

    fn make_variable_used(&self, scope_strong: Rc<RefCell<Scope>>, token: &Token) {
        let name = match &token.kind {
            TokenKind::Id(id) => id.name,
            _ => panic!("expected id"),
        };

        let mut scope = scope_strong.borrow_mut();

        if let Some(variable) = scope.variables.get_mut(&name) {
            variable.is_used = true;
        } else if let Some(parent_scope) = scope.parent.clone() {
            let parent_strong = parent_scope.upgrade().unwrap();
            self.make_variable_used(parent_strong, token);
        }
    }

    fn get_err_count(&self) -> usize {
        &self.uninitialized_reference.len()
            + &self.redeclared_variables.len()
            + &self.undeclared_references.len()
            + &self.mismatched_types.len()
    }

    pub fn show(&self) {
        let error_count = self.get_err_count();
        if error_count > 0 {
            let tab = "----";
            println!(
                "{} (x{})",
                "\nSEMANTIC ERRORS".red(),
                error_count,
            );
            for reference in &self.uninitialized_reference {
                let scope_strong = reference.scope.upgrade().unwrap();
                let scope = scope_strong.borrow();

                println!(
                    "{} variable \"{}\" found in scope {} at {}",
                    "Uninitialized Reference".red(),
                    reference.token.representation,
                    scope_to_str(scope.flat_scopes.clone()),
                    reference.token.get_position()
                )
            }

            for redeclaration in &self.redeclared_variables {
                let scope_strong = redeclaration.scope.upgrade().unwrap();
                let scope = scope_strong.borrow();
                println!(
                    "{} found in scope {}",
                    "Redeclared variable".red(),
                    scope_to_str(scope.flat_scopes.clone())
                );
                println!(
                    "{}declaration of \"{}\" at {} {}",
                    tab.red(),
                    redeclaration.first_variable.token.representation,
                    redeclaration.first_variable.token.get_position(),
                    "AND".red()
                );
                println!(
                    "{}declaration of \"{}\" at {}",
                    tab.red(),
                    redeclaration.second_variable.token.representation,
                    redeclaration.second_variable.token.get_position()
                );
            }

            for reference in &self.undeclared_references {
                let scope_strong = reference.scope.upgrade().unwrap();
                let scope = scope_strong.borrow();

                println!(
                    "{} variable \"{}\" found in scope {} at {}",
                    "Undeclared Reference".red(),
                    reference.token.representation,
                    scope_to_str(scope.flat_scopes.clone()),
                    reference.token.get_position()
                )
            }

            for mismatched_type in &self.mismatched_types {
                let scope_strong = mismatched_type.scope.upgrade().unwrap();
                let scope = scope_strong.borrow();

                println!(
                    "{} \"{}\" found in scope {} at {}",
                    "Mismatched Type".red(),
                    mismatched_type.token.representation,
                    scope_to_str(scope.flat_scopes.clone()),
                    mismatched_type.token.get_position(),
                );
                println!("{}expected: {}", tab, mismatched_type.expected_data_type.to_string());
                println!("{}found: {}", tab, mismatched_type.data_type.to_string());
            }

        }
        println!();
        println!("{}", "Displaying the symbol table".magenta());
        println!("{}", " NAME TYPE        INITED?  USED?  SCOPE".blue());
        SemanticChecks::<T>::show_self_children(&self.root);
    }

    fn show_self_children(scope_strong: &Rc<RefCell<Scope<'a>>>) {
        let col_width1 = 5;
        let col_width2 = 12;
        let col_width3 = 9;
        let col_width4 = 7;
        let col_width5 = 6;
        let total_width = col_width1 + col_width2 + col_width3 + col_width4 + col_width5;
        let total_width_half = total_width / 2;


        let scope = scope_strong.borrow();

        for variable in scope.variables.values() {
            print!("[");
            print!("{name:<col_width1$}", name = variable.token.representation);
            print!(
                "{data_type:<col_width2$}",
                data_type = variable.data_type.to_string()
            );
            print!("{is_init:<col_width3$}", is_init = variable.is_init);
            print!("{is_used:<col_width4$}", is_used = variable.is_used);
            let joined_scopes = scope
                .flat_scopes
                .iter()
                .map(|&num| num.to_string())
                .collect::<Vec<String>>()
                .join(".");
            print!("{scope:<col_width5$}", scope = joined_scopes);
            println!("]");
            if !variable.is_used {
                println!(
                    "{space1}{warning}{space2}",
                    space1="^".repeat(total_width_half - 9).yellow(),
                    warning="WARNING UNUSED VARIABLE".yellow(),
                    space2="^".repeat(total_width_half - 10).yellow(),
                    )
            }
        }
        for child_scope in scope.children.iter() {
            SemanticChecks::<T>::show_self_children(child_scope)
        }
    }
}

fn scope_to_str(scope: Vec<u8>) -> String {
    let joined_scopes = scope
        .iter()
        .map(|&num| num.to_string())
        .collect::<Vec<String>>()
        .join(".");
    return joined_scopes.to_string();
}

#[cfg(test)]
mod semantic_tests {
    use super::*;
    use crate::lex::*;
    use std::path::Path;
    use std::slice::Iter;

    // I should have never used generics :(
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
    fn helper_get_cst<'a>(tokens: Iter<'a, Token>) -> ConcreteSyntaxTree<'a, Iter<'a, Token>> {
        let cst = ConcreteSyntaxTree::new(tokens);
        cst.show_parse_steps();
        cst.show();
        if cst.is_err() {
            panic!("Error duing parse!!");
        }
        return cst;
    }

    // I did not have time / feel like doing programmatic test :(
    fn general_helper(path_str: &str) {
        let tokens = helper_get_tokens(path_str);
        let cst = helper_get_cst(tokens.iter());
        let ast = AbstractSyntaxTree::new(cst);
        let scope_tree = SemanticChecks::new(&ast);
        println!("Showing AST:");
        println!();
        ast.show();
        println!("Showing Scope table");
        println!();
        println!();
        scope_tree.show();
    }

    #[test]
    fn hello_semantic_analysis() {
        // file: {}$
        let path_str = "test_cases/general/hello-compiler";
        general_helper(path_str);
    }

    #[test]
    fn general_semantics() {}

    #[test]
    fn is_not_used() {
        let path_str = "test_cases/semantic-edge-cases/is-not-used";
        general_helper(path_str);
        let path_str = "test_cases/semantic-edge-cases/is-not-used-complicated";
        general_helper(path_str);
    }

    #[test]
    fn propagate_used() {
        let path_str = "test_cases/semantic-edge-cases/propagate-used";
        general_helper(path_str);
        let path_str = "test_cases/semantic-edge-cases/propagate-used-complicated";
        general_helper(path_str);
    }

    #[test]
    fn ok_block_scope_hell() {
        let path_str = "test_cases/semantic-edge-cases/ok-block-scope-hell";
        general_helper(path_str);
    }

    #[test]
    fn err_block_scope_hell() {
        let path_str = "test_cases/semantic-edge-cases/err-block-scope-hell";
        general_helper(path_str);
    }

    #[test]
    fn err_redeclaration() {
        let path_str = "test_cases/semantic-edge-cases/err-redeclaration";
        general_helper(path_str);
    }

    #[test]
    fn match_types() {
        let path_str = "test_cases/semantic-edge-cases/match-types";
        general_helper(path_str);
    }
}

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
    StringExpr(String), // sort of a mismatch because string expr will have no children but whtever
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
                write!(f, "String Expression \"{}\"", expression)
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
            None => todo!(),
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
                        self.add_production(AbstractProductionType::StringExpr("".to_string()))
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
                TokenKind::Char(char) => self.add_char(char.letter),
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
        println!();
        println!("adding {}", &abstract_type);
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
        println!("to {}", last_production.abstract_type);
        println!();
    }

    fn add_terminal(&mut self, token: &'a Token) {
        println!();
        println!("adding {}", token.representation);
        let new_node = AbstractNodeEnum::Terminal(token);
        let production_weak = &self.last_production;
        let production_strong = production_weak.upgrade().unwrap();
        let mut last_production = production_strong.borrow_mut();
        println!("to {}", last_production.abstract_type);
        println!();
        last_production.children.push(new_node);
    }

    // expects last_production to be StringExpr
    fn add_char(&mut self, ch: char) {
        let temp_strong = &self.last_production.upgrade().unwrap();
        let mut last_production = temp_strong.borrow_mut();
        println!("add ch {} to {} ", &ch, last_production.abstract_type);
        match &last_production.abstract_type {
            AbstractProductionType::StringExpr(current_string) => {
                last_production.abstract_type = AbstractProductionType::StringExpr(
                    current_string.to_string() + &ch.to_string(),
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

struct Reference<'a> {
    token: &'a Token,
    scope: Weak<RefCell<Scope<'a>>>,
}

// is_init is not included bc there would just be an error if the variable not found
struct Variable<'a> {
    token: &'a Token,
    references: Vec<Reference<'a>>,
    is_used: bool,
    data_type: &'a Token,
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

struct ScopeTree<'a, T> {
    root: Rc<RefCell<Scope<'a>>>,
    current_scope: Weak<RefCell<Scope<'a>>>,
    undeclared_references: Vec<Reference<'a>>,
    marker: PhantomData<T>,
}

impl<'a, T> ScopeTree<'a, T>
where
    T: Iterator<Item = &'a Token>,
{
    pub fn new(ast: &AbstractSyntaxTree<T>) -> Self {
        let root_node = Rc::new(RefCell::new(Scope {
            variables: HashMap::new(),
            children: vec![],
            parent: None,
            flat_scopes: vec![0],
        }));

        let scope_tree = ScopeTree {
            current_scope: Rc::downgrade(&root_node),
            root: root_node,
            undeclared_references: vec![],
            marker: PhantomData
        };

        return scope_tree;
    }

    fn add_variables_in_scope(
        &mut self,
        start_production_strong: Rc<RefCell<AbstractProduction<'a>>>,
        scopes: &mut Vec<u8>,
        include_as_used: bool,
        ) 
    {
        let start_production = start_production_strong.borrow_mut();
        for child in &start_production.children {
            let production_strong = match child {
                AbstractNodeEnum::AbstractProduction(p) => p,
                AbstractNodeEnum::Terminal(t) => match &t.kind {
                    TokenKind::Id(_) => todo!(),
                    _ => continue,
                },
            };
            let production = production_strong.borrow_mut();
            match production.abstract_type {
                AbstractProductionType::Block => {
                    // create a new scope owner for this scope and all siblings
                    let new_scopes = &mut scopes.clone();
                    new_scopes.push(0);
                    // I understand that this is perhaps slightly more complex and misdirected than
                    //    it needs to be... I am mananging instance state through side efffects 
                    //    and the state of recursive calls. could argue that it is better to simply
                    //    add a parameter current_node instead of having it instanced
                    self.add_scope(new_scopes);
                    self.add_variables_in_scope(production_strong.clone(), new_scopes, true);
                    // increment the scope for sibling scopes
                    let last_scope = scopes.last_mut().expect("scope list was empty??");
                    *last_scope += 1;
                }
                AbstractProductionType::VarDecl => {
                    let mut var_children = production.children.iter();
                    let variable_type = match var_children.next().unwrap() {
                        AbstractNodeEnum::Terminal(t) => match &t.kind {
                            TokenKind::Keyword(k) => match k {
                                Keyword::Boolean => *t,
                                Keyword::String => *t,
                                Keyword::Int => *t,
                                _ => panic!("expected type"),
                            },
                            _ => panic!("expected type")
                        },
                        AbstractNodeEnum::AbstractProduction(_) => panic!("expected type"),
                    };
                    let variable_token = match var_children.next().unwrap() {
                        AbstractNodeEnum::Terminal(t) => match &t.kind {
                            TokenKind::Id(_) => *t,
                            _ => panic!("expected id")
                        },
                        AbstractNodeEnum::AbstractProduction(_) => panic!("expected id"),
                    };
 
                    self.add_variable(Variable {
                        token: variable_token,
                        data_type: variable_type,
                        references: vec![],
                        is_used: false,
                    });
                }
                AbstractProductionType::AssignmentStatement => {
                    self.add_variables_in_scope(production_strong.clone(), scopes, false)
                }
                _ => self.add_variables_in_scope(production_strong.clone(), scopes, true),
            }
        }
    }

    fn use_variable(&mut self, scope: &Weak<RefCell<Scope<'a>>>, token: &'a Token, include_as_used: bool) {
        let name = match &token.kind {
            TokenKind::Id(id) => id.name,
            _ => panic!("expected id"),
        };

        // look up the tree from the current position
        let running_scope_weak = scope;// only should happen on the first
        let running_scope_strong = running_scope_weak.upgrade().unwrap();
        let mut running_scope = running_scope_strong.borrow_mut();
        if let Some(variable) = running_scope.variables.get_mut(&name) {
            if include_as_used { variable.is_used = true; }
            let reference = Reference { 
                token,
                scope: scope.clone(),
            };
            variable.references.push(reference);
            return;
        }
        if let Some(parent_scope_weak) = &running_scope.parent {
            self.use_variable(parent_scope_weak, token, include_as_used);
        } else {
            let reference = Reference { 
                scope: scope.clone(),
                token,
            };
            self.undeclared_references.push(reference);
        }
    }

    fn add_variable(&mut self, variable: Variable<'a>) {
        let name = match &variable.token.kind {
            TokenKind::Id(id) => id.name,
            _ => panic!("expected id"),
        };
        let curret_scope_strong = self.current_scope.upgrade().unwrap();
        let mut current_scope = curret_scope_strong.borrow_mut();
        if current_scope.variables.contains_key(&name) {
            todo!()
        }
        current_scope.variables.insert(name, variable);
    }

    fn add_scope(&mut self, flat_scopes: &Vec<u8>) {
        let last_scope = &self.current_scope;
        let last_scope_strong = last_scope.upgrade().unwrap();
        let mut last_scope = last_scope_strong.borrow_mut();
        let new_scope = Scope {
            parent: Some(Rc::downgrade(&last_scope_strong)),
            variables: HashMap::new(),
            children: vec![],
            flat_scopes: flat_scopes.clone()
        };
        let new_scope_ref = Rc::new(RefCell::new(new_scope));
        self.current_scope = Rc::downgrade(&new_scope_ref);
        last_scope.children.push(new_scope_ref);
    }
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

    #[test]
    fn hello_semantic_analysis() {
        // file: {}$
        let path_str = "test_cases/general/hello-compiler";
        let tokens = helper_get_tokens(path_str);
        let cst = helper_get_cst(tokens.iter());
        let ast = AbstractSyntaxTree::new(cst);
        println!("Showing AST:");
        println!();
        ast.show();
    }

    #[test]
    fn general_semantics() {
        // file: {}$
        let path_str = "test_cases/general/lex-with-spaces-no-comments";
        let tokens = helper_get_tokens(path_str);
        let cst = helper_get_cst(tokens.iter());
        let ast = AbstractSyntaxTree::new(cst);
        println!("Showing AST:");
        println!();
        ast.show();
    }

    #[test]
    fn string_semantics() {
        // file: {}$
        let path_str = "test_cases/semantic-edge-cases/combine-chars";
        let tokens = helper_get_tokens(path_str);
        let cst = helper_get_cst(tokens.iter());
        let ast = AbstractSyntaxTree::new(cst);
        println!("Showing AST:");
        println!();
        ast.show();
    }
}

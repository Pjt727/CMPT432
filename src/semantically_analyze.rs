#![allow(dead_code)]
use colored::Colorize;
use crate::parse::*;
use crate::token::*;
use std::cell::Ref;
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
                write!(f, "Boolean Operation {}", token.representation)
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
                    ProductionRule::IntExpr => self.add_production(AbstractProductionType::Add),
                    ProductionRule::StringExpr => {
                        self.add_production(AbstractProductionType::StringExpr("".to_string()))
                    }
                    ProductionRule::BooleanExpr => {
                        self.add_production(AbstractProductionType::BooleanExpr)
                    }
                    ProductionRule::Boolop => self.add_production(AbstractProductionType::Block),
                    ProductionRule::Boolval => self.add_production(AbstractProductionType::Block),
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
                },
                TokenKind::Id(_) => self.add_terminal(terminal),
                TokenKind::Symbol(symbol) => match symbol {
                    Symbol::CheckEquality => self.add_terminal(terminal),
                    Symbol::CheckInequality => self.add_terminal(terminal),
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

    fn add_bool_val(&mut self, boolean: bool) {
    }

    pub fn show(&self) {
        let root_mut = self.root.borrow_mut();
        let node_enum = &root_mut.children;
        println!("<{}>", root_mut.abstract_type);
        self.traverse(node_enum, 1);
    }

    fn traverse(&self, children: &Vec<AbstractNodeEnum<'a>>, depth: usize) {
        let indent = "-".repeat(depth);
        for child in children {
            match child {
                AbstractNodeEnum::AbstractProduction(p) => {
                    let p_mut = p.borrow_mut();
                    println!("{}<{}>", indent.blue(), p_mut.abstract_type);
                    self.traverse(&p_mut.children, depth + 1);
                }
                AbstractNodeEnum::Terminal(t) => {
                    println!("{}[{}]", indent.blue(), t.representation)
                },
            }
        }
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
    fn helper_get_cst<'a>(tokens: Iter<'a, Token>) -> ConcreteSyntaxTree<'a, Iter<'a, Token>> 
    {
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
    fn genernal_semantics() {
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

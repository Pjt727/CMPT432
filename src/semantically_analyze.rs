#![allow(dead_code)]
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
    BooleanExpr,
    Boolop(Token),
    Boolval,
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
                write!(f, "String Expression {}", expression)
            }
            AbstractProductionType::BooleanExpr => write!(f, "Boolean Expression"),
            AbstractProductionType::Boolop(token) => {
                write!(f, "Boolean Operation {}", token.representation)
            }
            AbstractProductionType::Boolval => write!(f, "Boolean Value"),
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
    root: Result<Rc<RefCell<AbstractProduction<'a>>>, SematincError>,
    last_production: Weak<RefCell<AbstractProduction<'a>>>,
    marker: PhantomData<T>, // only to make the compiler happy ig, there must be a better way
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
            root: Ok(root_node.clone()),
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
            NodeEnum::Production(production) => root_block=production.borrow(),
            NodeEnum::Terminal(_) => panic!("Unexpected cst structure"),
        }
        self.add_subtree(root_block)
    }

    fn add_subtree(&mut self, root: Ref<Production<'a>>) {
        for child in root.children.iter() {
            let add_production = self.add_node(child);
        }
    }

    fn add_node(&mut self, node: &NodeEnum<'a>) -> bool{
        let mut added_production = false;
        match node {
            NodeEnum::Production(production_strong) => {
                let production = production_strong.borrow();
                added_production = true;
                match production.rule {
                    ProductionRule::Block => todo!(),
                    ProductionRule::PrintStatement => todo!(),
                    ProductionRule::AssignmentStatement => todo!(),
                    ProductionRule::VarDecl => todo!(),
                    ProductionRule::WhileStatement => todo!(),
                    ProductionRule::IfStatement => todo!(),
                    ProductionRule::IntExpr => todo!(),
                    ProductionRule::StringExpr => todo!(),
                    ProductionRule::BooleanExpr => todo!(),
                    ProductionRule::Id => todo!(),
                    ProductionRule::CharList => todo!(),
                    ProductionRule::Boolop => todo!(),
                    ProductionRule::Boolval => todo!(),
                    ProductionRule::Intop => todo!(),
                    // productions just for the derivation
                    _ => { added_production = false }
                }
            },
            NodeEnum::Terminal(terminal) => match &terminal.kind {
                TokenKind::Keyword(keyword) => match keyword {
                    Keyword::LoopOnTrue => todo!(),
                    Keyword::If => todo!(),
                    Keyword::Boolean => todo!(),
                    Keyword::String => todo!(),
                    Keyword::Int => todo!(),
                    Keyword::True => todo!(),
                    Keyword::False => todo!(),
                    Keyword::Print => todo!(),
                },
                TokenKind::Id(_) => todo!(),
                TokenKind::Symbol(symbol) => match symbol {
                    Symbol::CheckEquality => todo!(),
                    Symbol::CheckInequality => todo!(),
                    // terminals just for parse
                    Symbol::Addition => {}, // this one does not matter because all intop are
                                            // addition
                    Symbol::Assignment => {},
                    Symbol::QuotatioinMark => {},
                    Symbol::EndProgram => {},
                    Symbol::OpenBlock => {},
                    Symbol::CloseBlock => {},
                    Symbol::OpenParenthesis => {},
                    Symbol::CloseParenthesis => {},
                },
                TokenKind::Digit(_) => self.add_terminal(terminal),
                TokenKind::Char(char) => self.add_char(char.letter),
            },
        }
        return added_production;
    }

    fn up_root(&mut self) {
        if let Err(_) = self.root {
            return;
        }
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
        let mut last_node = production_strong.borrow_mut();
        let new_node = AbstractNodeEnum::AbstractProduction(new_production.clone());
        last_node.children.push(new_node);

        self.last_production = Rc::downgrade(&new_production);
    }
    
    fn add_terminal(&mut self, token: &'a Token) {
        let new_node = AbstractNodeEnum::Terminal(token);
        let production_weak = &self.last_production;
        let production_strong = production_weak.upgrade().unwrap();
        let mut last_node = production_strong.borrow_mut();
        last_node.children.push(new_node);
    }
    
    // expects last_production to be StringExpr
    fn add_char(&mut self, ch: char) {
        let temp_strong = &self.last_production.upgrade().unwrap();
        let mut current_production = temp_strong.borrow_mut();
        match &current_production.abstract_type {
            AbstractProductionType::StringExpr(current_string) => {
                current_production.abstract_type = AbstractProductionType::StringExpr(
                    current_string.to_string() + &ch.to_string());
            },
           _ => panic!("Expected last production to be string expr"),
        }
    }

}

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

        let ast = AbstractSyntaxTree {
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
            self.add_node(child)
        }
    }

    fn add_node(&mut self, node: &NodeEnum<'a>) {
        match node {
            NodeEnum::Production(production_strong) => {
                let production = production_strong.borrow();
                match production.rule {
                    ProductionRule::Program => todo!(),
                    ProductionRule::Block => todo!(),
                    ProductionRule::StatementList => todo!(),
                    ProductionRule::Statement => todo!(),
                    ProductionRule::PrintStatement => todo!(),
                    ProductionRule::AssignmentStatement => todo!(),
                    ProductionRule::VarDecl => todo!(),
                    ProductionRule::WhileStatement => todo!(),
                    ProductionRule::IfStatement => todo!(),
                    ProductionRule::Expr => todo!(),
                    ProductionRule::IntExpr => todo!(),
                    ProductionRule::StringExpr => todo!(),
                    ProductionRule::BooleanExpr => todo!(),
                    ProductionRule::Id => todo!(),
                    ProductionRule::CharList => todo!(),
                    ProductionRule::Type => todo!(),
                    ProductionRule::Char => todo!(),
                    ProductionRule::Boolop => todo!(),
                    ProductionRule::Boolval => todo!(),
                    ProductionRule::Intop => todo!(),
                }
            },
            NodeEnum::Terminal(terminal) => match terminal.kind {
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
                    Symbol::OpenBlock => todo!(),
                    Symbol::CloseBlock => todo!(),
                    Symbol::OpenParenthesis => todo!(),
                    Symbol::CloseParenthesis => todo!(),
                    Symbol::QuotatioinMark => todo!(),
                    Symbol::Assignment => todo!(),
                    Symbol::CheckEquality => todo!(),
                    Symbol::CheckInequality => todo!(),
                    Symbol::Addition => todo!(),
                    Symbol::EndProgram => todo!(),
                },
                TokenKind::Digit(_) => todo!(),
                TokenKind::Char(ch) => self.bubble_char_up(ch),
            },
        }
    }
    
    fn bubble_char_up(&mut self, ch: Char) {
        todo!()
    }

}

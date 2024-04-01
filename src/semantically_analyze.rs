#![allow(dead_code)]
use crate::parse::*;
use crate::token::*;
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
    Add,                // Would need to expand to add other int ops
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
            AbstractProductionType::AssignmentStatement => write!(f, "Assignment Statment"),
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

enum NodeEnum<'a> {
    AbstractProduction(Rc<RefCell<AbstractProduction<'a>>>),
    Terminal(&'a Token),
}

// done like this because tokens
struct AbstractProduction<'a> {
    abtract_type: AbstractProductionType,
    children: Vec<NodeEnum<'a>>,
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
            abtract_type: AbstractProductionType::Block,
            children: vec![],
            parent: None,
        }));

        let ast = AbstractSyntaxTree {
            root: Ok(root_node.clone()),
            last_production: Rc::downgrade(&root_node),
            marker: PhantomData,
        };

        return ast;
    }
}

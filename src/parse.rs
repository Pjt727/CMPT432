
use std::{cell::RefCell, rc::Rc, rc::Weak};

mod token;
use token::*;

struct Production<'a> {
    // chosen as string instead of a enum because
    // every produciton rule is only added in one place
    rule: String,
    children: Vec<NodeEnum<'a>>,
    parent: Option<Weak<RefCell<Production<'a>>>>,
}

struct ParseError<'a> {
    token_found: Option<&'a Token>,
    expected_kinds: Vec<TokenKind>,
}

enum NodeEnum<'a> {
    Production(Rc<RefCell<Production<'a>>>),
    Terminal(&'a Token),
}

struct ConcreteSyntaxTree<'a, T>
where
    T: Iterator<Item = &'a Token>,
{
    root: Result<Rc<RefCell<Production<'a>>>, ParseError<'a>>,
    tokens: T,
    // this is only ever supposed to be a production
    // but it would be difficult to change types becuase nodes reference counted
    last_production: Weak<RefCell<Production<'a>>>,
}

impl<'a, T> ConcreteSyntaxTree<'a, T>
where
    T: Iterator<Item = &'a Token>,
{
    pub fn new(tokens: T) -> Self {
        let root_node = Rc::new(RefCell::new(Production {
            rule: "program".to_string(),
            children: vec![],
            parent: None,
        }));
        let mut cst = ConcreteSyntaxTree {
            root: Ok(root_node.clone()),
            tokens,
            last_production: Rc::downgrade(&root_node),
        };
        cst.do_program();
        return cst;
    }

    // moves the last_node to the parrent of the current node
    fn up_root(&mut self) {
        let last_production_weak = &self.last_production;
        let last_production_strong = last_production_weak.upgrade().unwrap();
        let last_production = last_production_strong.borrow_mut();
        match &last_production.parent {
            Some(parent) => self.last_production = parent.clone(),
            None => panic!("cannot uproot when there is no parent"),
        }
    }

    // has the side affect of moving the last_node to the added production
    fn add_production(&mut self, production_name: String) {
        let last_production_weak = &self.last_production;
        let last_production_strong = last_production_weak.upgrade().unwrap();
        let mut last_node = last_production_strong.borrow_mut();
        let new_production = Rc::new(RefCell::new(Production {
            rule: production_name,
            children: vec![],
            parent: Some(last_production_weak.clone()),
        }));
        self.last_production = Rc::downgrade(&new_production);
        let node = NodeEnum::Production(new_production);
        last_node.children.push(node);
    }

    // always consumes token
    fn match_kind(&mut self, kinds: Vec<TokenKind>) {
        let last_production_weak = &self.last_production;
        let last_production_strong = last_production_weak.upgrade().unwrap();
        let mut last_node = last_production_strong.borrow_mut();
        let token = self.tokens.next();
        match token {
            Some(t) => {
                for kind in kinds {
                    // will need to change this to ensure that it properly matches
                    // the nested types
                    if matches!(&t.kind, kind) {
                        last_node.children.push(NodeEnum::Terminal(&t));
                        return;
                    }
                }
                // will change to error later
                panic!("Did not match any tokens");
            }

            // maybe change this panic to an error message
            None => panic!("Ran out of tokens"),
        }
    }

    fn do_program(&mut self) {
        // other stuff
        self.match_kind(vec![TokenKind::Symbol(Symbol::EndProgram)]);
    }
}

pub fn parse<'a, T>(mut tokens: T)
where
    T: Iterator<Item = &'a Token>,
{
    let cst = ConcreteSyntaxTree::new(tokens);
}




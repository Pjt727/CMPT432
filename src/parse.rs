use std::{cell::RefCell, rc::Rc, rc::Weak};

use crate::token::*;

type Node<'a> = Rc<RefCell<NodeEnum<'a>>>;

struct Production<'a> {
    // chosen as string instead of a enum because
    // every produciton rule is only added in one place
    rule: String,
    children: Vec<Node<'a>>,
    parent: Option<Weak<RefCell<NodeEnum<'a>>>>,
}

struct ParseError<'a> {
    token_found: Option<&'a Token>,
    expected_kinds: Vec<TokenKind>,
}

enum NodeEnum<'a> {
    Production(Production<'a>),
    Terminal(&'a Token),
}

// becauase of the design it is difficult to get the enum type out of the node
// to do the adding so this sort of awkward code arisen where I do the matching in
// this method
impl<'a> NodeEnum<'a> {
    fn add_terminal(&mut self, node: &'a Token) {
        let node = Rc::new(RefCell::new(NodeEnum::Terminal(node)));
        match self {
            NodeEnum::Production(p) => {
                p.children.push(node);
            }
            NodeEnum::Terminal(_) => panic!("Cannot add to a terminal"),
        }
    }

    fn add_production(&mut self, new_production: Node<'a>) {
        match self {
            NodeEnum::Production(p) => {
                p.children.push(new_production);
            }
            NodeEnum::Terminal(_) => panic!("Cannot add to a terminal"),
        }
    }
}

struct ConcreteSyntaxTree<'a, T>
where
    T: Iterator<Item = &'a Token>,
{
    root: Result<Node<'a>, ParseError<'a>>,
    tokens: T,

    // this is only ever supposed to be a production
    // but it would be difficult to change types becuase nodes reference counted
    last_node: Weak<RefCell<NodeEnum<'a>>>,
}

impl<'a, T> ConcreteSyntaxTree<'a, T>
where
    T: Iterator<Item = &'a Token>,
{
    pub fn new(tokens: T) -> Self {
        let root_node = Rc::new(RefCell::new(NodeEnum::Production(Production {
            rule: "program".to_string(),
            children: vec![],
            parent: None,
        })));
        let mut cst = ConcreteSyntaxTree {
            root: Ok(root_node.clone()),
            tokens,
            last_node: Rc::downgrade(&root_node),
        };
        cst.do_program();
        return cst;
    }

    // moves the last_node to the parrent of the current node
    fn up_root(&mut self) {
        let last_node_weak = &self.last_node;
        let last_node = last_node_weak.upgrade().unwrap();
        let last_node_strong = last_node.borrow_mut();
        let node_enum = &*last_node_strong;
        match node_enum {
            NodeEnum::Production(_) => todo!(),
            NodeEnum::Terminal(_) => todo!(),
        }
    }

    // has the side affect of moving the last_node to the added production
    fn add_production(&mut self, production_name: String) {
        let last_node_weak = &self.last_node;
        let last_node = last_node_weak.upgrade().unwrap();
        let mut last_node_strong = last_node.borrow_mut();
        let node = Rc::new(RefCell::new(NodeEnum::Production(Production {
            rule: production_name,
            children: vec![],
            parent: Some(last_node_weak.clone()),
        })));
        self.last_node = Rc::downgrade(&node);
        last_node_strong.add_production(node);
    }

    // always consumes token
    fn match_kind(&mut self, kinds: Vec<TokenKind>) {
        let last_node_weak = &self.last_node;
        let last_node = last_node_weak.upgrade().unwrap();
        let mut last_node_strong = last_node.borrow_mut();
        let token = self.tokens.next();
        match token {
            Some(t) => {
                for kind in kinds {
                    // will need to change this to ensure that it properly matches
                    // the nested types
                    if matches!(&t.kind, kind) {
                        last_node_strong.add_terminal(t);
                        return;
                    }
                }
                return;
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

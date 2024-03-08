use std::{cell::RefCell, rc::Rc, rc::Weak};

use crate::token::*;

type Node<'a> = Rc<RefCell<NodeEnum<'a>>>;

struct Production<'a> {
    // chosen as string instead of a enum because
    // every produciton rule is only added in one place
    rule: String,
    children: Vec<Node<'a>>,
    parent: Option<Node<'a>>,
}

struct ParseError<'a> {
    token_found: Option<&'a Token>,
    expected_kinds: Vec<TokenKind>,
}

enum NodeEnum<'a> {
    Production(Production<'a>),
    Terminal(&'a Token),
}

impl<'a> NodeEnum<'a> {
    // it might be better to have this method be on the
    // the production type but it is sort of difficult
    // to get the reference production from a node enum ref count
    fn add_terminal(&mut self, node: &'a Token) {
        let node = Rc::new(RefCell::new(NodeEnum::Terminal(node)));
        match self {
            NodeEnum::Production(p) => {
                p.children.push(node);
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

    fn add_prodction(&mut self, production_name: String) {
        let last_node_weak = &self.last_node;
        let last_node = last_node_weak.upgrade().unwrap();
        let mut last_node_strong = last_node.borrow_mut();
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

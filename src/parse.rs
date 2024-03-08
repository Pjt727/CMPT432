use std::{cell::RefCell, rc::Rc, rc::Weak};

use crate::token::*;

struct Production<'a> {
    rule: String,
    children: Vec<Node<'a>>,
}

struct ParseError<'a> {
    token_found: Option<&'a Token>,
    expected_kinds: Vec<TokenKind>,
}

type Node<'a> = Rc<RefCell<NodeEnum<'a>>>;

enum NodeEnum<'a> {
    Production(Production<'a>),
    Terminal(&'a Token),
}

struct ConcreteSyntaxTree<'a, T>
where
    T: Iterator<Item = &'a Token>,
{
    root: Result<Node<'a>, ParseError<'a>>,
    tokens: T,
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
        })));
        let mut cst = ConcreteSyntaxTree {
            root: Ok(root_node.clone()),
            tokens,
            last_node: Rc::downgrade(&root_node),
        };
        cst.do_program();
        return cst;
    }

    // always consumes token
    fn match_kind(&mut self, kinds: Vec<TokenKind>) {
        let last_node_strong = &mut self.last_node.upgrade().unwrap();
        let token = self.tokens.next();
        match token {
            Some(t) => {
                for kind in kinds {
                    // will need to change this to ensure that it properly matches
                    // the nested types
                    if matches!(&t.kind, kind) {
                        let node = Node::Terminal(&t);
                        last_production.children.push(node);
                        return;
                    }
                }

                return;
            }
            None => return,
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

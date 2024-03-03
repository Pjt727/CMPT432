use crate::token::*;

struct Produdction<'a> { 
    rule: String,
    children: Vec<Node<'a>>
}

struct ParseError<'a> {
    token_found: Option<&'a Token>,
    expected_kinds: Vec<TokenKind>
}

enum Node<'a> {
    Production(Produdction<'a>),
    Terminal(&'a Token)
}

struct ConcreteSyntaxTree<'a, T>
where
    T: Iterator<Item = &'a Token>,
{
    root: Result<Node<'a>, ParseError<'a>>,
    tokens: T,
}

impl<'a, T> ConcreteSyntaxTree<'a, T>
where
    T: Iterator<Item = &'a Token>,
{
    pub fn new(tokens: T) -> Self {
        let root_node = Node::Production(Produdction { 
                rule: "root".to_string(), 
                children: vec![]
            });
        let mut cst = ConcreteSyntaxTree { 
            root: Ok(root_node),
            tokens
        };
        cst.do_program();
        return cst;
    }

    // always consumes token
    fn match_kind(&mut self, kinds: Vec<TokenKind>) {
        let current_production = match &mut self.root {
            Ok(n) => {
                match n {
                    Node::Production(p) => p,
                    // should never be not a production
                    _ => panic!("Tried adding a non terminal to a non terminal?"),
                }
            },
            Err(_) => return
        };
        let token = self.tokens.next();
        match token {
            Some(t) => {
                for kind in kinds {
                    // will need to change this to ensure that it properly matches
                    // the nested types
                    if matches!(&t.kind, kind) {
                        let node = Node::Terminal(&t);
                        current_production.children.push(node);
                        return;
                    }
                }
                
                return;
            },
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


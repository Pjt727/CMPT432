use crate::token::*;


struct Produdction<'a> { 
    rule: String,
    children: Vec<Node<'a>>
}

enum Node<'a> {
    Production(Produdction<'a>),
    Terminal(&'a Token)
}

struct ConcreteSyntaxTree<'a> {
    root: Node<'a>
}

fn program<'a, T>(mut tokens: T)
where
    T: Iterator<Item = &'a Token>,
{

}


pub fn parse<'a, T>(mut tokens: T)
where
    T: Iterator<Item = &'a Token>,
{
    let cst = ConcreteSyntaxTree { 
        root: Node::Production(Produdction { 
            rule: "root".to_string(), 
            children: vec![] 
        })};
    let token = tokens.next();
}


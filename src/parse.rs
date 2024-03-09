#![allow(dead_code)]
use crate::token::*;
use std::{cell::RefCell, iter::Peekable, rc::Rc, rc::Weak};

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

pub struct ConcreteSyntaxTree<'a, T>
where
    T: Iterator<Item = &'a Token>,
{
    root: Result<Rc<RefCell<Production<'a>>>, ParseError<'a>>,
    tokens: Peekable<T>,
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
            tokens: tokens.peekable(),
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
                    if t.is_like(kind) {
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

    /*
     * PARSING GRAMMAR do_GRAMMAR_RULE
     *
     */
    fn do_program(&mut self) {
        //  rule of program already done during init
        self.do_block();
        self.match_kind(vec![TokenKind::Symbol(Symbol::EndProgram)]);
    }

    fn do_block(&mut self) {
        self.add_production("Block".to_string());
        self.match_kind(vec![TokenKind::Symbol(Symbol::OpenBlock)]);
        self.do_statement_list();
        self.match_kind(vec![TokenKind::Symbol(Symbol::CloseBlock)]);
        self.up_root();
    }

    fn do_statement_list(&mut self) {
        self.add_production("Statement List".to_string());
        let expected_kinds = vec![
            TokenKind::Keyword(Keyword::Print),
            TokenKind::Char(Char { letter: 'X' }),
            TokenKind::Keyword(Keyword::Int),
            TokenKind::Keyword(Keyword::Boolean),
            TokenKind::Keyword(Keyword::String),
            TokenKind::Keyword(Keyword::LoopOnTrue),
            TokenKind::Keyword(Keyword::If),
        ];
        let next_token = match self.tokens.peek() {
            Some(t) => t,
            None => {
                self.root = Err(ParseError {
                    token_found: None,
                    expected_kinds,
                });
                return;
            }
        };

        // only thing that should follow the nothing of statement
        if !next_token.is_like(TokenKind::Symbol(Symbol::CloseBlock)) {
            self.do_statement();
            self.do_statement_list();
        }

        self.up_root()
    }

    fn do_statement(&mut self) {
        self.add_production("Statement".to_string());
        let expected_kinds = vec![
            TokenKind::Keyword(Keyword::Print),
            TokenKind::Char(Char { letter: 'X' }),
            TokenKind::Keyword(Keyword::Int),
            TokenKind::Keyword(Keyword::Boolean),
            TokenKind::Keyword(Keyword::String),
            TokenKind::Keyword(Keyword::LoopOnTrue),
            TokenKind::Keyword(Keyword::If),
        ];
        let next_token = match self.tokens.peek() {
            Some(t) => t,
            None => {
                self.root = Err(ParseError {
                    token_found: None,
                    expected_kinds,
                });
                return;
            }
        };
        if next_token.is_like(TokenKind::Keyword(Keyword::Print)) {
            self.do_print_statement();
            // the
        } else if next_token.is_like(TokenKind::Char(Char { letter: 'X' })) {
            self.do_assignment_statement();
        } else if next_token.is_like(TokenKind::Keyword(Keyword::Int))
            || next_token.is_like(TokenKind::Keyword(Keyword::Boolean))
            || next_token.is_like(TokenKind::Keyword(Keyword::String))
        {
            self.do_var_decl();
        } else if next_token.is_like(TokenKind::Keyword(Keyword::LoopOnTrue)) {
            self.do_while_statement();
        } else if next_token.is_like(TokenKind::Keyword(Keyword::If)) {
            self.do_if_statement();
        } else {
            self.root = Err(ParseError {
                token_found: Some(next_token),
                expected_kinds,
            });
        }
        self.up_root();
    }

    fn do_print_statement(&mut self) {
        self.add_production("Print Statement".to_string());
        self.match_kind(vec![TokenKind::Keyword(Keyword::Print)]);
        self.match_kind(vec![TokenKind::Symbol(Symbol::OpenParenthesis)]);

        self.match_kind(vec![TokenKind::Symbol(Symbol::CloseParenthesis)]);
        self.up_root();
    }

    fn do_assignment_statement(&mut self) {
        self.add_production("Assignment Statement".to_string());
        self.do_id();
        self.match_kind(vec![TokenKind::Symbol(Symbol::Assignment)]);
        self.do_expr();
        self.up_root();
    }

    fn do_var_decl(&mut self) {
        self.add_production("Variable Declaration".to_string());
        self.do_type();
        self.do_id();
        self.up_root();
    }

    fn do_while_statement(&mut self) {
        self.add_production("While Statement".to_string());
        self.do_boolean_expr();
        self.do_block();
        self.up_root();
    }

    fn do_if_statement(&mut self) {
        self.add_production("If Statement".to_string());
        self.do_boolean_expr();
        self.do_block();
        self.up_root();
    }

    fn do_expr(&mut self) {
        self.add_production("Expression".to_string());
        let expected_kinds = vec![
            TokenKind::Digit(Digit { value: 0 }),
            TokenKind::Symbol(Symbol::QuotatioinMark),
            TokenKind::Symbol(Symbol::OpenParenthesis),
            TokenKind::Keyword(Keyword::True),
            TokenKind::Keyword(Keyword::False),
            TokenKind::Char(Char { letter: 'X' }),
        ];

        let next_token = match self.tokens.peek() {
            Some(t) => t,
            None => {
                self.root = Err(ParseError {
                    token_found: None,
                    expected_kinds,
                });
                return;
            }
        };

        if next_token.is_like(TokenKind::Digit(Digit { value: 0 })) {
            self.do_int_expr();
        } else if next_token.is_like(TokenKind::Symbol(Symbol::QuotatioinMark)) {
            self.do_string_expr();
        } else if next_token.is_like(TokenKind::Symbol(Symbol::OpenParenthesis))
            || next_token.is_like(TokenKind::Keyword(Keyword::True))
            || next_token.is_like(TokenKind::Keyword(Keyword::False))
        {
            self.do_boolean_expr();
        } else if next_token.is_like(TokenKind::Char(Char { letter: 'X' })) {
            self.do_id();
        } else {
            self.root = Err(ParseError {
                token_found: None,
                expected_kinds,
            });
            return;
        }

        self.up_root();
    }

    fn do_int_expr(&mut self) {
        self.add_production("Int Expression".to_string());
        self.match_kind(vec![TokenKind::Digit(Digit { value: 0 })]);
        let expected_kinds = vec![
            TokenKind::Symbol(Symbol::Addition),
            TokenKind::Symbol(Symbol::CloseParenthesis),
            // and then a lot in case that it was id = expr
            TokenKind::Keyword(Keyword::Print),
            TokenKind::Char(Char { letter: 'X' }),
            TokenKind::Keyword(Keyword::Int),
            TokenKind::Keyword(Keyword::Boolean),
            TokenKind::Keyword(Keyword::String),
            TokenKind::Keyword(Keyword::LoopOnTrue),
            TokenKind::Keyword(Keyword::If),
        ];
        let next_token = match self.tokens.peek() {
            Some(t) => t,
            None => {
                self.root = Err(ParseError {
                    token_found: None,
                    expected_kinds,
                });
                return;
            }
        };

        if next_token.is_like(TokenKind::Symbol(Symbol::Addition)) {
            self.do_int_op();
            self.do_expr();
        } else {
            let mut no_matches = true;
            // need the clone bc I move token kinds
            for token_kind in expected_kinds.clone() {
                if next_token.is_like(token_kind) {
                    no_matches = false;
                    break;
                }
            }
            if no_matches {
                self.root = Err(ParseError {
                    token_found: None,
                    expected_kinds,
                });
                return;
            }
        }

        self.up_root()
    }

    fn do_string_expr(&mut self) {
        self.add_production("String Expression".to_string());
        self.match_kind(vec![TokenKind::Symbol(Symbol::QuotatioinMark)]);
        self.do_char_list();
        self.match_kind(vec![TokenKind::Symbol(Symbol::QuotatioinMark)]);
        self.up_root()
    }

    fn do_boolean_expr(&mut self) {
        self.add_production("Boolean Expression".to_string());
        let expected_kinds = vec![
            TokenKind::Symbol(Symbol::OpenParenthesis),
            TokenKind::Keyword(Keyword::True),
            TokenKind::Keyword(Keyword::False),
        ];
        let next_token = match self.tokens.peek() {
            Some(t) => t,
            None => {
                self.root = Err(ParseError {
                    token_found: None,
                    expected_kinds,
                });
                return;
            }
        };

        if next_token.is_like(TokenKind::Symbol(Symbol::OpenParenthesis)) {
            self.match_kind(vec![TokenKind::Symbol(Symbol::OpenParenthesis)]);
            self.do_expr();
            self.do_bool_op();
            self.do_expr();
            self.match_kind(vec![TokenKind::Symbol(Symbol::CloseParenthesis)]);
        } else if next_token.is_like(TokenKind::Keyword(Keyword::True))
            || next_token.is_like(TokenKind::Keyword(Keyword::False))
        {
            self.do_bool_val();
        }

        self.up_root();
    }

    fn do_id(&mut self) {
        self.add_production("Id".to_string());
        self.match_kind(vec![TokenKind::Char(Char { letter: 'X' })]);
        self.up_root();
    }

    fn do_type(&mut self) {
        self.add_production("Type".to_string());
        self.match_kind(vec![
            TokenKind::Keyword(Keyword::Int),
            TokenKind::Keyword(Keyword::Boolean),
            TokenKind::Keyword(Keyword::String),
        ]);
        self.up_root();
    }

    fn do_bool_op(&mut self) {
        self.add_production("Boolean Operation".to_string());
        self.match_kind(vec![
            TokenKind::Symbol(Symbol::CheckEquality),
            TokenKind::Symbol(Symbol::CheckInequality),
        ]);
        self.up_root();
    }

    fn do_bool_val(&mut self) {
        self.add_production("Boolean Value".to_string());
        self.match_kind(vec![
            TokenKind::Keyword(Keyword::False),
            TokenKind::Keyword(Keyword::True),
        ]);
        self.up_root();
    }

    fn do_int_op(&mut self) {
        self.add_production("Int Operation".to_string());
        self.match_kind(vec![TokenKind::Symbol(Symbol::Addition)]);
        self.up_root();
    }

    fn do_char_list(&mut self) {
        self.add_production("Char List".to_string());
        let expected_kinds = vec![
            TokenKind::Symbol(Symbol::QuotatioinMark),
            TokenKind::Char(Char { letter: 'X' }),
        ];
        let next_token = match self.tokens.peek() {
            Some(t) => t,
            None => {
                self.root = Err(ParseError {
                    token_found: None,
                    expected_kinds,
                });
                return;
            }
        };

        if next_token.is_like(TokenKind::Char(Char { letter: 'X' })) {
            self.match_kind(vec![TokenKind::Char(Char { letter: 'X' })]);
            self.do_char_list();
        } else if !next_token.is_like(TokenKind::Symbol(Symbol::QuotatioinMark)) {
            self.root = Err(ParseError {
                token_found: None,
                expected_kinds,
            });
            return;
        }
        self.up_root();
    }
}

#[cfg(test)]
mod parse_tests {
    use super::*;
    use crate::lex::*;
    use std::{borrow::BorrowMut, path::Path};

    #[test]
    fn hello_parse() {
        // file: {}$
        let path = Path::new("test_cases/general/hello-compiler");
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

        let cst = ConcreteSyntaxTree::new(tokens.iter());
    }
}

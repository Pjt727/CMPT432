#![allow(dead_code)]
#![allow(suspicious_double_ref_op)]
use crate::token::*;
use colored::Colorize;
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
    productions: Vec<String>,
}

impl<'a, T> ConcreteSyntaxTree<'a, T>
where
    T: Iterator<Item = &'a Token>,
{
    pub fn new(tokens: T) -> Self {
        let root_node = Rc::new(RefCell::new(Production {
            rule: "Program".to_string(),
            children: vec![],
            parent: None,
        }));
        let mut cst = ConcreteSyntaxTree {
            root: Ok(root_node.clone()),
            tokens: tokens.peekable(),
            last_production: Rc::downgrade(&root_node),
            productions: vec!["Program".to_string()],
        };
        cst.do_program();
        return cst;
    }

    pub fn has_error(self) -> bool {
        match self.root {
            Ok(_) => false,
            Err(_) => true,
        }
    }
    pub fn show_parse_steps(&self) {
        static INFO_TEXT: &str = "DEBUG INFO parse:";
        static ERROR_TEXT: &str = "DEBUG ERROR parse:";
        for production in &self.productions {
            println!("{} - {}", INFO_TEXT.cyan(), production,);
        }
        if let Err(parse_err) = &self.root {
            let token_names: Vec<&str> = parse_err
                .expected_kinds
                .iter()
                .map(|kind| get_token_verbose_name(kind))
                .collect();

            if let Some(token) = parse_err.token_found {
                let (start_pos, end_pos) = token.start_end_position;
                let position_rep;
                if start_pos == end_pos - 1 {
                    position_rep = format!("{}", start_pos);
                } else {
                    position_rep = format!("{}-{}", start_pos, end_pos,)
                }
                println!(
                    "{} - Expected [ {} ] found {} [ {} ] at {}:{}",
                    ERROR_TEXT.red(),
                    token_names.join(", "),
                    get_token_verbose_name(&token.kind),
                    token.representation,
                    token.line,
                    position_rep
                )
            } else {
                println!(
                    "{} - Expected [ {} ] found nothing",
                    ERROR_TEXT.red(),
                    token_names.join(", ")
                )
            }
        }
    }

    pub fn show_cst(&self) {
        static ERROR_TEXT: &str = "DEBUG ERROR parse:";
        static INFO_TEXT: &str = "Sucessfully parsed";
        let root = match &self.root {
            Ok(root) => {
                println!("{} - Showing CST", INFO_TEXT.magenta());
                root
            }
            Err(_) => {
                println!("{} - Parse Error Skipping CST", ERROR_TEXT.red());
                return;
            }
        };
        let root_mut = root.borrow_mut();
        let node_enum = &root_mut.children;
        println!("<{}>", root_mut.rule);
        self.traverse_cst(node_enum, 1);
    }

    fn traverse_cst(&self, children: &Vec<NodeEnum<'a>>, depth: usize) {
        let indent = "-".repeat(depth);
        for child in children {
            match child {
                NodeEnum::Production(p) => {
                    let p_mut = p.borrow_mut();
                    println!("{}<{}>", indent.blue(), p_mut.rule);
                    self.traverse_cst(&p_mut.children, depth + 1);
                }
                NodeEnum::Terminal(t) => {
                    println!("{}[{}]", indent.blue(), t.representation)
                }
            }
        }
    }
    // moves the last_node to the parrent of the current node
    fn up_root(&mut self) {
        if let Err(_) = self.root {
            return;
        }
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
        if let Err(_) = self.root {
            return;
        }
        self.productions.push(production_name.clone());
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

    fn add_error(&mut self, error: ParseError<'a>) {
        match self.root {
            Ok(_) => {
                // exhust the tooken iter can
                //    have a different implementation
                //    for error recovery
                let _rest_of_tokens: Vec<&Token> = self.tokens.by_ref().collect();
                self.root = Err(error);
            }

            // do nothing if there is already an error
            //    can adapt this function if meaningful errors can be gained
            //    after the first one
            Err(_) => {}
        }
    }

    // always consumes token
    fn match_kind(&mut self, kinds: Vec<TokenKind>) {
        let last_production_weak = &self.last_production;
        let last_production_strong = last_production_weak.upgrade().unwrap();
        let mut last_node = last_production_strong.borrow_mut();
        let token = self.tokens.next();
        match token {
            Some(t) => {
                for kind in kinds.clone() {
                    if t.is_like(kind) {
                        last_node.children.push(NodeEnum::Terminal(&t));
                        return;
                    }
                }
                self.add_error(ParseError {
                    token_found: Some(t),
                    expected_kinds: kinds,
                })
            }

            // maybe change this panic to an error message
            None => self.add_error(ParseError {
                token_found: None,
                expected_kinds: kinds,
            }),
        }
    }

    /*
     * PARSING GRAMMAR do_GRAMMAR_RULE
     *
     */
    fn do_program(&mut self) {
        //  rule of program already done during init
        self.do_block();
        // the program ensures that there is only one end program
        //    in the token slice so there is not need to esnure that
        //    the terminal (tokens) have been exhuasted here
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
            TokenKind::Id(Id { name: 'X' }),
            TokenKind::Keyword(Keyword::Int),
            TokenKind::Keyword(Keyword::Boolean),
            TokenKind::Keyword(Keyword::String),
            TokenKind::Keyword(Keyword::LoopOnTrue),
            TokenKind::Keyword(Keyword::If),
        ];
        let next_token = match self.tokens.peek() {
            Some(t) => t,
            None => {
                self.add_error(ParseError {
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
            TokenKind::Id(Id { name: 'X' }),
            TokenKind::Keyword(Keyword::Int),
            TokenKind::Keyword(Keyword::Boolean),
            TokenKind::Keyword(Keyword::String),
            TokenKind::Keyword(Keyword::LoopOnTrue),
            TokenKind::Keyword(Keyword::If),
        ];
        let next_token = match self.tokens.peek() {
            Some(t) => t,
            None => {
                self.add_error(ParseError {
                    token_found: None,
                    expected_kinds,
                });
                return;
            }
        };
        if next_token.is_like(TokenKind::Keyword(Keyword::Print)) {
            self.do_print_statement();
            // the
        } else if next_token.is_like(TokenKind::Id(Id { name: 'X' })) {
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
            let cloned_reference = next_token.clone();
            self.add_error(ParseError {
                token_found: Some(cloned_reference),
                expected_kinds,
            });
        }
        self.up_root();
    }

    fn do_print_statement(&mut self) {
        self.add_production("Print Statement".to_string());
        self.match_kind(vec![TokenKind::Keyword(Keyword::Print)]);
        self.match_kind(vec![TokenKind::Symbol(Symbol::OpenParenthesis)]);
        self.do_expr();
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
        self.match_kind(vec![TokenKind::Keyword(Keyword::LoopOnTrue)]);
        self.do_boolean_expr();
        self.do_block();
        self.up_root();
    }

    fn do_if_statement(&mut self) {
        self.add_production("If Statement".to_string());
        self.match_kind(vec![TokenKind::Keyword(Keyword::If)]);
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
            TokenKind::Id(Id { name: 'X' }),
        ];

        let next_token = match self.tokens.peek() {
            Some(t) => t,
            None => {
                self.add_error(ParseError {
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
        } else if next_token.is_like(TokenKind::Id(Id { name: 'X' })) {
            self.do_id();
        } else {
            let cloned_reference = next_token.clone();
            self.add_error(ParseError {
                token_found: Some(cloned_reference),
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
            TokenKind::Id(Id { name: 'X' }),
            TokenKind::Keyword(Keyword::Int),
            TokenKind::Keyword(Keyword::Boolean),
            TokenKind::Keyword(Keyword::String),
            TokenKind::Keyword(Keyword::LoopOnTrue),
            TokenKind::Keyword(Keyword::If),
        ];
        let next_token = match self.tokens.peek() {
            Some(t) => t,
            None => {
                self.add_error(ParseError {
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
                let cloned_reference = next_token.clone();
                self.add_error(ParseError {
                    token_found: Some(cloned_reference),
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
                self.add_error(ParseError {
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
        self.match_kind(vec![TokenKind::Id(Id { name: 'X' })]);
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
                self.add_error(ParseError {
                    token_found: None,
                    expected_kinds,
                });
                return;
            }
        };

        if next_token.is_like(TokenKind::Char(Char { letter: 'X' })) {
            self.do_char();
            self.do_char_list();
        } else if !next_token.is_like(TokenKind::Symbol(Symbol::QuotatioinMark)) {
            let cloned_reference = next_token.clone();
            self.add_error(ParseError {
                token_found: Some(cloned_reference),
                expected_kinds,
            });
            return;
        }
        self.up_root();
    }

    fn do_char(&mut self) {
        self.add_production("Char".to_string());
        self.match_kind(vec![TokenKind::Char(Char { letter: 'X' })]);
        self.up_root();
    }
}

// I do not really have a programatic way to test parse test unless
//    I were to hardcode the cst tree and like... no
#[cfg(test)]
mod parse_tests {
    use super::*;
    use crate::lex::*;
    use std::path::Path;

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
        cst.show_parse_steps();
    }

    #[test]
    fn parse_with_spaces() {
        // file: {}$
        let path = Path::new("test_cases/general/lex-with-spaces");
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
        cst.show_parse_steps();
    }
}

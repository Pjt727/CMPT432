use std::mem::discriminant;

pub enum Keyword {
    LoopOnTrue,
    If,
    Boolean,
    String,
    Int,
    True,
    False,
    Print,
}

pub mod keyword_mappings {
    pub const LOOP_ON_TRUE: &str = "while";
    pub const IF: &str = "if";
    pub const BOOLEAN: &str = "boolean";
    pub const INT: &str = "int";
    pub const STRING: &str = "string";
    pub const TRUE: &str = "true";
    pub const FALSE: &str = "false";
    pub const PRINT: &str = "print";
}

// will read from these later
#[allow(dead_code)]
pub struct Id {
    pub name: char,
}

pub enum Symbol {
    OpenBlock,
    CloseBlock,
    OpenParenthesis,
    CloseParenthesis,
    QuotatioinMark,
    Assignment,
    CheckEquality,
    CheckInequality,
    Addition,
    EndProgram,
}

pub mod symbol_mappings {
    pub const OPEN_BLOCK: &str = "{";
    pub const CLOSE_BLOCK: &str = "}";
    pub const OPEN_PARENTHESIS: &str = "(";
    pub const CLOSE_PARENTHESIS: &str = ")";
    pub const QUOTATION_MARK: &str = "\"";
    pub const ASSIGNMENT: &str = "=";
    pub const CHECK_EQUALITY: &str = "==";
    pub const CHECK_INEQUALITY: &str = "!=";
    pub const ADDITION: &str = "+";
    pub const END_PROGRAM: &str = "$";
}

// will read from these later
#[allow(dead_code)]
pub struct Digit {
    pub value: u8,
}

// will read from these later
#[allow(dead_code)]
pub struct Char {
    pub letter: char,
}

pub enum TokenKind {
    Keyword(Keyword),
    Id(Id),
    Symbol(Symbol),
    Digit(Digit),
    Char(Char),
}

pub struct Token {
    pub kind: TokenKind,
    pub start_end_position: (i32, i32),
    // Tokens can only be 1 line as \n acts as fold for everything
    pub line: i32,
    pub representation: String,
}

impl Token {
    // matching enum variants correctly with depth when needed
    pub fn is_like(&self, kind: TokenKind) -> bool {
        match (&self.kind, kind) {
            (TokenKind::Keyword(k1), TokenKind::Keyword(k2)) => {
                discriminant(k1) == discriminant(&k2)
            }
            (TokenKind::Id(_), TokenKind::Id(_)) => true,
            (TokenKind::Symbol(k1), TokenKind::Symbol(k2)) => discriminant(k1) == discriminant(&k2),
            (TokenKind::Digit(_), TokenKind::Digit(_)) => true,
            (TokenKind::Char(_), TokenKind::Char(_)) => true,
            _ => false,
        }
    }
}

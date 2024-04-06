use std::mem::discriminant;

#[derive(Clone)]
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

#[derive(Clone)]
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
//
// Gets the class name of variable
// Can change toggle to only include last n classes
//    or to match enums if it does not work well
pub fn get_token_verbose_name(token: &TokenKind) -> &str {
    match token {
        TokenKind::Keyword(keyword) => match keyword {
            Keyword::LoopOnTrue => "LOOP_ON_TRUE",
            Keyword::If => "IF_TRUE_DO",
            Keyword::String => "STRING_TYPE",
            Keyword::Boolean => "TYPE_BOOL",
            Keyword::Int => "INT_TYPE",
            Keyword::True => "LITERAL_TRUE",
            Keyword::False => "LITERAL_FALSE",
            Keyword::Print => "PRINT",
        },
        TokenKind::Id(_) => "IDENTIFIER",
        TokenKind::Symbol(symbol) => match symbol {
            Symbol::OpenBlock => "BLOCK_OPEN",
            Symbol::CloseBlock => "BLOCK_CLOSE",
            Symbol::OpenParenthesis => "PARENTHESIS_OPEN",
            Symbol::CloseParenthesis => "PARENTHESIS_CLOSE",
            Symbol::QuotatioinMark => "QUOTATION_MARK",
            Symbol::Assignment => "ASSIGNMENT",
            Symbol::CheckEquality => "CHECK_EQUALITY",
            Symbol::CheckInequality => "CHECK_INEQUALITY",
            Symbol::Addition => "OPERATOR_ADDITION",
            Symbol::EndProgram => "END_OF_PROGRAM",
        },
        TokenKind::Digit(_) => "LITERAL_DIGIT",
        TokenKind::Char(_) => "LITERAL_CHAR",
    }
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

impl Clone for TokenKind {
    fn clone(&self) -> Self {
        match self {
            TokenKind::Keyword(k) => TokenKind::Keyword(k.clone()),
            TokenKind::Id(id) => TokenKind::Id(Id { name: id.name }),
            TokenKind::Symbol(s) => TokenKind::Symbol(s.clone()),
            TokenKind::Digit(d) => TokenKind::Digit(Digit { value: d.value }),
            TokenKind::Char(c) => TokenKind::Char(Char { letter: c.letter }),
        }
    }
}

#[derive(Clone)]
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

    pub fn get_position(&self) -> String {
        let character_rep;

        // I want to print ranges of position only if they are not
        //    1 char long
        let (start_pos, end_pos) = self.start_end_position;
        if start_pos == end_pos - 1 {
            character_rep = format!("{}", start_pos);
        } else {
            character_rep = format!("{}-{}", start_pos, end_pos,)
        }
        return format!("{}:{}", self.line, character_rep)
    }
}

pub enum Keyword {
    LoopOnTrue,
    If,
    Boolean,
    Int,
    True,
    False,
    Print,
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



// depending on how I want to implement future tokens for the other steps
//    I may put the token declaration in a different file
pub struct Token {
    pub kind: TokenKind,
    pub start_end_position: (i32, i32),
    // Tokens can only be 1 line as \n acts as fold for everything
    pub line: i32,
    pub representation: String,
}

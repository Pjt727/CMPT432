// 1. keyword
// 2. id
// 3. symbol
// 4. digit
// 5. char

use regex::Regex;
use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};

enum Keyword {
    LoopOnTrue,
    If,
    Boolean,
    Int,
    True,
    False,
    Print,
}

struct Id {
    name: char,
}

enum Symbol {
    OpenBlock,
    CloseBlock,
    OpenParenthesis,
    CloseParenthesis,
    QuotatioinMark,
    Assignment,
    CheckEqaulity,
    CheckIneqaulity,
    Addition,
    EndProgram,
}

struct Digit {
    value: u8,
}

struct Char {
    letter: char,
}

enum TokenKind {
    Keyword(Keyword),
    Id(Id),
    Symbol(Symbol),
    Digit(Digit),
    Char(Char),
}

struct Token {
    kind: TokenKind,
}

// non char, id, and number range are single char that can use regex to
// or other range techniques to discover
// It also seems pointless to use regex here
fn get_non_range_token(buffer: &str) -> Option<Token> {
    match buffer {
        "boolean" => Some(Token {
            kind: TokenKind::Keyword(Keyword::Boolean),
        }),
        "while" => Some(Token {
            kind: TokenKind::Keyword(Keyword::LoopOnTrue),
        }),
        "print" => Some(Token {
            kind: TokenKind::Keyword(Keyword::Print),
        }),
        "false" => Some(Token {
            kind: TokenKind::Keyword(Keyword::False),
        }),
        "true" => Some(Token {
            kind: TokenKind::Keyword(Keyword::True),
        }),
        "int" => Some(Token {
            kind: TokenKind::Keyword(Keyword::Int),
        }),
        "if" => Some(Token {
            kind: TokenKind::Keyword(Keyword::If),
        }),
        "==" => Some(Token {
            kind: TokenKind::Symbol(Symbol::CheckEqaulity),
        }),
        "!=" => Some(Token {
            kind: TokenKind::Symbol(Symbol::CheckIneqaulity),
        }),
        "=" => Some(Token {
            kind: TokenKind::Symbol(Symbol::Assignment),
        }),
        "(" => Some(Token {
            kind: TokenKind::Symbol(Symbol::OpenParenthesis),
        }),
        ")" => Some(Token {
            kind: TokenKind::Symbol(Symbol::CloseParenthesis),
        }),
        "{" => Some(Token {
            kind: TokenKind::Symbol(Symbol::OpenBlock),
        }),
        "}" => Some(Token {
            kind: TokenKind::Symbol(Symbol::CloseBlock),
        }),
        _ => None,
    }
}
struct TokenStream {
    token: Token,
    next_token: Box<Option<TokenStream>>,
}

fn fold(token_stream: TokenStream, start_position: i32) {}

fn lex_file(path: &Path) -> Option<TokenStream> {
    let file = File::open(path).expect(&format!("Failed to open file, {}", path.to_string_lossy()));
    let reader = BufReader::new(file);

    const SYMBOL_MAX_SIZE: i32 = 2;

    // we gotta use regex for something
    let in_alphabet: Regex = Regex::new("a-z0-9{}[]!=").unwrap();
    let mut in_string = false;
    let mut last_is_symbol = false;
    let mut line_number = 1;
    // set to negative to propely increment at start 0 since we preincrement
    let mut start_char_position = -1;
    let mut current_char_position = 0;
    let mut buffer = String::from("");
    let token_stream = Default::default();
    for line in reader.lines() {
        // poor preformance for insanely long single line files
        // can choose other deliminators
        let line = line.expect("Unexpected File Reading Error");
        for c in line.chars() {
            current_char_position += 1;
            // Character processing
            if (c >= 'a' && c <= 'z') && !last_is_symbol {
                // do nothing because character followed by character
            } else if c == '"' {
                // fold
                in_string = !in_string;
                // Hey! isn't this technically a bit of parsing?
                // ----- open satire -----
                // Where do we draw the line?
                // How far do we go?
                // Do we return full string token?
                // Do we return full blocks together with recursive types?
                // Do we return just return the full CST?
                // Do we just return the AST?
                // Do we just return the generated code?
                // Do we just execute the result of the program and return that?
                // Do we just sense the intent of the programmer by reading their mind
                //  and return the output?
                // ----- close satire -----

                // if buffer is currently string then fold bc it reached symbol
                //    or if symbol litmit has exceded
            } else if !last_is_symbol
                || ((start_char_position - current_char_position) > SYMBOL_MAX_SIZE)
            {
                // fold
            }
            buffer.push(c);
        }
        line_number += 1;
        current_char_position = 0;
    }
    // Need to fold for end of file
    // fold
    return token_stream;
}

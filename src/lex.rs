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

/* recursively exhausts the string buffer
// has side affects of emptying the string buffer*** and adding the tokens to the stream
// When this function is called everything in the buffer is expected to be a valid token
//    and the end of the current buffer is interpretted as the place to stop and fold
//
// EXAMPLE:
// buffer -> "inta"
// Loops through "inta" char by char possibly updating the token to add
//    stops once it reaches the end.
// After the end the int token is added and then fold is called with
//    buffer -> "a"
//
// You may think that this might lead to erros on edge cases but it does not
//
// SPECIAL CASE****:
// The buffer string may not be fully emptied yet return when there is a
//    single unknown char left (perhaps more generally when SYMBOL_MAX_SIZE - 1
//    chars are left).
// We know that when this function is called at least 1 token must be present (unless there is an
//      unknown token)
// However, we may get an unknown token when exhuasting a 2 symbol buffer
// buffer -> "=!", full_text = "=!="
// = is a token
// =! is not a token
// = is added to the token stream
// ! is not a valid token NOT reporting but skipping for now
// Cannot assert that ! is unknown because it may be followed by an equal sign
// Note that if this is the first call from the lex file we CAN assume the fold has a token
// buffer -> "!{"
// ! is not a token
// !{ is not a token
// ! is not a valid token reporting and skipping (no recursive call)
*/
fn fold(buffer: &mut String, start_position: i32, token_stream: &TokenStream) {}

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

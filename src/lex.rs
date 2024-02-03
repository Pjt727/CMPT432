// 1. keyword
// 2. id
// 3. symbol
// 4. digit
// 5. char

use regex::Regex;
use std::{
    fs::File,
    i32,
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

struct Error {
    character: char,
}

enum TokenKind {
    Keyword(Keyword),
    Id(Id),
    Symbol(Symbol),
    Digit(Digit),
    Char(Char),
    Error(Error),
}

struct Token {
    kind: TokenKind,
    start_position: i32,
    end_position: i32,
}

// non char, id, and number range are single char that can use regex to
// or other range techniques to discover
// It also seems pointless to use regex here
// Not sure what optimizations rust does during compilation
//    but more match checks could only do ones of the correct
//    str length
fn get_non_range_token(buffer: &str, start_position: i32, end_position: i32) -> Option<Token> {
    match buffer {
        "boolean" => Some(Token {
            kind: TokenKind::Keyword(Keyword::Boolean),
            start_position,
            end_position,
        }),
        "while" => Some(Token {
            kind: TokenKind::Keyword(Keyword::LoopOnTrue),
            start_position,
            end_position,
        }),
        "print" => Some(Token {
            kind: TokenKind::Keyword(Keyword::Print),
            start_position,
            end_position,
        }),
        "false" => Some(Token {
            kind: TokenKind::Keyword(Keyword::False),
            start_position,
            end_position,
        }),
        "true" => Some(Token {
            kind: TokenKind::Keyword(Keyword::True),
            start_position,
            end_position,
        }),
        "int" => Some(Token {
            kind: TokenKind::Keyword(Keyword::Int),
            start_position,
            end_position,
        }),
        "if" => Some(Token {
            kind: TokenKind::Keyword(Keyword::If),
            start_position,
            end_position,
        }),
        "==" => Some(Token {
            kind: TokenKind::Symbol(Symbol::CheckEqaulity),
            start_position,
            end_position,
        }),
        "!=" => Some(Token {
            kind: TokenKind::Symbol(Symbol::CheckIneqaulity),
            start_position,
            end_position,
        }),
        "=" => Some(Token {
            kind: TokenKind::Symbol(Symbol::Assignment),
            start_position,
            end_position,
        }),
        "(" => Some(Token {
            kind: TokenKind::Symbol(Symbol::OpenParenthesis),
            start_position,
            end_position,
        }),
        ")" => Some(Token {
            kind: TokenKind::Symbol(Symbol::CloseParenthesis),
            start_position,
            end_position,
        }),
        "{" => Some(Token {
            kind: TokenKind::Symbol(Symbol::OpenBlock),
            start_position,
            end_position,
        }),
        "}" => Some(Token {
            kind: TokenKind::Symbol(Symbol::CloseBlock),
            start_position,
            end_position,
        }),
        _ => None,
    }
}

struct TokenStream {
    token: Token,
    next_token: Box<Option<&TokenStream>>,
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
fn fold(
    buffer: &mut String,
    start_position: i32,
    mut token_stream: TokenStream,
    is_recursive_call: bool,
) -> TokenStream {
    let mut longest_token: Option<Token> = None;
    let mut longest_token_length;
    for i in 1..buffer.len() {
        let end_position = start_position + i as i32;
        match get_non_range_token(&buffer[0..i], start_position, end_position) {
            Some(token) => {
                longest_token = Some(token);
                longest_token_length = i;
            }
            None => {}
        }
    }
    match longest_token {
        Some(token) => {
            let new_token_stream = TokenStream {
                token,
                next_token: Box::new(None),
            };
            token_stream.next_token = Box::new(Some(&new_token_stream));
            let buffer_end_pos = (token.end_position - token.start_position) as usize;
            *buffer = buffer[buffer_end_pos..buffer.len()].to_string();
            return fold(buffer, token.end_position, new_token_stream, true);
        }
        None => {
            if !is_recursive_call {
                // Because of the nature of the langauge
                //    lex error tokens can only be a single char.
                let new_err_token;
                if let Some(first_char) = buffer.chars().next() {
                    new_err_token = Token {
                        kind: TokenKind::Error(Error {
                            character: first_char,
                        }),
                        start_position,
                        end_position: start_position + 1,
                    };
                } else {
                    panic!("Unexpected empty string!");
                }
                let new_token_stream = TokenStream {
                    token: new_err_token,
                    next_token: Box::new(None),
                };
                token_stream.next_token = Box::new(Some(&new_token_stream));
                *buffer = buffer[1..buffer.len()].to_string();
                return fold(buffer, start_position + 1, new_token_stream, true);
            }
            return token_stream;
        }
    }
}

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

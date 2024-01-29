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

struct Token {}

fn get_token(buffer: &String, in_string: bool) -> Option<Token> {
    match buffer {
        // MY OTHER CONDITIONS
        _ => None,
    }
}
struct TokenStream {
    token: Token,
    next_token: Box<Option<TokenStream>>,
}

fn lex_file(path: &Path) -> Option<TokenStream> {
    let file = File::open(path).expect(&format!("Failed to open file, {}", path.to_string_lossy()));
    let reader = BufReader::new(file);

    let in_alphabet: Regex = Regex::new("a-z0-9{}[]!=").unwrap();
    let mut line_number = 1;
    let mut current_char_position = 0;
    let mut buffer = String::from("");
    let token_stream = Default::default();
    for line in reader.lines() {
        let line = line.expect("Unexpected File Reading Error");
        for c in line.chars() {
            current_char_position += 1;
            // Character processing
            if c.is_ascii_lowercase() {
                buffer.push(c);
                continue;
            }
            new_token = get_token(&buffer, true);
            buffer.push(c);
        }
        line_number += 1;
        current_char_position = 0;
    }
    return token_stream;
}

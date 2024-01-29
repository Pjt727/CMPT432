// 1. keyword
// 2. id
// 3. symbol
// 4. digit
// 5. char

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
struct TokenStream {
    token: Token,
    next_token: Box<Option<TokenStream>>,
}

fn lex_file(path: &Path) -> Option<TokenStream> {
    let file = File::open(path).expect(&format!("Failed to open file, {}", path.to_string_lossy()));
    let reader = BufReader::new(file);
    let mut line_number = 1;
    let mut current_char_position = 0;
    let token_stream = Default::default();
    for line in reader.lines() {
        let line = line.expect("Unexpected File Reading Error");
        for c in line.chars() {
            current_char_position += 1;
            // Character processing
            println!(
                "Character: {}, Line: {}, Position: {}",
                c, line_number, current_char_position
            );
        }
        line_number += 1;
        current_char_position = 0;
    }
    return token_stream;
}

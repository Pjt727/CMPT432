// SO SAD I DIDNT USE REGEX, but I really dont think it's the right way
//    to go for this project
// use regex::Regex;
use colored::Colorize;
use std::any::type_name;
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
    CheckEquality,
    CheckInequality,
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

enum LexError {
    InvalidChar(char),
    MissingEndProgram,
}

pub struct Token {
    kind: Result<TokenKind, LexError>,
    start_end_position: (i32, i32),
    // Tokens can only be 1 line as \n acts as fold for everything
    line: i32,
    representation: String,
}

// Not sure what optimizations rust does during compilation
//    but more match checks could only do ones of the correct
//    str length
fn get_token(
    buffer: &str,
    start_end_position: (i32, i32),
    line: i32,
    in_string: bool,
) -> Option<Token> {
    if buffer.len() == 1 {
        let character = buffer.chars().next().unwrap();
        if character >= 'a' && character <= 'z' {
            if in_string {
                return Some(Token {
                    kind: Ok(TokenKind::Char(Char { letter: character })),
                    start_end_position,
                    line,
                    representation: buffer.to_string(),
                });
            }
            return Some(Token {
                kind: Ok(TokenKind::Id(Id { name: character })),
                start_end_position,
                line,
                representation: buffer.to_string(),
            });
        } else if character >= '0' && character <= '9' {
            return Some(Token {
                kind: Ok(TokenKind::Digit(Digit {
                    value: character as u8,
                })),
                start_end_position,
                line,
                representation: buffer.to_string(),
            });
        }
    }

    match buffer {
        "boolean" => Some(Token {
            kind: Ok(TokenKind::Keyword(Keyword::Boolean)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        "while" => Some(Token {
            kind: Ok(TokenKind::Keyword(Keyword::LoopOnTrue)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        "print" => Some(Token {
            kind: Ok(TokenKind::Keyword(Keyword::Print)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        "false" => Some(Token {
            kind: Ok(TokenKind::Keyword(Keyword::False)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        "true" => Some(Token {
            kind: Ok(TokenKind::Keyword(Keyword::True)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        "int" => Some(Token {
            kind: Ok(TokenKind::Keyword(Keyword::Int)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        "if" => Some(Token {
            kind: Ok(TokenKind::Keyword(Keyword::If)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        "==" => Some(Token {
            kind: Ok(TokenKind::Symbol(Symbol::CheckEquality)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        "!=" => Some(Token {
            kind: Ok(TokenKind::Symbol(Symbol::CheckInequality)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        "$" => Some(Token {
            kind: Ok(TokenKind::Symbol(Symbol::EndProgram)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        "=" => Some(Token {
            kind: Ok(TokenKind::Symbol(Symbol::Assignment)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        "+" => Some(Token {
            kind: Ok(TokenKind::Symbol(Symbol::Addition)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        "\"" => Some(Token {
            kind: Ok(TokenKind::Symbol(Symbol::QuotatioinMark)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        "(" => Some(Token {
            kind: Ok(TokenKind::Symbol(Symbol::OpenParenthesis)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        ")" => Some(Token {
            kind: Ok(TokenKind::Symbol(Symbol::CloseParenthesis)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        "{" => Some(Token {
            kind: Ok(TokenKind::Symbol(Symbol::OpenBlock)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        "}" => Some(Token {
            kind: Ok(TokenKind::Symbol(Symbol::CloseBlock)),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        _ => None,
    }
}

/* recursively exhausts the string buffer
// has side affects of emptying the string buffer*** and adding the tokens to the stream
// When this function is called everything in the buffer is expected to be a valid token
//    and the end of the current buffer is interpreted as the place to stop and fold
//
// EXAMPLE:
// buffer -> "inta"
// Loops through "inta" char by char possibly updating the token to add
//    stops once it reaches the end.
// After the end the int token is added and then fold is called with
//    buffer -> "a"
//
// You may think that this might lead to errors on edge cases but it does not
//
// SPECIAL CASE****:
// The buffer string may not be fully emptied yet return when there is a
//    single unknown char left (perhaps more generally when SYMBOL_MAX_SIZE - 1
//    chars are left).
// We know that when this function is called at least 1 token must be present (unless there is an
//      unknown token)
// However, we may get an unknown token when exhausting a 2 symbol buffer
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
    line: i32,
    start_position: i32,
    token_stream: &mut Vec<Token>,
    in_string: bool,
    is_recursive_call: bool,
) -> i32 {
    let mut longest_token: Option<Token> = None;
    for i in 1..buffer.len() {
        let end_position = start_position + i as i32;
        match get_token(
            &buffer[0..i],
            (start_position, end_position),
            line,
            in_string,
        ) {
            Some(token) => {
                longest_token = Some(token);
            }
            None => {}
        }
    }
    match longest_token {
        Some(token) => {
            // need to the the position first bc the object will be moved
            let token_end_position = token.start_end_position.1;
            let buffer_end_pos = (token_end_position - token.start_end_position.0) as usize;
            token_stream.push(token);
            *buffer = buffer[buffer_end_pos..buffer.len()].to_string();
            return fold(
                buffer,
                line,
                token_end_position,
                token_stream,
                in_string,
                true,
            );
        }
        None => {
            /*
            Bc of how the buffers are formed there can
              be a case where a recursive call incorrectly tries
              to produce and error token
              EX:
              =!=
              The buffer would be =!
            */
            if !is_recursive_call {
                // Because of the nature of the language
                //    lex error tokens can only be a single char.
                let new_err_token;
                if let Some(first_char) = buffer.chars().next() {
                    new_err_token = Token {
                        kind: Err(LexError::InvalidChar(first_char)),
                        start_end_position: (start_position, start_position + 1),
                        line,
                        representation: first_char.to_string(),
                    };
                } else {
                    panic!("Unexpected empty string!");
                }
                token_stream.push(new_err_token);
                *buffer = buffer[1..buffer.len()].to_string();
                return fold(
                    buffer,
                    line,
                    start_position + 1,
                    token_stream,
                    in_string,
                    true,
                );
            }
            return start_position;
        }
    }
}

// Gets the class name of variable
// Can change toggle to only include last n classes
//    or to match enums if it does not work well
fn get_class_name<T>(obj: &T) -> &str {
    return type_name::<T>();
}

// processes a single token to the standard output
// Returns true if tokenkind is OK
// Returns false if tokenkind is err
fn process_lexeme(token: &Token) -> bool {
    static INFO_TEXT: &str = "DEBUG INFO lex: ";
    static ERROR_TEXT: &str = "DEBUG ERROR lex: ";

    match &token.kind {
        Ok(kind) => {
            let position_rep;
            if token.start_end_position.0 == token.start_end_position.1 {
                position_rep = format!("{}", token.start_end_position.0);
            } else {
                position_rep = format!(
                    "{}-{}",
                    token.start_end_position.0, token.start_end_position.1,
                )
            }
            println!(
                "{} - {} [ {} ] found at ({}:{})",
                INFO_TEXT.color("grey"),
                get_class_name(&kind),
                token.representation,
                token.line,
                position_rep
            );
            return true;
        }
        Err(kind) => {
            // errors may not have representations
            // might be better form to have this as an option or representation
            //    part of a different class so errors don't need it but
            //    it is what it is
            let token_representation;
            if token.representation.len() == 0 {
                token_representation = "".to_string();
            } else {
                token_representation = format!(" [ {} ]", token.representation)
            }
            println!(
                "{} - {}{} found at ({}:{})",
                ERROR_TEXT.red(),
                get_class_name(&kind),
                token_representation,
                token.line,
                token.start_end_position.0 // errors have no pos or just 1 char
            );
            return false;
        }
    }
}

// Processes multiple tokens
// Returns true if all tokenkind is OK
// Returns false if any tokenkind is err
fn process_lexemes(tokens: &Vec<Token>) -> bool {
    let mut is_ok = true;
    for token in tokens {
        is_ok = is_ok && process_lexeme(token);
    }
    return is_ok;
}

pub fn get_lexemes(path: &Path) -> Vec<Token> {
    let file = File::open(path).expect(&format!("Failed to open file, {}", path.to_string_lossy()));
    let reader = BufReader::new(file);

    const SYMBOL_MAX_SIZE: i32 = 2;

    let mut in_string = false;
    let mut in_comment = true;
    let mut last_is_symbol = false;
    let mut line_number = 1;
    // set to negative to properly increment at start 0 since we preincrement
    let mut start_char_position = -1;
    let mut current_char_position = 0;
    let mut buffer = String::from("");
    let mut token_stream: Vec<Token> = Vec::new();
    for line in reader.lines() {
        // poor performance for insanely long single line files
        // can choose other deliminators
        let line = line.expect("Unexpected File Reading Error");
        for c in line.chars() {
            current_char_position += 1;
            // first check for comments
            // put this here bc comments dont generate tokens
            if !in_string && buffer == "/*" {
                in_comment = true;
                // hack to make buffer always 2 chars for buffer in comment
                //    since we want a sliding window of two chars
                buffer = "X".to_string();
                buffer.push(c);
                continue;
            } else if in_comment {
                // move sliding window one to the right
                buffer = buffer.chars().skip(1).collect();
                buffer.push(c);
                // can check after push bc */ will never match on first chase bc
                //     of dummy char put at start of comment
                if buffer == "*/" {
                    in_comment = false;
                    buffer = "";
                }
                continue;
            }

            let is_next_alpha = c >= 'a' && c <= 'z';
            // Character processing
            if is_next_alpha && !last_is_symbol {
                // do nothing because character followed by character
            } else if c == '"' {
                start_char_position = fold(
                    &mut buffer,
                    line_number,
                    start_char_position,
                    &mut token_stream,
                    in_string,
                    false,
                );
                in_string = !in_string;
                /*
                Hey! isn't this technically a bit of parsing?
                ----- open satire -----
                Where do we draw the line?
                How far do we go?
                Do we return full string token?
                Do we return full blocks together with recursive types?
                Do we return just return the full CST?
                Do we just return the AST?
                Do we just return the generated code?
                Do we just execute the result of the program and return that?
                Do we just sense the intent of the programmer by reading their mind
                 and return the output?
                ----- close satire -----
                */

                // if buffer is currently string then fold bc it reached symbol
                //    or if symbol limit has exceeded
            } else if !last_is_symbol
                || ((start_char_position - current_char_position) > SYMBOL_MAX_SIZE)
            {
                start_char_position = fold(
                    &mut buffer,
                    line_number,
                    start_char_position,
                    &mut token_stream,
                    in_string,
                    false,
                );
            }
            if is_next_alpha {
                last_is_symbol = false;
            } else {
                last_is_symbol = true;
            }
            buffer.push(c);
        }
        line_number += 1;
        current_char_position = -1;
    }
    // Need to fold for end of file
    start_char_position = fold(
        &mut buffer,
        line_number,
        start_char_position,
        &mut token_stream,
        in_string,
        false,
    );

    // check if file ends with $
    let mut ends_with_eop = false;
    if let Some(last_token) = token_stream.last() {
        match &last_token.kind {
            Ok(token) => match token {
                TokenKind::Symbol(Symbol::EndProgram) => ends_with_eop = true,
                _ => {}
            },
            Err(_) => {}
        }
    }
    if !ends_with_eop {
        let eop_error_token = (Token {
            kind: Err(LexError::MissingEndProgram),
            start_end_position: (start_char_position, start_char_position),
            line: line_number,
            representation: "$".to_string(),
        });
        token_stream.push(eop_error_token);
    }

    return token_stream;
}

// Unit tests for lex
#[cfg(test)]
mod lex_tests {
    // imports the lex mod as lex_tests is a sub mod
    use super::*;
    use std::path::Path;

    // helper function to determine if token sequences are "like"
    //     another
    // Really just doesnt check positions bc I would be insane if I were
    //     to code that into a my test cases and I also it also
    //     helps for asserting likeness in the lex with/ without spaces
    fn tokens_are_like(tokens1: &Vec<Token>, tokens2: &Vec<Token>) -> bool {
        let mut are_like = true;
        let zipped: Vec<_> = tokens1.iter().zip(tokens2.iter()).collect();
        for (token1, token2) in zipped {
            let are_like = are_like && token1.kind == token2.kind;
        }
        return are_like;
    }

    #[test]
    fn hello_world() {
        let first_path = Path::new("/test_cases/ok/hello-world.txt");
        let tokens = lex_file(first_path);
        process_lexemes(&tokens);
    }
}

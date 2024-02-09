// SO SAD I DIDNT USE REGEX, but I really dont think it's the right way
//    to go for this project
// use regex::Regex;
use colored::Colorize;
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

// will read from these later
#[allow(dead_code)]
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

// will read from these later
#[allow(dead_code)]
struct Digit {
    value: u8,
}

// will read from these later
#[allow(dead_code)]
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
    // in_string only ever processes a single character
    let buffer_is_length_one = buffer.len() == 1;
    if in_string {
        if !buffer_is_length_one {
            return None;
        }
        let character = buffer.chars().next().unwrap();
        if (character >= 'a' && character <= 'z') || character == ' ' {
            return Some(Token {
                kind: Ok(TokenKind::Char(Char { letter: character })),
                start_end_position,
                line,
                representation: buffer.to_string(),
            });
        }
        if character == '\"' {
            return Some(Token {
                kind: Ok(TokenKind::Symbol(Symbol::QuotatioinMark)),
                start_end_position,
                line,
                representation: buffer.to_string(),
            });
        }
        // could maybe just emit the error here to
        //     speed things up since no symbols are allowed to be in quotes
        return None;
    } else if buffer_is_length_one {
        // range matches for the single chars
        let character = buffer.chars().next().unwrap();
        if character >= 'a' && character <= 'z' {
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

    // string matches for all non range words
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
    for end_range in 1..(buffer.len() + 1) {
        // minus 1 bc it is an exclusive range
        let end_position = start_position + end_range as i32;
        match get_token(
            &buffer[0..end_range],
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
            // only for testing purposes do I want to process the token as it is made
            #[cfg(test)]
            {
                process_lexeme(&token);
            }
            // need to get position first bc the object will be moved
            let token_end_position = token.start_end_position.1;
            let buffer_end_pos = token.representation.len();
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
              "=!="
              The buffer of symbol length two would be sent "=!"
              = would be longest recognized token then leaving
              just "!" has the recursive call when cannot be recognized for anything
              even though it forms "!=" in the rest of the text
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
                    // only for testing purposes do I want to process the token as it is made
                    #[cfg(test)]
                    {
                        process_lexeme(&new_err_token);
                    }
                } else {
                    // string was empty so do nothing!!
                    return start_position;
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
fn get_token_verbose_name(token: &TokenKind) -> &str {
    match token {
        TokenKind::Keyword(keyword) => match keyword {
            Keyword::LoopOnTrue => "LOOP_ON_TRUE",
            Keyword::If => "IF_TRUE_DO",
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

fn get_error_verbose_name(err: &LexError) -> &str {
    match err {
        LexError::InvalidChar(_) => "Unrecognized Character",
        LexError::MissingEndProgram => "Missing EOP Symbol",
    }
}

// processes a single token to the standard output
// Returns true if tokenkind is OK
// Returns false if tokenkind is err
fn process_lexeme(token: &Token) -> bool {
    static INFO_TEXT: &str = "DEBUG INFO lex:";
    static ERROR_TEXT: &str = "DEBUG ERROR lex:";

    match &token.kind {
        Ok(kind) => {
            let position_rep;

            // I want to print ranges of position only if they are not
            //    1 char long
            let (start_pos, end_pos) = token.start_end_position;
            if start_pos == end_pos - 1 {
                position_rep = format!("{}", start_pos);
            } else {
                position_rep = format!("{}-{}", start_pos, end_pos,)
            }
            println!(
                "{} - {} [ {} ] found at ({}:{})",
                INFO_TEXT.bold().underline(),
                get_token_verbose_name(&kind),
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
                ERROR_TEXT.bold().underline().red(),
                get_error_verbose_name(&kind),
                token_representation,
                token.line,
                token.start_end_position.0
            );
            return false;
        }
    }
}

// Processes multiple tokens
// Returns true if all tokenkind is Ok up to eop
//    or Returns false if any tokenkind is Err
// Returns the index of the first eop if it exists
pub fn process_lexemes(tokens: &Vec<Token>) -> (bool, Option<usize>) {
    let mut is_ok = true;
    let mut end_size = None;
    for (index, token) in tokens.iter().enumerate() {
        is_ok = is_ok && process_lexeme(token);
        if matches!(token.kind, Ok(TokenKind::Symbol(Symbol::EndProgram))) {
            end_size = Some(index);
            break;
        }
    }
    return (is_ok, end_size);
}

pub fn get_lexemes(path: &Path) -> Vec<Token> {
    let file = File::open(path).expect(&format!("Failed to open file, {}", path.to_string_lossy()));
    let reader = BufReader::new(file);

    const SYMBOL_MAX_SIZE: i32 = 2;
    let allowed_but_skipped: Vec<char> = vec!['\t', ' '];

    let mut in_string = false;
    let mut in_comment = false;
    let mut is_last_alpha = false;
    let mut line_number = 0;
    let mut start_char_position = 0;
    let mut buffer = String::from("");
    let mut token_stream: Vec<Token> = Vec::new();
    for line in reader.lines() {
        // poor performance for insanely long single line files
        // can choose other deliminators
        line_number += 1;
        let line = line.expect("Unexpected File Reading Error");
        for c in line.chars() {
            dbg!(c, &buffer, in_string);
            // first check for comments
            // put here here bc comments dont generate tokens
            if in_comment {
                // move sliding window one to the right
                buffer = buffer.chars().skip(1).collect();
                buffer.push(c);
                // can check after push bc */ will never match on first chase bc
                //     of dummy char put at start of comment
                dbg!(c, &buffer);
                if buffer == "*/" {
                    in_comment = false;
                    buffer = "".to_string();
                }
                continue;
            } else if buffer == "/*" {
                in_comment = true;
                // hack to make buffer always 2 chars for buffer in comment
                //    since we want a sliding window of two chars
                buffer = "X".to_string();
                buffer.push(c);
                continue;
            }

            // in_string processing can fold at each character
            if in_string {
                if c == '"' {
                    in_string = false;
                }
                // push before bc " starts empty
                buffer.push(c);
                dbg!("Folding String");
                start_char_position = fold(
                    &mut buffer,
                    line_number,
                    start_char_position,
                    &mut token_stream,
                    in_string,
                    false,
                );
                continue;
            }

            // regular out of context processing
            let is_next_alpha = c >= 'a' && c <= 'z';
            // not that buffer_size is not always
            //   equal to its range in my interpretation of the language
            //   "! =" should become the not eqaul token "!\n=" should become
            //   an error token token for ! and then and eqauls token
            //   new line white space is treated differently
            let buffer_size = buffer.len();
            // when compiled with optimization I am pretty sure that rust will be able
            //    to tell that when calculating these bools there are no side effects
            //    and thus can hoist them so that the have the possibity of short circuiting
            let switched_type = (is_next_alpha != is_last_alpha) && (buffer_size > 1);
            let execedes_symbol_size = (buffer_size as i32 >= SYMBOL_MAX_SIZE) && !is_last_alpha;
            dbg!(is_next_alpha, execedes_symbol_size, switched_type);
            if switched_type || execedes_symbol_size {
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
                is_last_alpha = is_next_alpha;
            }

            // string processing happens before so it can have differnt rules
            if !allowed_but_skipped.contains(&c) && !in_string {
                if c == '\"' {
                    // I dont flip bools bc i have a discrete path for in_string
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
                    start_char_position = fold(
                        &mut buffer,
                        line_number,
                        start_char_position,
                        &mut token_stream,
                        in_string,
                        false,
                    );
                    in_string = true;
                }
                buffer.push(c);
            } else {
            }
        }
        // \n is a terminator so fold here
        // note this does not reset strings values so multi line strings should be without err
        fold(
            &mut buffer,
            line_number,
            start_char_position,
            &mut token_stream,
            in_string,
            false,
        );
        start_char_position = 0;
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
        let eop_error_token = Token {
            kind: Err(LexError::MissingEndProgram),
            start_end_position: (start_char_position, start_char_position),
            line: line_number,
            representation: "$".to_string(),
        };
        token_stream.push(eop_error_token);
    }

    return token_stream;
}

// Unit tests for lex
#[cfg(test)]
mod lex_tests {
    // imports the lex mod as lex_tests is a sub mod
    use super::*;
    use colored::Colorize;
    use std::path::Path;

    // helper function to get the token from a rep to
    //    make expected tests cases more readable
    fn reps_to_tokens(reps: Vec<&str>) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut in_string = false;

        for rep in reps {
            match get_token(rep, (0, 0), 0, in_string) {
                Some(token) => tokens.push(token),
                None => {
                    let unknown_err_token = Token {
                        kind: Err(LexError::InvalidChar(rep.chars().next().unwrap() as char)),
                        start_end_position: (0, 0),
                        line: 0,
                        representation: "".to_string(),
                    };
                    tokens.push(unknown_err_token);
                }
            }
            if rep == "\"" {
                in_string = !in_string;
            }
        }

        return tokens;
    }

    // helper function to determine if token sequences are "like"
    //     another
    // Really just doesnt check positions bc I would be insane if I were
    //     to code that into a my test cases and I also it also
    //     helps for asserting likeness in the lex with/ without spaces
    fn tokens_are_like(tokens1: &Vec<Token>, tokens2: &Vec<Token>) -> bool {
        static DIF_DOWN: &str = "DIFF ↓ ↓ ↓ ↓ ↓ ↓ ";
        static DIF_UP: &str = "DIFF ↑ ↑ ↑ ↑ ↑ ↑ \n";
        let mut are_like_flag = true;
        let zipped: Vec<_> = tokens1.iter().zip(tokens2.iter()).collect();
        for (token1, token2) in zipped {
            // kinda hacky are to do this but its testing
            //     so who cares???
            match &token1.kind {
                Ok(token_kind1) => match &token2.kind {
                    Ok(token_kind2) => {
                        if !(get_token_verbose_name(&token_kind1)
                            == get_token_verbose_name(&token_kind2))
                        {
                            println!("{}", DIF_DOWN.red());
                            process_lexeme(&token1);
                            process_lexeme(&token2);
                            println!("{}", DIF_UP.red());
                            are_like_flag = false;
                        }
                    }
                    Err(_) => {
                        println!("{}", DIF_DOWN.red());
                        process_lexeme(&token1);
                        process_lexeme(&token2);
                        println!("{}", DIF_UP.red());
                        are_like_flag = false;
                    }
                },
                Err(err1) => match &token2.kind {
                    Ok(_) => {
                        println!("{}", DIF_DOWN.red());
                        process_lexeme(&token1);
                        process_lexeme(&token2);
                        println!("{}", DIF_UP.red());
                        are_like_flag = false;
                    }
                    Err(err2) => {
                        if !(get_error_verbose_name(&err1) == get_error_verbose_name(&err2)) {
                            println!("{}", DIF_DOWN.red());
                            process_lexeme(&token1);
                            process_lexeme(&token2);
                            println!("{}", DIF_UP.red());
                            are_like_flag = false;
                        }
                    }
                },
            };
        }
        if tokens1.len() != tokens2.len() {
            if tokens1.len() > tokens2.len() {
                println!("{}", "FIRST TOKENS GENERATED MORE TOKENS".red());
            } else {
                println!("{}", "SECOND TOKENS GENERATED MORE TOKENS".red());
            }
            return false;
        }
        return are_like_flag;
    }

    #[test]
    fn hello_lex() {
        // file: {}$
        let expected_reps = vec!["{", "}", "$"];
        let expected_tokens = reps_to_tokens(expected_reps);
        let path = Path::new("test_cases/ok/hello-compiler.txt");
        let tokens = get_lexemes(path);
        assert!(tokens_are_like(&expected_tokens, &tokens))
    }

    #[test]
    fn three_symbols() {
        // file: =!=
        let expected_reps = vec!["=", "!="];
        let missing_eop = Token {
            kind: Err(LexError::MissingEndProgram),
            start_end_position: (0, 0),
            line: 0,
            representation: "".to_string(),
        };
        let mut expected_tokens = reps_to_tokens(expected_reps);
        expected_tokens.push(missing_eop);
        let path = Path::new("test_cases/ok/three-symbols.txt");
        let tokens = get_lexemes(path);
        assert!(tokens_are_like(&expected_tokens, &tokens))
    }

    // skip formatting to make expected more readible
    #[rustfmt::skip]
    #[test]
    fn lex_with_and_without_spaces() {
        let expected_reps = vec![
            "{", 
                "int", "a", 
                "int", "b", 
                "a", "=", "0", 
                "b", "=", "0", 
                "while", "(", "a", "!=", "3", ")", "{", 
                    "print", "(", "a", ")",
                    "while", "(", "b", "!=", "3", ")", "{", 
                        "print", "(", "b", ")",
                        "b", "=", "1", "+", "b", 
                        "if", "(", "b", "==", "2", ")", "{", 
                            "print", "(", "\"", 
                                "t", "h", "e", "r", "e", " ",
                                "i", "s", " ",
                                "n", "o", " ", 
                                "s", "p", "o", "o", "n", 
                            "\"", ")",
                        "}", 
                    "}",
                    "b", "=", "0", 
                    "a", "=", "1", "+", "a",
                "}",
            "}", "$"
        ];
        let expected_tokens = reps_to_tokens(expected_reps);
        let path_with_spaces_no_comments = Path::new("test_cases/ok/lex-with-spaces-no-comments.txt");
        let path_with_spaces = Path::new("test_cases/ok/lex-with-spaces.txt");
        let path_without_spaces = Path::new("test_cases/ok/lex-without-spaces.txt");
        let tokens_with_spaces_no_comments = get_lexemes(path_with_spaces);
        let tokens_with_spaces = get_lexemes(path_with_spaces_no_comments);
        let tokens_without_spaces = get_lexemes(path_without_spaces);
        assert!(tokens_are_like(&expected_tokens, &tokens_with_spaces));
        assert!(tokens_are_like(&expected_tokens, &tokens_without_spaces));
        assert!(tokens_are_like(&expected_tokens, &tokens_with_spaces_no_comments));
    }
}

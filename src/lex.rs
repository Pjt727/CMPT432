// SO SAD I DIDNT USE REGEX, but I really dont think it's the right way
//    to go for this project
// use regex::Regex;
use crate::token::*;
use colored::Colorize;
use std::{
    fs::File,
    i32,
    io::{BufRead, BufReader},
    path::Path,
};

pub struct InvalidChar {
    character: char,
    line: i32,
    position: i32,
}

pub enum LexError {
    InvalidChar(InvalidChar),
}

pub enum LexWarning {
    MissingEndProgram,
    MissingCommentClose,
}

pub enum LexProblem {
    LexError(LexError),
    LexWarning(LexWarning),
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
                kind: TokenKind::Char(Char { letter: character }),
                start_end_position,
                line,
                representation: buffer.to_string(),
            });
        }
        if character == symbol_mappings::QUOTATION_MARK.chars().next().unwrap() {
            return Some(Token {
                kind: TokenKind::Symbol(Symbol::QuotatioinMark),
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
                kind: TokenKind::Id(Id { name: character }),
                start_end_position,
                line,
                representation: buffer.to_string(),
            });
        } else if character >= '0' && character <= '9' {
            return Some(Token {
                kind: TokenKind::Digit(Digit {
                    value: character as u8,
                }),
                start_end_position,
                line,
                representation: buffer.to_string(),
            });
        }
    }

    // string matches for all non range words
    match buffer {
        keyword_mappings::BOOLEAN => Some(Token {
            kind: TokenKind::Keyword(Keyword::Boolean),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        keyword_mappings::STRING => Some(Token {
            kind: TokenKind::Keyword(Keyword::String),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        keyword_mappings::LOOP_ON_TRUE => Some(Token {
            kind: TokenKind::Keyword(Keyword::LoopOnTrue),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        keyword_mappings::PRINT => Some(Token {
            kind: TokenKind::Keyword(Keyword::Print),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        keyword_mappings::FALSE => Some(Token {
            kind: TokenKind::Keyword(Keyword::False),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        keyword_mappings::TRUE => Some(Token {
            kind: TokenKind::Keyword(Keyword::True),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        keyword_mappings::INT => Some(Token {
            kind: TokenKind::Keyword(Keyword::Int),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        keyword_mappings::IF => Some(Token {
            kind: TokenKind::Keyword(Keyword::If),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        symbol_mappings::CHECK_EQUALITY => Some(Token {
            kind: TokenKind::Symbol(Symbol::CheckEquality),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        symbol_mappings::CHECK_INEQUALITY => Some(Token {
            kind: TokenKind::Symbol(Symbol::CheckInequality),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        symbol_mappings::END_PROGRAM => Some(Token {
            kind: TokenKind::Symbol(Symbol::EndProgram),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        symbol_mappings::ASSIGNMENT => Some(Token {
            kind: TokenKind::Symbol(Symbol::Assignment),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        symbol_mappings::ADDITION => Some(Token {
            kind: TokenKind::Symbol(Symbol::Addition),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        symbol_mappings::QUOTATION_MARK => Some(Token {
            kind: TokenKind::Symbol(Symbol::QuotatioinMark),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        symbol_mappings::OPEN_PARENTHESIS => Some(Token {
            kind: TokenKind::Symbol(Symbol::OpenParenthesis),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        symbol_mappings::CLOSE_PARENTHESIS => Some(Token {
            kind: TokenKind::Symbol(Symbol::CloseParenthesis),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        symbol_mappings::OPEN_BLOCK => Some(Token {
            kind: TokenKind::Symbol(Symbol::OpenBlock),
            start_end_position,
            line,
            representation: buffer.to_string(),
        }),
        symbol_mappings::CLOSE_BLOCK => Some(Token {
            kind: TokenKind::Symbol(Symbol::CloseBlock),
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
    token_stream: &mut Vec<Result<Token, LexProblem>>,
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
            // need to get position first bc the object will be moved
            let token_end_position = token.start_end_position.1;
            let buffer_end_pos = token.representation.len();
            // to to create this variable so that I can send a reference
            //     to the debug print and then let the token_stream have
            //     ownership
            let token_entry = Ok(token);
            // only for testing purposes do I want to process the token as it is made
            #[cfg(test)]
            {
                process_lexeme(&token_entry);
            }
            token_stream.push(token_entry);
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
                let lex_problem;
                if let Some(first_char) = buffer.chars().next() {
                    lex_problem = Err(LexProblem::LexError(LexError::InvalidChar(InvalidChar {
                        character: first_char,
                        line,
                        position: start_position,
                    })));
                    // only for testing purposes do I want to process the token as it is made
                    #[cfg(test)]
                    {
                        process_lexeme(&lex_problem);
                    }
                } else {
                    // string was empty so do nothing!!
                    return start_position;
                }
                token_stream.push(lex_problem);
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

fn get_error_text(problem: &LexProblem) -> String {
    match problem {
        LexProblem::LexError(LexError::InvalidChar(invalid_char)) => {
            format!(
                "Unrecognized Character [ {} ] at {}-{}",
                invalid_char.character, invalid_char.line, invalid_char.position
            )
        }
        LexProblem::LexWarning(LexWarning::MissingEndProgram) => {
            format!("Missing EOP Symbol at End of Program")
        }
        LexProblem::LexWarning(LexWarning::MissingCommentClose) => {
            format!("Missing Close Comment Symbol")
        }
    }
}

// processes a single token to the standard output
// Returns true if tokenkind is OK
// Returns false if tokenkind is err
fn process_lexeme(token_entry: &Result<Token, LexProblem>) {
    static INFO_TEXT: &str = "DEBUG INFO lex:";
    static ERROR_TEXT: &str = "DEBUG ERROR lex:";
    static WARNING_TEXT: &str = "DEBUG WARNING lex:";

    match &token_entry {
        Ok(token) => {
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
                INFO_TEXT.cyan(),
                get_token_verbose_name(&token.kind),
                token.representation,
                token.line,
                position_rep
            );
        }
        Err(lex_problem) => match lex_problem {
            LexProblem::LexError(_) => {
                println!("{} - {}", ERROR_TEXT.red(), get_error_text(&lex_problem),);
            }
            LexProblem::LexWarning(_) => {
                println!(
                    "{} - {}",
                    WARNING_TEXT.yellow(),
                    get_error_text(&lex_problem),
                );
            }
        },
    }
}

// Processes multiple tokens
// Returns a tuple of lex errors and lex warnings up to the end
//    could make this into a another type to make it more obvious
// Returns the index of the first eop if it exists
// Needs the generic tomfoolery to allow for vector slicing
pub fn process_lexemes<'a, T>(token_entries: T) -> ((i32, i32), Option<usize>)
where
    T: Iterator<Item = &'a Result<Token, LexProblem>>,
{
    let mut end_process = None;
    let mut errors_and_warnings = (0, 0);
    for (index, token_entry) in token_entries.enumerate() {
        process_lexeme(&token_entry);
        match token_entry {
            Ok(token) => {
                if matches!(token.kind, TokenKind::Symbol(Symbol::EndProgram)) {
                    end_process = Some(index);
                    break;
                }
            }
            Err(lex_problem) => match lex_problem {
                LexProblem::LexError(_) => errors_and_warnings.0 += 1,
                LexProblem::LexWarning(_) => errors_and_warnings.1 += 1,
            },
        }
    }
    return (errors_and_warnings, end_process);
}

pub fn get_lexemes(path: &Path) -> Vec<Result<Token, LexProblem>> {
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
    let mut token_stream: Vec<Result<Token, LexProblem>> = Vec::new();
    for line in reader.lines() {
        // poor performance for insanely long single line files
        // can choose other deliminators
        line_number += 1;
        let line = line.expect("Unexpected File Reading Error");
        for c in line.chars() {
            // first check for comments
            // put here here bc comments dont generate tokens
            if in_comment {
                // move sliding window one to the right
                buffer = buffer.chars().skip(1).collect();
                buffer.push(c);
                // can check after push bc */ will never match on first chase bc
                //     of dummy char put at start of comment
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
            //   "! =" should become the not equal token "!\n=" should become
            //   an error token token for ! and then and eqauls token
            //   new line white space is treated differently
            let buffer_size = buffer.len();
            // when compiled with optimization I am pretty sure that rust will be able
            //    to tell that when calculating these bools there are no side effects
            //    and thus can hoist them so that the have the possibility of short circuiting
            let switched_type = (is_next_alpha != is_last_alpha) && (buffer_size > 1);
            let execedes_symbol_size = (buffer_size as i32 >= SYMBOL_MAX_SIZE) && !is_last_alpha;
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

            // string processing happens before so it can have different rules
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
            } else if allowed_but_skipped.contains(&c) && !in_string {
                // fold here because spaces / tabs are symbols which should always
                //    fold even if the buffer is a symbol of just one
                start_char_position = fold(
                    &mut buffer,
                    line_number,
                    start_char_position,
                    &mut token_stream,
                    in_string,
                    false,
                );
            }
        }
        // \n is a terminator so fold here
        // note this does not reset strings values so multi line strings should be without err

        if !in_comment {
            fold(
                &mut buffer,
                line_number,
                start_char_position,
                &mut token_stream,
                in_string,
                false,
            );
        }
        start_char_position = 0;
    }

    // Need to fold for end of file
    if !in_comment {
        fold(
            &mut buffer,
            line_number,
            start_char_position,
            &mut token_stream,
            in_string,
            false,
        );
    }
    // check to see if there is a comment close before eop check to create a new program to allow
    //    missing eop program
    if in_comment {
        let comment_error_entry = LexProblem::LexWarning(LexWarning::MissingCommentClose);
        token_stream.push(Err(comment_error_entry))
    }

    // check if file ends with $
    let mut ends_with_eop = false;
    if let Some(last_token_entry) = token_stream.last() {
        if let Ok(token) = last_token_entry {
            if matches!(token.kind, TokenKind::Symbol(Symbol::EndProgram)) {
                ends_with_eop = true;
            }
        }
    }
    if !ends_with_eop {
        let eop_error_entry = LexProblem::LexWarning(LexWarning::MissingEndProgram);
        token_stream.push(Err(eop_error_entry));
    }

    return token_stream;
}

// Unit tests for lex
#[cfg(test)]
mod lex_tests {
    // imports the lex mod as lex_tests is a sub mod
    use super::*;
    use colored::Colorize;
    use std::mem::discriminant;
    use std::path::Path;

    // helper function to get the token from a rep to
    //    make expected tests cases more readable
    fn reps_to_tokens(reps: Vec<&str>) -> Vec<Result<Token, LexProblem>> {
        let mut token_entries = Vec::new();
        let mut in_string = false;

        for rep in reps {
            match get_token(rep, (0, 0), 0, in_string) {
                Some(token) => token_entries.push(Ok(token)),
                None => {
                    let lex_problem = LexProblem::LexError(LexError::InvalidChar(InvalidChar {
                        character: rep.chars().next().unwrap(),
                        line: 0,
                        position: 0,
                    }));
                    token_entries.push(Err(lex_problem));
                }
            }
            if rep == "\"" {
                in_string = !in_string;
            }
        }

        let mut ends_with_eop = false;
        if let Some(last_token_entry) = token_entries.last() {
            if let Ok(token) = last_token_entry {
                if matches!(token.kind, TokenKind::Symbol(Symbol::EndProgram)) {
                    ends_with_eop = true;
                }
            }
        }
        if !ends_with_eop {
            let eop_error_entry = LexProblem::LexWarning(LexWarning::MissingEndProgram);
            token_entries.push(Err(eop_error_entry));
        }

        return token_entries;
    }

    // helper function to determine if token sequences are "like"
    //     another which means they produce more less the same tokens
    //     ignoring things such as values and positions
    // Really just doesnt check positions bc I would be insane if I were
    //     to code that into a my test cases and I also it also
    //     helps for asserting likeness in the lex with/ without spaces
    fn tokens_are_like(
        token_entries1: &Vec<Result<Token, LexProblem>>,
        tokens_entries2: &Vec<Result<Token, LexProblem>>,
    ) -> bool {
        static DIF_DOWN: &str = "DIFF ↓ ↓ ↓ ↓ ↓ ↓ ";
        static DIF_UP: &str = "DIFF ↑ ↑ ↑ ↑ ↑ ↑ \n";
        let mut are_like_flag = true;
        let zipped: Vec<_> = token_entries1.iter().zip(tokens_entries2.iter()).collect();
        for (token_entry1, token_entry2) in zipped {
            match &token_entry1 {
                Ok(token1) => match &token_entry2 {
                    Ok(token2) => {
                        if !(get_token_verbose_name(&token1.kind)
                            == get_token_verbose_name(&token2.kind))
                        {
                            println!("{}", DIF_DOWN.red());
                            process_lexeme(&token_entry1);
                            process_lexeme(&token_entry2);
                            println!("{}", DIF_UP.red());
                            are_like_flag = false;
                        }
                    }
                    Err(_) => {
                        println!("{}", DIF_DOWN.red());
                        process_lexeme(&token_entry1);
                        process_lexeme(&token_entry2);
                        println!("{}", DIF_UP.red());
                        are_like_flag = false;
                    }
                },
                Err(err1) => match &token_entry2 {
                    Ok(_) => {
                        println!("{}", DIF_DOWN.red());
                        process_lexeme(&token_entry1);
                        process_lexeme(&token_entry2);
                        println!("{}", DIF_UP.red());
                        are_like_flag = false;
                    }
                    Err(err2) => {
                        if !(discriminant(err1) == discriminant(err2)) {
                            println!("{}", DIF_DOWN.red());
                            process_lexeme(&token_entry1);
                            process_lexeme(&token_entry2);
                            println!("{}", DIF_UP.red());
                            are_like_flag = false;
                        }
                    }
                },
            };
        }
        if token_entries1.len() != tokens_entries2.len() {
            if token_entries1.len() > tokens_entries2.len() {
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
        let path = Path::new("test_cases/general/hello-compiler");
        let tokens = get_lexemes(path);
        assert!(tokens_are_like(&expected_tokens, &tokens))
    }

    #[test]
    fn three_symbols() {
        // file: =!=
        let expected_reps = vec!["=", "!="];
        let expected_tokens = reps_to_tokens(expected_reps);
        let path = Path::new("test_cases/lex-edge-cases/three-symbols");
        let tokens = get_lexemes(path);
        assert!(tokens_are_like(&expected_tokens, &tokens))
    }

    // skip formatting to make expected more readable
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
        let path_with_spaces_no_comments = Path::new("test_cases/general/lex-with-spaces-no-comments");
        let path_with_spaces = Path::new("test_cases/general/lex-with-spaces");
        let path_without_spaces = Path::new("test_cases/general/lex-without-spaces");
        let tokens_with_spaces_no_comments = get_lexemes(path_with_spaces);
        let tokens_with_spaces = get_lexemes(path_with_spaces_no_comments);
        let tokens_without_spaces = get_lexemes(path_without_spaces);
        assert!(tokens_are_like(&expected_tokens, &tokens_with_spaces));
        assert!(tokens_are_like(&expected_tokens, &tokens_without_spaces));
        assert!(tokens_are_like(&expected_tokens, &tokens_with_spaces_no_comments));
    }

    #[test]
    fn symbols_and_spaces() {
        // file:
        // ! =
        // !
        // =
        // =! =
        let expected_reps = vec!["!", "=", "!", "=", "=", "!", "="];
        let expected_tokens = reps_to_tokens(expected_reps);
        let path = Path::new("test_cases/lex-edge-cases/symbols_and_white_space");
        let tokens = get_lexemes(path);
        assert!(tokens_are_like(&expected_tokens, &tokens));
    }

    #[test]
    fn comment_between() {
        // file:
        // i/*COMMENT*/nt
        // =/*COMMENT*/=
        // "h/*O*/i"
        let expected_reps = vec![
            "i", "n", "t", "=", "=", "\"", "h", "/", "*", "O", "*", "/", "i", "\"",
        ];
        let expected_tokens = reps_to_tokens(expected_reps);
        let path = Path::new("test_cases/lex-edge-cases/comment_between");
        let tokens = get_lexemes(path);
        assert!(tokens_are_like(&expected_tokens, &tokens));
    }

    #[test]
    #[rustfmt::skip]
    fn multi_program() {
        /*
         * file:
        {
            int a
            a = 5
            print(a)
        }$
        {
            int b
            b = 2
            print(b)
        }$
        */
        let expected_reps = vec![
            "{", "int", "a", "a", "=", "5", "print", "(", "a", ")", "}", "$",
            "{", "int", "b", "b", "=", "2", "print", "(", "b", ")", "}", "$",
        ];
        let expected_tokens = reps_to_tokens(expected_reps);
        let path = Path::new("test_cases/general/multi-program");
        let tokens = get_lexemes(path);
        assert!(tokens_are_like(&expected_tokens, &tokens));
    }

    #[test]
    fn string() {
        // Where did some of my tests go?????
        // I swear I had a test called abc's that went over all tokens
        // I looked at commit history and didn't find anything and my git stash
        // I had distinctly 10 tests
        // apparently I did not have the string token
        // maybe it is somehow on my desktop idk
        let expected_reps = vec!["string"];
        let expected_tokens = reps_to_tokens(expected_reps);
        let path = Path::new("test_cases/lex-edge-cases/string");
        let tokens = get_lexemes(path);
        assert!(tokens_are_like(&expected_tokens, &tokens));
    }

    #[test]
    fn multi_string() {
        todo!("re-IMPLEMENT MULTI LINE STRING ERRORS");
    }
}

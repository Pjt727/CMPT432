use std::env;
use std::fs;
use std::path::Path;
mod lex;
use colored::Colorize;
mod parse;
mod semantically_analyze;
mod token;
mod code_generation;

fn compile_steps(file: &Path) {
    let token_entries = lex::get_lexemes(file);
    let mut programs_processed = 0;
    let mut amount_processed = 0;
    while amount_processed < token_entries.len() {
        programs_processed += 1;
        println!(
            "{} for program {}",
            "Starting Lex".magenta(),
            programs_processed
        );
        let token_entry_slice = token_entries[amount_processed..].iter();
        let (errors_and_warnings, end_index) = lex::process_lexemes(token_entry_slice);
        // TODO IMPLEMENT MOVING ON TO REST ONLY IF PROGRAM DOES NOT HAVE ERRORS
        //    MAY NEED SOME CHANGES TO IMPLEMENT WARNING
        if errors_and_warnings.0 == 0 {
            println!(
                "{} with {} Warning(s)",
                "Successfully Lexed".magenta(),
                errors_and_warnings.1
            );
            println!();

            // move on to parse
            let mut tokens: Vec<token::Token> = vec![];
            let safe_end_range = match end_index {
                Some(index) => index + amount_processed + 1,
                None => token_entries.len(),
            };
            for lexeme in token_entries[amount_processed..safe_end_range].iter() {
                match lexeme {
                    Ok(token) => tokens.push(token.clone()),
                    Err(lex_problem) => match lex_problem {
                        lex::LexProblem::LexError(_) => panic!("Error during lex!!"),
                        lex::LexProblem::LexWarning(w) => {
                            // need to add an endprogram if that error exists
                            match w {
                                lex::LexWarning::MissingEndProgram => tokens.push(token::Token {
                                    kind: token::TokenKind::Symbol(token::Symbol::EndProgram),
                                    // -1 to demonstrate that it did not exist in the program
                                    start_end_position: (-1, 0),
                                    line: -1,
                                    representation: "$".to_string(),
                                }),
                                lex::LexWarning::MissingCommentClose => {}
                            }
                        }
                    },
                }
            }
            println!(
                "{} for program {}",
                "Starting Parse".magenta(),
                programs_processed
            );

            let cst = parse::ConcreteSyntaxTree::new(tokens.iter());
            cst.show_parse_steps();
            println!();
            cst.show();
            if cst.has_error() {
                println!("{} because of parse error.", "Skipping Next Steps".red())
            } else {
                println!();
                let ast = semantically_analyze::AbstractSyntaxTree::new(cst);
                println!(
                    "{} for program {}",
                    "Starting Semantic Analysis".magenta(),
                    programs_processed
                    );
                println!( "{}", "Showing AST".magenta());
                ast.show();
                let semantic_checks = semantically_analyze::SemanticChecks::new(&ast);
                semantic_checks.show();
                println!();
            }
        } else {
            println!(
                "{} with {} Fatal Error(s) and {} Warning(s)",
                "Failed Lex".red(),
                errors_and_warnings.0,
                errors_and_warnings.1,
            );
            println!("{} because of lex errors", "Skipping Next Steps".red())
        }
        println!();
        match end_index {
            Some(end_index) => amount_processed += end_index + 1,
            None => break,
        }
    }

    // TODO: SEMANTIC ANALYSIS
    // TODO: CODE GENERATION
}

fn main() {
    // the first arg is the target
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        panic!("\nExpected files or directory input!\n")
    }
    let first_path = Path::new(&args[1]);
    if !first_path.exists() {
        panic!("\nThat path does not exist! Please place files / folders in the root directory.\n")
    } else if first_path.is_dir() {
        let files = fs::read_dir(first_path).unwrap();
        for file in files {
            let file = match file {
                Ok(file) => file,
                Err(_) => continue,
            };
            let file_path = file.path();
            println!(
                "{} for {}",
                "Starting compile steps".magenta(),
                file_path.to_string_lossy()
            );
            compile_steps(file_path.as_path());
            println!("{}", "Finished compile steps\n".magenta())
        }
    } else {
        println!(
            "{} for {}",
            "Starting compile steps".magenta(),
            first_path.to_string_lossy()
        );
        compile_steps(first_path);
        println!("{}", "Finished compile steps\n".magenta())
    }
}

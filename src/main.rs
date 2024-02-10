use std::env;
use std::fs;
use std::path::Path;
mod lex;
use colored::Colorize;

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
        } else {
            println!(
                "{} with {} Fatal Error(s) and {} Warning(s)",
                "Failed Lex".red(),
                errors_and_warnings.0,
                errors_and_warnings.1,
            );
        }
        match end_index {
            Some(end_index) => amount_processed += end_index + 1,
            None => break,
        }
    }

    // TODO: PARSE
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
            match file_path.extension() {
                Some(extension) => {
                    if extension != "txt" {
                        continue;
                    }
                }
                None => continue,
            }
            println!(
                "{} for {}",
                "Starting compile steps".magenta(),
                file_path.to_string_lossy()
            );
            compile_steps(file_path.as_path());
            println!("{}", "Finished compile steps\n".magenta())
        }
    } else if let Some(extension) = first_path.extension() {
        if extension != "txt" {
            panic!("That is not a valid filetype to compile!")
        }
        println!(
            "{} for {}",
            "Starting compile steps".magenta(),
            first_path.to_string_lossy()
        );
        compile_steps(first_path);
        println!("{}", "Finished compile steps\n".magenta())
    }
}

use std::env;
use std::fs;
use std::path::Path;
mod lex;

fn compile_steps(file: &Path) {
    let _tokens = lex::lex_file(file);
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
                    if extension != ".txt" {
                        continue;
                    }
                }
                None => continue,
            }
            compile_steps(file_path.as_path())
        }
    } else if let Some(extension) = first_path.extension() {
        if extension != "txt" {
            panic!("That is not a valid filetype to compile!")
        }
        compile_steps(first_path)
    }
}

use std::{
    fs,
    io,
    fmt,
};
use colored::Colorize;
use clap::{Arg, App, SubCommand};
use crate::parser;
use crate::symbol_table;

#[derive(Debug)]
pub enum CliError<'argument, 'input> {
    FileNotFound {
        name: &'argument str,
        error: io::Error,
    },
    ParseError {
        error: parser::ParseError<'input>,
    },
    SymbolTableError {
        error: symbol_table::SymbolTableError<'input>,
    }
}

impl<'argument, 'input> fmt::Display for CliError<'argument, 'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            CliError::FileNotFound { name, .. } => {
                write!(f, "File not found: {}", name)
            },
            CliError::ParseError { error } => {
                write!(f, "ParseError: {} ", error)
            },
            CliError::SymbolTableError { error } => {
                write!(f, "SymbolTableError: {} ", error)
            }
        };
    }
}

#[inline]
fn print_error(err: CliError) {
    eprintln!("{} {}", "error:".red(), err);
}

fn compile_command(input_file: &str) {
    match fs::read_to_string(input_file) {
        Ok(content) => {
            match parser::parse(&content) {
                Ok(program) => {
                    match symbol_table::SymbolTable::build(&program) {
                        Ok(_) => {

                        },
                        Err(err) => {
                            print_error(CliError::SymbolTableError {
                                error: err,
                            });
                        }
                    }
                },
                Err(err) => {
                    print_error(CliError::ParseError {
                        error: err,
                    });
                }
            }
        },
        Err(err) => {
            print_error(CliError::FileNotFound {
                name: input_file,
                error: err,
            });
        }
    }
}

pub fn run_cli() {
    let app = App::new("V compiler")
        .setting(clap::AppSettings::ArgRequiredElseHelp)
        .version("0.1.0")
        .author("Ozan Akın")
        .about("Compiles V language, a programming language for CENG444 lecture")
        .subcommand(
            SubCommand::with_name("compile")
                .arg(
                    Arg::with_name("input")
                        .help("Sets the input file to use")
                        .required(true)
                        .index(1)
                )
        );

    let matches = app.get_matches();

    if let Some(matches) = matches.subcommand_matches("compile") {
        let input_file = matches.value_of("input").unwrap();

        compile_command(input_file);
    }
}

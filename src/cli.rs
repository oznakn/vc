use std::fs;
use std::fmt;
use std::path::Path;
use std::error::Error;
use colored::Colorize;
use clap::{Arg, App, SubCommand};

use crate::parser;
use crate::symbol_table;
use crate::ir;
use crate::codegen;

#[derive(Debug)]
pub struct CliError {
    error: String,
}
impl<'argument, 'input> Error for CliError {}

impl<'argument, 'input> fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.error)
    }
}

#[inline]
fn print_error(err: CliError) {
    println!("{} {}", "error:".red(), err);
}

fn compile_command(input_file: &str) -> Result<(), CliError>{
    let content = fs::read_to_string(input_file)
        .map_err(|_| CliError {
            error: format!("File not found: {}", input_file),
        })?;

    let program = parser::parse(&content)
        .map_err(|err| CliError {
            error: format!("{}", err),
        })?;

    let table = symbol_table::SymbolTable::build(&program)
        .map_err(|err| CliError {
            error: format!("{}", err),
        })?;

    let ir_context = ir::Builder::build(&program, &table);

    let generated_code = codegen::CodeGenerator::build(&ir_context);

    let output_file = format!("{}.S", Path::new(&input_file).file_stem().unwrap().to_str().unwrap());

    fs::write(Path::new(&output_file), codegen::convert_to_string(&generated_code))
        .map_err(|err| CliError {
            error: format!("{}", err),
        })?;

    return Ok(());
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

        if let Err(err) = compile_command(input_file) {
            print_error(err);
        }
    }
}

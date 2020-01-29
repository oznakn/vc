use clap::{App, Arg};
use colored::Colorize;
use std::fs;
use std::path::Path;

use crate::codegen;
use crate::error::CliError;
use crate::ir;
use crate::parser;
use crate::symbol_table;

#[inline]
fn print_error(err: CliError) {
    println!("{} {}", "error:".red(), err);
}

fn compile_command(matches: &clap::ArgMatches) -> Result<(), CliError> {
    let input_file = matches.value_of("input").unwrap();

    let content = fs::read_to_string(input_file).map_err(|_| CliError {
        error: format!("File not found: {}", input_file),
    })?;

    let program = parser::parse(&content).map_err(|err| CliError {
        error: format!("{}", err.to_string(&content)),
    })?;

    if matches.is_present("ast") {
        dbg!(&program);
    }

    let table = symbol_table::SymbolTable::build(&program).map_err(|err| CliError { error: format!("{}", err) })?;

    if matches.is_present("table") {
        dbg!(&table);
    }

    let ir_context = ir::IRContext::build(&program, &table);

    if matches.is_present("ir") {
        for item in &ir_context.items {
            match item {
                ir::IRItem::Label(_) => println!("{}", item),
                ir::IRItem::Function(_) => println!("{}", item),
                _ => println!("    {}", item),
            }
        }
    }

    let generated_code = codegen::riscv::CodeGenerator::build(&ir_context);

    let assembly = codegen::riscv::convert_to_string(&generated_code);

    if matches.is_present("print") {
        println!("{}", assembly);
    } else {
        let path = Path::new(&input_file).with_extension("s");

        fs::write(Path::new(&path), assembly).map_err(|err| CliError { error: format!("{}", err) })?;
    }

    return Ok(());
}

pub fn run_cli() {
    let app = App::new("V compiler")
        .setting(clap::AppSettings::ArgRequiredElseHelp)
        .version("0.1.0")
        .author("Ozan Akın")
        .about("Compiles V language, a programming language for CENG444 lecture")
        .arg(Arg::with_name("ast").long("emit-ast").help("Emits the abstract syntax tree"))
        .arg(Arg::with_name("ir").long("emit-ir").help("Emits the intermediate representation"))
        .arg(Arg::with_name("table").long("emit-symbol-table").help("Emits the symbol table"))
        .arg(Arg::with_name("print").long("print").help("Print assembly instead of writing a file"))
        .arg(Arg::with_name("input").help("Sets the input file to use").required(true).index(1));

    let matches = app.get_matches();

    if let Err(err) = compile_command(&matches) {
        print_error(err);
    }
}

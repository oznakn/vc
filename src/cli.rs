use std::fs;
use colored::Colorize;
use clap::{Arg, App, SubCommand};
use crate::parser;

fn compile_command(input_file: &str) {
    let result = fs::read_to_string(input_file);

    if result.is_err() {
        println!("{} cannot find file: {}", "error:".red(), input_file.blue());
    } else {
        let content = result.unwrap();

        parser::parse(&content);
    }
}

pub fn run_cli() {
    let matches = App::new("V compiler")
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
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("compile") {
        let input_file = matches.value_of("input").unwrap();

        compile_command(input_file);
    }
}

use std::fs;
use clap::{Arg, App, SubCommand};
use crate::parser;

#[derive(Debug)]
pub enum CliError<'argument> {
    FileNotFound {
        name: &'argument str,
    }
}

fn compile_command(input_file: &str) -> Result<(), CliError>{
    let content = fs::read_to_string(input_file).map_err(|_err| {
        return CliError::FileNotFound {
            name: input_file,
        };
    })?;

    let _p = parser::parse(&content);

    return Ok(());
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

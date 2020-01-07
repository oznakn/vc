use clap::{Arg, App, SubCommand};

fn build_command(input_file: &str) {
    println!("Running build command for file {}.", input_file);
}

pub fn run_cli() {
    let matches = App::new("V compiler")
        .version("0.1.0")
        .author("Ozan Akın")
        .about("Compiles V language, a programming language for CENG444 lecture")
        .subcommand(
            SubCommand::with_name("build")
                .arg(
                    Arg::with_name("input")
                        .help("Sets the input file to use")
                        .required(true)
                        .index(1)
                )
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("build") {
        let input_file = matches.value_of("input").unwrap();

        build_command(input_file);
    }
}

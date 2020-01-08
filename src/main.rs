#[allow(unused_imports)]
#[macro_use]
extern crate lalrpop_util;
extern crate clap;

pub mod tokens;
pub mod lexer;
pub mod parser;
pub mod ast;
pub mod cli;

fn main() {
    cli::run_cli();
}

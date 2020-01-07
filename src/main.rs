#[macro_use]
extern crate lalrpop_util;
extern crate clap;

pub mod parser;
pub mod ast;
mod cli;

fn main() {
    cli::run_cli();
}

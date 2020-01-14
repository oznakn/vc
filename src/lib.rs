use lalrpop_util::lalrpop_mod;

pub const MAIN_FUNCTION: &str = "main";
pub mod cli;
pub mod tokens;
pub mod lexer;
pub mod ast;
pub mod parser;
pub mod symbol_table;
pub mod ir;
pub mod codegen;

lalrpop_mod!(pub vlang);
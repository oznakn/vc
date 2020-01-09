use lalrpop_util::lalrpop_mod;

pub mod cli;
pub mod tokens;
pub mod lexer;
pub mod ast;
pub mod parser;
pub mod symbol_table;

lalrpop_mod!(pub vlang);
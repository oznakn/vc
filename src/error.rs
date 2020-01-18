use crate::tokens::Token;
use colored::Colorize;
use lalrpop_util::ParseError as LParseError;
use std::fmt;

use crate::location::{Lines, Location};

#[derive(Clone, Debug)]
pub enum LexicalError {
    InvalidChar { location: Location, ch: char },
    MissingChar { location: Location, ch: char },
}

impl<'input> From<LexicalError> for LParseError<Location, Token<'input>, LexicalError> {
    fn from(err: LexicalError) -> Self {
        LParseError::User { error: err }
    }
}

#[derive(Clone, Debug)]
pub enum ParseError<'input> {
    InvalidToken {
        location: Location,
    },
    UnrecognizedEOF {
        location: Location,
        expected: Vec<String>,
    },
    UnrecognizedToken {
        token: (Location, Token<'input>, Location),
        expected: Vec<String>,
    },
    ExtraToken {
        token: (Location, Token<'input>, Location),
    },
    User {
        error: LexicalError,
    },
}

impl<'input> From<LParseError<Location, Token<'input>, LexicalError>> for ParseError<'input> {
    fn from(err: LParseError<Location, Token<'input>, LexicalError>) -> Self {
        match err {
            LParseError::InvalidToken { location } => ParseError::InvalidToken { location },
            LParseError::UnrecognizedEOF { location, expected } => {
                ParseError::UnrecognizedEOF { location, expected }
            }
            LParseError::UnrecognizedToken { token, expected } => {
                ParseError::UnrecognizedToken { token, expected }
            }
            LParseError::ExtraToken { token } => ParseError::ExtraToken { token },
            LParseError::User { error } => ParseError::User { error },
        }
    }
}

impl<'input> ParseError<'input> {
    pub fn to_string(&self, source: &'input str) -> String {
        let lines = Lines::new(source);

        match self {
            ParseError::InvalidToken { location } => {
                let location_with_line_column = lines.location_with_line_column(location).unwrap();

                format!("Invalid token at position {}", location_with_line_column)
            }
            ParseError::UnrecognizedEOF { location, expected } => {
                let location_with_line_column = lines.location_with_line_column(location).unwrap();

                format!(
                    "Unrecognized EOF at position {}, expected one of: [{}]",
                    location_with_line_column,
                    expected.join(", ").purple(),
                )
            }
            ParseError::UnrecognizedToken { token, expected } => {
                let location_with_line_column = lines.location_with_line_column(&token.0).unwrap();

                format!(
                    "Unrecognized token at position {}, expected one of: [{}]",
                    location_with_line_column,
                    expected.join(", ").purple(),
                )
            }
            ParseError::ExtraToken { token } => {
                let location_with_line_column = lines.location_with_line_column(&token.0).unwrap();

                format!(
                    "Extra token `{}` at position {}",
                    format!("{}", token.1).purple(),
                    location_with_line_column
                )
            }
            ParseError::User { error } => match error {
                LexicalError::InvalidChar { location, ch } => {
                    let location_with_line_column =
                        lines.location_with_line_column(location).unwrap();

                    format!(
                        "Invalid char `{}` at position {}",
                        format!("{}", ch).purple(),
                        location_with_line_column
                    )
                }
                LexicalError::MissingChar { location, ch } => {
                    let location_with_line_column =
                        lines.location_with_line_column(location).unwrap();

                    format!(
                        "Missing char `{}` at position {}",
                        format!("{}", ch).purple(),
                        location_with_line_column
                    )
                }
            },
        }
    }
}

#[derive(Clone, Debug)]
pub enum SymbolTableError<'input> {
    VariableAlreadyDefinedError { name: &'input str },
    FunctionAlreadyDefinedError { name: &'input str },
    VariableNotFoundError { name: &'input str },
    TypesNotMatchError,
    FunctionNotFoundError { name: &'input str },
    WrongNumberOfArguments { name: &'input str },
    MainFunctionDoesNotExists,
    MainFunctionReturnTypeMustBeInt,
}

impl<'input> fmt::Display for SymbolTableError<'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            SymbolTableError::VariableAlreadyDefinedError { name } => {
                write!(f, "Variable `{}` already defined", name.purple())
            }
            SymbolTableError::FunctionAlreadyDefinedError { name } => {
                write!(f, "Function `{}` already defined", name.purple())
            }
            SymbolTableError::VariableNotFoundError { name } => {
                write!(f, "Variable `{}` not found", name.purple())
            }
            SymbolTableError::TypesNotMatchError => write!(f, "Wrong type detected"),
            SymbolTableError::FunctionNotFoundError { name } => {
                write!(f, "Function `{}` not found", name.purple())
            }
            SymbolTableError::WrongNumberOfArguments { name } => write!(
                f,
                "Wrong number of arguments when calling function `{}`",
                name.purple()
            ),
            SymbolTableError::MainFunctionDoesNotExists => {
                write!(f, "Main function does not exists")
            }
            SymbolTableError::MainFunctionReturnTypeMustBeInt => {
                write!(f, "Main function return type must be int")
            }
        };
    }
}

#[derive(Clone, Debug)]
pub struct CliError {
    pub error: String,
}

impl<'argument, 'input> fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.error)
    }
}

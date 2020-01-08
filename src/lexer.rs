use std::str::CharIndices;
use std::error::Error;
use crate::tokens::Token;
use std::fmt;

const EOF: char = '\0';

pub type Location = usize;

#[derive(Debug, PartialEq)]
pub enum LexicalError {
    InvalidToken {
        location: Location,
        token: char,
    },
    MissingToken {
        location: Location,
        token: char,
    },
}

impl Error for LexicalError {}

impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return match self {
            LexicalError::InvalidToken { location, token } => {
                write!(f, "Invalid token {}", token)
            },
            LexicalError::MissingToken { location, token } => {
                write!(f, "Missing token {}", token)
            }
        };
    }
}

pub struct Lexer<'input> {
    source: &'input str,
    chars: CharIndices<'input>,
    curr:  (usize, char),
    next:  (usize, char),
}

type LexerItem<'input> = Result<(Location, Token<'input>, Location), LexicalError>;

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        let mut chars = input.char_indices();
        let mut curr: (usize, char) = (0, EOF);
        let mut next: (usize, char) = (0, EOF);

        if let Some(item) = chars.next() {
            curr = item;
        }

        if let Some(item) = chars.next() {
            next = item;
        }

        return Lexer{
            source: input,
            chars,
            curr,
            next,
        }
    }

    #[inline]
    fn curr_char(&self) -> char {
        return self.curr.1;
    }

    #[inline]
    fn peek_char(&self) -> char {
        return self.next.1;
    }

    fn has_char(&self) -> bool {
        return self.curr.1 != EOF;
    }

    fn next_char(&mut self) -> char {
        self.curr = self.next;

        if let Some(item) = self.chars.next() {
            self.next = item;
        } else {
            self.next = (0, EOF);
        }

        return self.curr.1;
    }

    fn skip_whitespace(&mut self) {
        while self.has_char() && self.curr_char().is_whitespace() {
            self.next_char();
        }
    }

    fn read_number(&mut self) -> LexerItem<'input> {
        let start = self.curr.0;
        let mut pos = start;

        while self.has_char() {
            if !self.peek_char().is_numeric() && self.peek_char() != '.' {
                break;
            }

            pos += 1;
            self.next_char();
        }

        let result: f64 = self.source[start..pos].parse::<f64>().unwrap();

        println!("New number found: {}", result);

        return Ok((start, Token::Number(result), pos));
    }

    fn read_string(&mut self) -> LexerItem<'input> {
        let start = self.curr.0 + 1;
        let mut pos = start;

        self.next_char();

        while self.has_char() {
            if self.curr_char() == '\\' {
                pos += 2;

                self.next_char();
                self.next_char();
            } else if self.curr_char() == '"' {
                self.next_char();

                break;
            } else {
                pos += 1;
                self.next_char();
            }
        }

        println!("New string found: {}", &self.source[start..pos]);

        return Ok((start, Token::String(&self.source[start..pos]), pos));
    }

    fn read_identifier(&mut self) -> LexerItem<'input> {
        let start = self.curr.0;
        let mut pos = start;

        if !self.curr_char().is_alphabetic() && self.curr_char() != '_' {
            // TODO: error handling
        }

        while self.has_char() {
            if self.curr_char().is_alphanumeric() || self.curr_char() == '_' {
                pos += 1;
                self.next_char();
            } else {
                pos += 1;

                break;
            }
        }

        println!("New identifier found: {}", &self.source[start..pos]);

        return Ok((start, Token::String(&self.source[start..pos]), pos));
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = LexerItem<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.has_char() {
            self.skip_whitespace();

            match self.curr_char() {
                _ => {
                    if self.curr_char().is_numeric() {
                        return Some(self.read_number());
                    }

                    if self.curr_char() == '\"' {
                        return Some(self.read_string());
                    }

                    return Some(self.read_identifier());
                },
            }
        }

        return None;
    }
}
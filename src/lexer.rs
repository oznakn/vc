use crate::error::LexicalError;
use crate::location::Location;
use crate::tokens::Token;

use std::str::CharIndices;

const EOF: char = '\0';

pub struct Lexer<'input> {
    source: &'input str,
    chars: CharIndices<'input>,
    curr: (Location, char),
    next: (Location, char),
}

type LexerItem<'input> = Result<(Location, Token<'input>, Location), LexicalError>;

impl<'input> Lexer<'input> {
    pub fn new(source: &'input str) -> Self {
        let mut chars = source.char_indices();
        let mut curr: (Location, char) = (0, EOF);
        let mut next: (Location, char) = (0, EOF);

        if let Some(item) = chars.next() {
            curr = item;
        }

        if let Some(item) = chars.next() {
            next = item;
        }

        Lexer { source, chars, curr, next }
    }

    #[inline]
    fn curr_location(&self) -> Location {
        self.curr.0
    }

    #[inline]
    fn curr_char(&self) -> char {
        self.curr.1
    }

    #[inline]
    fn has_char(&self) -> bool {
        self.curr.1 != EOF
    }

    fn next_char(&mut self) -> char {
        self.curr = self.next;

        if let Some(item) = self.chars.next() {
            self.next = item;
        } else {
            self.next = (0, EOF);
        }

        self.curr.1
    }

    #[inline]
    fn is_breaking_whitespace(&self) -> bool {
        self.curr_char() == '\r' || self.curr_char() == '\n'
    }

    fn skip_whitespace(&mut self) {
        while self.has_char() && self.curr_char().is_whitespace() {
            self.next_char();
        }
    }

    fn skip_comments(&mut self) {
        if self.has_char() && self.curr_char() == '%' {
            while self.has_char() && self.curr_char() != '\r' && self.curr_char() != '\n' {
                self.next_char();
            }
        }
    }

    fn read_number(&mut self) -> LexerItem<'input> {
        let start = self.curr.0;
        let mut pos = start;

        let mut sci_start = 0;
        let mut sci_pos = 0;
        let mut sci_sign = 1;

        let mut is_integer: bool = true;
        let mut dot_seen: bool = false;
        let mut e_seen: bool = false;
        let mut e_sign_seen: bool = false;

        while self.has_char() {
            if self.curr_char().is_numeric() {
                if e_seen {
                    sci_pos += 1;
                } else {
                    pos += 1;
                }

                self.next_char();
            } else if self.curr_char() == '.' {
                if dot_seen {
                    return Err(LexicalError::InvalidChar {
                        location: self.curr_location(),
                        ch: self.curr_char(),
                    });
                }

                pos += 1;
                is_integer = false;
                dot_seen = true;

                self.next_char();
            } else if self.curr_char() == 'E' || self.curr_char() == 'e' {
                if !dot_seen || e_seen {
                    return Err(LexicalError::InvalidChar {
                        location: self.curr_location(),
                        ch: self.curr_char(),
                    });
                }

                e_seen = true;
                sci_start = pos + 1;
                sci_pos = sci_start;

                self.next_char();
            } else if self.curr_char() == '+' || self.curr_char() == '-' {
                if !dot_seen || !e_seen || e_sign_seen {
                    return Err(LexicalError::InvalidChar {
                        location: self.curr_location(),
                        ch: self.curr_char(),
                    });
                }

                if self.curr_char() == '-' {
                    sci_sign = -1;
                }

                sci_start = sci_pos + 1;
                sci_pos = sci_start;
                e_sign_seen = true;

                self.next_char();
            } else {
                break;
            }
        }

        if is_integer {
            let result = self.source[start..pos].parse::<u64>().unwrap();

            return Ok((start, Token::IntLiteral(result), pos));
        }

        let mut result: f64 = self.source[start..pos].parse::<f64>().unwrap();

        if e_seen {
            let coefficient = (self.source[sci_start..sci_pos].parse::<u32>().unwrap() as i32) * sci_sign;
            result *= 10f64.powi(coefficient);
        }

        Ok((start, Token::RealLiteral(result), pos))
    }

    fn read_string(&mut self) -> LexerItem<'input> {
        let start = self.curr.0 + 1;
        let mut pos = start;

        if self.curr_char() != '"' {
            return Err(LexicalError::InvalidChar {
                location: self.curr_location(),
                ch: self.curr_char(),
            });
        }
        self.next_char();

        while self.has_char() {
            if self.curr_char() == '"' {
                break;
            } else if self.is_breaking_whitespace() {
                return Err(LexicalError::InvalidChar {
                    location: self.curr_location(),
                    ch: self.curr_char(),
                });
            } else if self.curr_char() == '\\' {
                pos += 2;

                self.next_char();
                self.next_char();
            } else {
                pos += 1;

                self.next_char();
            }
        }

        if self.curr_char() != '"' {
            return Err(LexicalError::InvalidChar {
                location: self.curr_location(),
                ch: self.curr_char(),
            });
        }
        self.next_char();

        Ok((start, Token::StringLiteral(&self.source[start..pos]), pos))
    }

    fn read_raw_identifier_or_keyword(&mut self) -> LexerItem<'input> {
        let start = self.curr.0;
        let mut pos = start;

        if !self.curr_char().is_alphabetic() && self.curr_char() != '_' {
            return Err(LexicalError::InvalidChar {
                location: self.curr_location(),
                ch: self.curr_char(),
            });
        }

        while self.has_char() {
            if self.curr_char().is_alphanumeric() || self.curr_char() == '_' {
                pos += 1;

                self.next_char();
            } else {
                break;
            }
        }

        Ok((start, Token::Identifier(&self.source[start..pos]), pos))
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = LexerItem<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        if self.has_char() {
            return match self.curr_char() {
                '{' => {
                    self.next_char();

                    Some(Ok((self.curr_location(), Token::OpenBrace, self.curr_location() + 1)))
                }
                '}' => {
                    self.next_char();

                    Some(Ok((self.curr_location(), Token::CloseBrace, self.curr_location() + 1)))
                }
                '[' => {
                    self.next_char();

                    return Some(Ok((self.curr_location(), Token::OpenBracket, self.curr_location() + 1)));
                }
                ']' => {
                    self.next_char();

                    Some(Ok((self.curr_location(), Token::CloseBracket, self.curr_location() + 1)))
                }
                '(' => {
                    self.next_char();

                    Some(Ok((self.curr_location(), Token::OpenParen, self.curr_location() + 1)))
                }
                ')' => {
                    self.next_char();

                    Some(Ok((self.curr_location(), Token::CloseParen, self.curr_location() + 1)))
                }
                ';' => {
                    self.next_char();

                    Some(Ok((self.curr_location(), Token::Semicolon, self.curr_location() + 1)))
                }
                ',' => {
                    self.next_char();

                    Some(Ok((self.curr_location(), Token::Comma, self.curr_location() + 1)))
                }
                '+' => {
                    self.next_char();

                    Some(Ok((self.curr_location(), Token::Plus, self.curr_location() + 1)))
                }
                '-' => {
                    self.next_char();

                    Some(Ok((self.curr_location(), Token::Minus, self.curr_location() + 1)))
                }
                '*' => {
                    self.next_char();

                    Some(Ok((self.curr_location(), Token::Star, self.curr_location() + 1)))
                }
                '/' => {
                    self.next_char();

                    Some(Ok((self.curr_location(), Token::Slash, self.curr_location() + 1)))
                }
                ':' => {
                    self.next_char();

                    if self.curr_char() == '=' {
                        self.next_char();

                        return Some(Ok((self.curr_location() - 1, Token::Assign, self.curr_location() + 1)));
                    }

                    Some(Ok((self.curr_location(), Token::Colon, self.curr_location() + 1)))
                }
                '=' => {
                    self.next_char();

                    Some(Ok((self.curr_location(), Token::Equal, self.curr_location() + 1)))
                }
                '<' => {
                    self.next_char();

                    if self.curr_char() == '>' {
                        self.next_char();

                        return Some(Ok((self.curr_location() - 1, Token::NotEqual, self.curr_location() + 1)));
                    }

                    if self.curr_char() == '=' {
                        self.next_char();

                        return Some(Ok((self.curr_location() - 1, Token::LessEqual, self.curr_location() + 1)));
                    }

                    Some(Ok((self.curr_location(), Token::Less, self.curr_location() + 1)))
                }
                '>' => {
                    self.next_char();

                    if self.curr_char() == '=' {
                        self.next_char();

                        return Some(Ok((self.curr_location() - 1, Token::GreaterEqual, self.curr_location() + 1)));
                    }

                    Some(Ok((self.curr_location(), Token::Greater, self.curr_location() + 1)))
                }
                '%' => {
                    self.skip_comments();

                    self.next()
                }
                c if c.is_numeric() => Some(self.read_number()),
                c if c == '"' => Some(self.read_string()),
                _ => {
                    let result = self.read_raw_identifier_or_keyword();

                    if result.is_err() {
                        return Some(result);
                    }

                    let (l, token, l2) = result.unwrap();

                    return match token {
                        Token::Identifier(s) => match s {
                            "var" => Some(Ok((l, Token::Var, l2))),
                            "func" => Some(Ok((l, Token::Func, l2))),
                            "endfunc" => Some(Ok((l, Token::EndFunc, l2))),
                            "return" => Some(Ok((l, Token::Return, l2))),
                            "to" => Some(Ok((l, Token::To, l2))),
                            "by" => Some(Ok((l, Token::By, l2))),
                            "and" => Some(Ok((l, Token::And, l2))),
                            "mod" => Some(Ok((l, Token::Mod, l2))),
                            "div" => Some(Ok((l, Token::Div, l2))),
                            "if" => Some(Ok((l, Token::If, l2))),
                            "then" => Some(Ok((l, Token::Then, l2))),
                            "else" => Some(Ok((l, Token::Else, l2))),
                            "endif" => Some(Ok((l, Token::EndIf, l2))),
                            "for" => Some(Ok((l, Token::For, l2))),
                            "endfor" => Some(Ok((l, Token::EndFor, l2))),
                            "or" => Some(Ok((l, Token::Or, l2))),
                            "do" => Some(Ok((l, Token::Do, l2))),
                            "print" => Some(Ok((l, Token::Print, l2))),
                            "read" => Some(Ok((l, Token::Read, l2))),
                            "while" => Some(Ok((l, Token::While, l2))),
                            "endwhile" => Some(Ok((l, Token::EndWhile, l2))),
                            "not" => Some(Ok((l, Token::Not, l2))),
                            "int" => Some(Ok((l, Token::Int, l2))),
                            "real" => Some(Ok((l, Token::Real, l2))),
                            _ => Some(Ok((l, Token::Identifier(s), l2))),
                        },
                        _ => {
                            unreachable!();
                        }
                    };
                }
            };
        }

        None
    }
}

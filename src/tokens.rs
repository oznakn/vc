use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'input> {
    // Literals
    Identifier(&'input str),
    String(&'input str),
    Number(f64),
}

impl<'input> fmt::Display for Token<'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Token::String(s)  => write!(f, "{}", s),
            _ => write!(f, "{}", "nothing")
        }
    }
}
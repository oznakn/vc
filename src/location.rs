use colored::Colorize;
use std::fmt;
use std::iter;

pub type Location = usize;

#[derive(Clone, Debug)]
pub struct LocationWithLineColumn {
    line: usize,
    column: usize,
}

impl fmt::Display for LocationWithLineColumn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format!("{}:{}", self.line + 1, self.column + 1).blue())
    }
}

// https://github.com/gluon-lang/gluon/blob/f8326d21a14b5f21d203e9c43fa5bb7f0688a74c/base/src/source.rs
#[derive(Clone, Debug)]
pub struct Lines {
    starting_bytes: Vec<usize>,
    end: usize,
}

impl<'input> Lines {
    pub fn new(src: &'input str) -> Self {
        let mut len = 0;
        let starting_bytes = {
            let input_indices = src.chars().into_iter().inspect(|_| len += 1).enumerate().filter(|&(_, b)| b == '\n').map(|(i, _)| i + 1); // index of first char in the line

            iter::once(0).chain(input_indices).collect()
        };

        Lines { starting_bytes, end: len }
    }

    fn line(&self, line_number: usize) -> Option<Location> {
        self.starting_bytes.get(line_number).cloned()
    }

    pub fn location_with_line_column(&self, pos: &usize) -> Option<LocationWithLineColumn> {
        if *pos <= self.end {
            let line_index = self.line_number_at_byte(*pos);

            self.line(line_index).map(|line_byte| LocationWithLineColumn { line: line_index, column: pos - line_byte })
        } else {
            None
        }
    }

    fn line_number_at_byte(&self, byte: Location) -> usize {
        let num_lines = self.starting_bytes.len();

        (0..num_lines).filter(|&i| self.starting_bytes[i] > byte).map(|i| i - 1).next().unwrap_or(num_lines - 1)
    }
}

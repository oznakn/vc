pub struct NumLiteral {
    pub value: f64,
}

pub struct Identifier<'input> {
    pub name: &'input str
}

pub struct StringLiteral<'input> {
    pub value: &'input str
}
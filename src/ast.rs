pub struct NumLiteral {
    pub value: f64,
}

pub struct Identifier<'input> {
    pub name: &'input str
}

impl NumLiteral {
    pub fn build(s: &str) -> NumLiteral {
        let value= s.parse::<f64>().unwrap();

        return NumLiteral{
            value,
        };
    }
}

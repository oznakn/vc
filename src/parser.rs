#[cfg(test)]
mod test {
    lalrpop_mod!(pub vc);

    use crate::lexer::Lexer;

    #[test]
    fn string_test() {
        let mut lexer;

        lexer = Lexer::new("\"selamlar\"");
        assert!(vc::StringLiteralParser::new().parse(lexer).is_ok());
    }

    #[test]
    fn identifier_test() {
        let mut lexer;

        lexer = Lexer::new("merhaba");
        assert!(vc::StringLiteralParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("__merhaba123");
        assert!(vc::StringLiteralParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("23merhaba");
        assert!(vc::StringLiteralParser::new().parse(lexer).is_err());
    }
}

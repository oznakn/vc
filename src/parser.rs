#[cfg(test)]
mod test {
    lalrpop_mod!(pub vc);

    use crate::lexer::Lexer;

    #[test]
    fn variable_definition_test() {
        let mut lexer;

        lexer = Lexer::new("a: int");
        assert!(vc::VariableParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("a: int");
        assert!(vc::VariableListParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("a: int, b: int");
        assert!(vc::VariableListParser::new().parse(lexer).is_ok());
    }

    #[test]
    fn declaration_list_test() {
        let mut lexer;

        lexer = Lexer::new("var a: int, b: int;");
        assert!(vc::DeclarationStatementParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("var a: int, b: real;");
        assert!(vc::DeclarationStatementParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("var a: int[], b: int[];");
        assert!(vc::DeclarationStatementParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("a: int");
        assert!(vc::DeclarationStatementParser::new().parse(lexer).is_err());
    }

    #[test]
    fn string_test() {
        let lexer;

        lexer = Lexer::new("\"selamlar\"");
        assert!(vc::StringLiteralParser::new().parse(lexer).is_ok());
    }

    #[test]
    fn identifier_test() {
        let mut lexer;

        lexer = Lexer::new("merhaba");
        assert!(vc::IdentifierParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("__merhaba123");
        assert!(vc::IdentifierParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("23merhaba");
        assert!(vc::IdentifierParser::new().parse(lexer).is_err());
    }
}

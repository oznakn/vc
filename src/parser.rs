lalrpop_mod!(pub vlang);

use crate::lexer::Lexer;
use colored::Colorize;

pub fn parse(s: &str) {
    let lexer = Lexer::new(s);
    let parser = vlang::ProgramParser::new();

    let result = parser.parse(lexer);

    if result.is_err() {
        if let Some(err) = result.err() {
            println!("{}", err);
        }
    } else {
        result.unwrap();

        println!("{}", "Success".green());
    }
}

#[cfg(test)]
mod test {
    lalrpop_mod!(pub vlang);

    use crate::lexer::Lexer;

    #[test]
    fn variable_definition_test() {
        let mut lexer;

        lexer = Lexer::new("a: int");
        assert!(vlang::VariableParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("a: int");
        assert!(vlang::VariableListParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("a: int, b: int");
        assert!(vlang::VariableListParser::new().parse(lexer).is_ok());
    }

    #[test]
    fn declaration_list_test() {
        let mut lexer;

        lexer = Lexer::new("var a: int, b: int;");
        assert!(vlang::DeclarationStatementParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("var a: int, b: real;");
        assert!(vlang::DeclarationStatementParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("var a: int[], b: int[];");
        assert!(vlang::DeclarationStatementParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("a: int");
        assert!(vlang::DeclarationStatementParser::new().parse(lexer).is_err());
    }

    #[test]
    fn string_test() {
        let lexer;

        lexer = Lexer::new("\"selamlar\"");
        assert!(vlang::StringLiteralParser::new().parse(lexer).is_ok());
    }

    #[test]
    fn identifier_test() {
        let mut lexer;

        lexer = Lexer::new("merhaba");
        assert!(vlang::IdentifierParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("__merhaba123");
        assert!(vlang::IdentifierParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("23merhaba");
        assert!(vlang::IdentifierParser::new().parse(lexer).is_err());
    }
}

use crate::lexer;
use crate::ast;
use crate::tokens;
use crate::vlang;
use lalrpop_util::ParseError as LParseError;

pub type ParseError<'input> = LParseError<lexer::Location, tokens::Token<'input>, lexer::LexicalError>;

pub fn parse(s: &str) -> Result<ast::Program, ParseError> {
    let lexer = lexer::Lexer::new(s);
    let parser = vlang::ProgramParser::new();

    let program: ast::Program = parser.parse(lexer)?;

    return Ok(program);
}

#[cfg(test)]
mod test {
    use lalrpop_util::lalrpop_mod;
    use crate::lexer::Lexer;

    lalrpop_mod!(pub vlang);

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
        assert!(vlang::DeclarationParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("var a: int, b: real;");
        assert!(vlang::DeclarationParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("var a: int[], b: int[10];");
        assert!(vlang::DeclarationParser::new().parse(lexer).is_ok());

        lexer = Lexer::new("a: int");
        assert!(vlang::DeclarationParser::new().parse(lexer).is_err());
    }
}

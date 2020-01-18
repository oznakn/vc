use crate::ast;
use crate::error::ParseError;
use crate::lexer;
use crate::vlang;

pub fn parse<'input>(source: &'input str) -> Result<ast::Program, ParseError<'input>> {
    let lexer = lexer::Lexer::new(source);
    let parser = vlang::ProgramParser::new();

    let program: ast::Program = parser.parse(lexer)?;

    return Ok(program);
}

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use lalrpop_util::lalrpop_mod;

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
        assert!(vlang::DeclarationParser::new().parse(lexer).is_err());

        lexer = Lexer::new("a: int");
        assert!(vlang::DeclarationParser::new().parse(lexer).is_err());
    }
}

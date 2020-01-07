lalrpop_mod!(pub vc);

#[test]
fn test_parser() {
    assert!(vc::NumLiteralParser::new().parse("23").is_ok());
    assert!(vc::NumLiteralParser::new().parse("23.12").is_ok());

    assert!(vc::IdentifierParser::new().parse("aaa").is_ok());
    assert!(vc::IdentifierParser::new().parse("1aaa<").is_err());
    assert!(vc::IdentifierParser::new().parse("1aaa").is_err());
}
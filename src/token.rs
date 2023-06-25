use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    ILLEGAL,
    EOF,

    IDENT(String),
    INT(String),

    ASSIGN,
    PLUS,

    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    FUNCTION,
    LET,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::ILLEGAL => write!(f, "ILLEGAL"),
            Token::EOF => write!(f, "EOF"),
            Token::IDENT(ident) => write!(f, "IDENT {}", ident),
            Token::INT(int) => write!(f, "INT, {}", int),
            Token::ASSIGN => write!(f, "="),
            Token::PLUS => write!(f, "+"),
            Token::COMMA => write!(f, ","),
            Token::SEMICOLON => write!(f, ";"),
            Token::LPAREN => write!(f, "("),
            Token::RPAREN => write!(f, ")"),
            Token::LBRACE => write!(f, "{{"),
            Token::RBRACE => write!(f, "}}"),
            Token::FUNCTION => write!(f, "FUNCTION"),
            Token::LET => write!(f, "LET"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Token;

    #[test]
    fn to_string() {
        assert_eq!(Token::LBRACE.to_string(), "{");
    }
}

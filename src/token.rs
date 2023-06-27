use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident(String),
    Int(String),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Eq,
    NotEq,
    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lt,
    Gt,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Illegal => write!(f, "ILLEGAL"),
            Token::Eof => write!(f, "EOF"),
            Token::Ident(ident) => write!(f, "IDENT {}", ident),
            Token::Int(int) => write!(f, "INT, {}", int),
            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Eq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::Lparen => write!(f, "("),
            Token::Rparen => write!(f, ")"),
            Token::Lbrace => write!(f, "{{"),
            Token::Rbrace => write!(f, "}}"),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::Function => write!(f, "FUNCTION"),
            Token::Let => write!(f, "LET"),
            Token::True => write!(f, "TRUE"),
            Token::False => write!(f, "FALSE"),
            Token::If => write!(f, "IF"),
            Token::Else => write!(f, "ELSE"),
            Token::Return => write!(f, "RETURN"),
        }
    }
}

pub fn lookup_ident(ident: String) -> Token {
    match ident.as_str() {
        "fn" => Token::Function,
        "let" => Token::Let,
        "true" => Token::True,
        "false" => Token::False,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        _ => Token::Ident(ident),
    }
}

#[cfg(test)]
mod tests {
    use super::Token;

    #[test]
    fn to_string() {
        assert_eq!(Token::Lbrace.to_string(), "{");
    }
}

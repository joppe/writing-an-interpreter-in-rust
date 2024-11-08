use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Illegal,
    Eof,

    // Identifiers + literals
    Identifier(String),
    Int(String),
    String(String),

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
    Colon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lt,
    Gt,
    Lbracket,
    Rbracket,

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
            Token::Identifier(ident) => write!(f, "IDENTIFIER {}", ident),
            Token::Int(int) => write!(f, "INT, {}", int),
            Token::String(string) => write!(f, "STRING {}", string),
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
            Token::Colon => write!(f, ":"),
            Token::Lparen => write!(f, "("),
            Token::Rparen => write!(f, ")"),
            Token::Lbrace => write!(f, "{{"),
            Token::Rbrace => write!(f, "}}"),
            Token::Lbracket => write!(f, "["),
            Token::Rbracket => write!(f, "]"),
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

pub fn lookup_identifier(identifier: String) -> Token {
    match identifier.as_str() {
        "fn" => Token::Function,
        "let" => Token::Let,
        "true" => Token::True,
        "false" => Token::False,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        _ => Token::Identifier(identifier),
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

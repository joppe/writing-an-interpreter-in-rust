use crate::token::{lookup_identifier, Token};

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    char: char,
}

pub const EOF: char = '\u{0}';

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let chars = input.chars();
        let mut lexer = Lexer {
            input: chars.collect(),
            position: 0,
            read_position: 0,
            char: EOF,
        };

        lexer.read_char();

        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.char {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();

                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            ':' => Token::Colon,
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();

                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            '[' => Token::Lbracket,
            ']' => Token::Rbracket,
            '<' => Token::Lt,
            '>' => Token::Gt,
            '"' => Token::String(self.read_string()),
            EOF => Token::Eof,
            _ => {
                if is_letter(self.char) {
                    let ident = self.read_identifier();

                    return lookup_identifier(ident);
                } else if is_digit(self.char) {
                    let number = self.read_number();

                    return Token::Int(number);
                } else {
                    return Token::Illegal;
                }
            }
        };

        self.read_char();

        token
    }

    fn skip_whitespace(&mut self) {
        while self.char.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            EOF
        } else {
            self.input[self.read_position]
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.char = EOF;
        } else {
            self.char = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;

        while is_letter(self.char) {
            self.read_char();
        }

        self.input[position..self.position].iter().collect()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;

        while is_digit(self.char) {
            self.read_char();
        }

        self.input[position..self.position].iter().collect()
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;

        loop {
            self.read_char();

            if self.char == '"' || self.char == EOF {
                break;
            }
        }

        self.input[position..self.position].iter().collect()
    }
}

fn is_letter(char: char) -> bool {
    char.is_ascii_alphabetic() || char == '_'
}

fn is_digit(char: char) -> bool {
    char.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::token::Token;

    #[test]
    fn next_token() {
        let input = "
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
            \"foobar\"
            \"foo bar\"
            [1, 2];
            {\"foo\": \"bar\"}
        ";

        let tests = vec![
            Token::Let,
            Token::Identifier("five".to_string()),
            Token::Assign,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("ten".to_string()),
            Token::Assign,
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Identifier("x".to_string()),
            Token::Comma,
            Token::Identifier("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier("result".to_string()),
            Token::Assign,
            Token::Identifier("add".to_string()),
            Token::Lparen,
            Token::Identifier("five".to_string()),
            Token::Comma,
            Token::Identifier("ten".to_string()),
            Token::Rparen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::Gt,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::If,
            Token::Lparen,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::Rbrace,
            Token::Int("10".to_string()),
            Token::Eq,
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::Int("10".to_string()),
            Token::NotEq,
            Token::Int("9".to_string()),
            Token::Semicolon,
            Token::String("foobar".to_string()),
            Token::String("foo bar".to_string()),
            Token::Lbracket,
            Token::Int("1".to_string()),
            Token::Comma,
            Token::Int("2".to_string()),
            Token::Rbracket,
            Token::Semicolon,
            Token::Lbrace,
            Token::String("foo".to_string()),
            Token::Colon,
            Token::String("bar".to_string()),
            Token::Rbrace,
            Token::Eof,
        ];
        let mut lexer = Lexer::new(input.to_string());

        tests.iter().for_each(|test_token| {
            let token = lexer.next_token();

            assert_eq!(token, *test_token);
        });
    }
}

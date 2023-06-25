use crate::token::Token;

// https://github.com/mohitk05/monkey-rust/blob/master/src/main.rs
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    char: char,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let chars = input.chars();
        let mut lexer = Lexer {
            input: chars.collect(),
            position: 0,
            read_position: 0,
            char: '\u{0}',
        };

        lexer.read_char();

        lexer
    }

    pub fn next_token(&mut self) -> Token {
        let token = match self.char {
            '=' => Token::ASSIGN,
            ';' => Token::SEMICOLON,
            '(' => Token::LPAREN,
            ')' => Token::RPAREN,
            ',' => Token::COMMA,
            '+' => Token::PLUS,
            '{' => Token::LBRACE,
            '}' => Token::RBRACE,
            '\u{0}' => Token::EOF,
            _ => Token::ILLEGAL,
        };

        self.read_char();

        token
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.char = '\u{0}';
        } else {
            self.char = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::token::Token;

    #[test]
    fn next_token() {
        let input = String::from("=+(){},;");
        let tests = vec![
            Token::ASSIGN,
            Token::PLUS,
            Token::LPAREN,
            Token::RPAREN,
            Token::LBRACE,
            Token::RBRACE,
            Token::COMMA,
            Token::SEMICOLON,
            Token::EOF,
        ];
        let mut lexer = Lexer::new(input);

        tests.iter().for_each(|test_token| {
            let token = lexer.next_token();

            assert_eq!(token, *test_token);
        });
    }
}

use std::mem;

use crate::{
    ast::{Expression, Program, Statement},
    lexer::Lexer,
    token::Token,
};

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            current_token: Token::Illegal,
            peek_token: Token::Illegal,
            errors: Vec::new(),
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };

        while !self.current_token_is(Token::Eof) {
            let statement = self.parse_statement();

            if let Some(statement) = statement {
                program.statements.push(statement);
            }

            self.next_token();
        }

        program
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let identifier = match self.peek_token.clone() {
            Token::Ident(ident) => {
                self.next_token();
                ident
            }
            _ => return None,
        };

        if !self.expect_peek(Token::Assign) {
            return None;
        }

        while !self.current_token_is(Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Let(
            identifier.clone(),
            Expression::Ident(identifier.clone()),
        ))
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            _ => None,
        }
    }

    fn peek_error(&mut self, token: Token) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            token, self.peek_token
        );

        self.errors.push(msg);
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token_is(token.clone()) {
            self.next_token();

            true
        } else {
            self.peek_error(token.clone());

            false
        }
    }

    fn peek_token_is(&self, token: Token) -> bool {
        self.peek_token == token
    }

    fn current_token_is(&self, token: Token) -> bool {
        self.current_token == token
    }

    fn next_token(&mut self) {
        self.current_token = mem::replace(&mut self.peek_token, self.lexer.next_token());
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::{
        ast::{Expression, Statement},
        lexer::Lexer,
    };

    #[test]
    fn let_statements() {
        let input = r#"
let x = 5;
let y = 10;
let foobar = 838383;
"#;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        let tests = [("x", "5"), ("y", "10"), ("foobar", "838383")];

        for (i, test) in tests.iter().enumerate() {
            let statement = &program.statements[i];

            assert_eq!(
                statement,
                &Statement::Let(test.0.to_string(), Expression::Ident(test.0.to_string()))
            );
        }
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();

        if errors.is_empty() {
            return;
        }

        eprintln!("parser has {} errors", errors.len());

        for error in errors {
            eprintln!("parser error: {}", error);
        }

        panic!("parser has errors");
    }
}

use std::fmt;
use std::mem;
use std::result::Result;

use crate::{
    ast::{Expression, Program, Statement},
    lexer::Lexer,
    token::Token,
};

#[derive(Clone)]
pub enum ParserError {
    ExpectedAssign(Token),
    ExpectedIdentifier(Token),
    ExpectedExpression(Token),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::ExpectedAssign(got) => {
                write!(f, "expected assign token, got {}", got)
            }
            ParserError::ExpectedIdentifier(got) => {
                write!(f, "expected identifier token, got {}", got)
            }
            ParserError::ExpectedExpression(got) => {
                write!(f, "expected expression token, got {}", got)
            }
        }
    }
}

pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

type PrefixParserFn = fn(&Parser) -> Option<Expression>;
//type InfixParserFn = fn(&Parser, Expression) -> Option<Expression>;

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>,
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

        while self.current_token != Token::Eof {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(error) => self.errors.push(error),
            };

            self.next_token();
        }

        program
    }

    pub fn errors(&self) -> Vec<ParserError> {
        self.errors.clone()
    }

    fn prefix_parser_fn(&self) -> Option<PrefixParserFn> {
        match &self.current_token {
            Token::Identifier(_) => Some(Parser::parse_identifier),
            _ => None,
        }
    }

    /*
    fn infix_parser_fn(&self, expression: Expression) -> Option<InfixParserFn> {
        match &self.peek_token {
            _ => None,
        }
    }
    */

    fn parse_identifier(&self) -> Option<Expression> {
        match &self.current_token {
            Token::Identifier(ident) => Some(Expression::Idententifier(ident.clone())),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let identifier = match self.peek_token.clone() {
            Token::Identifier(ident) => {
                self.next_token();
                ident
            }
            _ => return Err(ParserError::ExpectedIdentifier(self.peek_token.clone())),
        };

        if !self.expect_peek(Token::Assign) {
            return Err(ParserError::ExpectedAssign(self.peek_token.clone()));
        }

        while self.current_token != Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Let(
            identifier.clone(),
            Expression::Idententifier(identifier.clone()),
        ))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        let statement = Statement::Return(Expression::Idententifier("".to_string()));

        self.next_token();

        while self.current_token != Token::Semicolon {
            self.next_token();
        }

        Ok(statement)
    }

    fn parse_expression(&mut self, _precedence: Precedence) -> Option<Expression> {
        match self.prefix_parser_fn() {
            Some(parser_fn) => parser_fn(self),
            None => None,
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expression = match self.parse_expression(Precedence::Lowest) {
            Some(expression) => expression,
            None => return Err(ParserError::ExpectedExpression(self.current_token.clone())),
        };

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Expression(expression))
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token == token {
            self.next_token();

            true
        } else {
            false
        }
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
    fn identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        let statement = &program.statements[0];

        assert_eq!(
            statement,
            &Statement::Expression(Expression::Idententifier("foobar".to_string()))
        );
    }

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
                &Statement::Let(
                    test.0.to_string(),
                    Expression::Idententifier(test.0.to_string())
                )
            );
        }
    }

    #[test]
    fn return_statements() {
        let input = r#"
            return 5;
            return 10;
            return 993322;
        "#;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        let tests = ["5", "10", "993322"];

        for (i, _test) in tests.iter().enumerate() {
            let statement = &program.statements[i];

            assert_eq!(
                statement,
                &Statement::Return(Expression::Idententifier("".to_string()))
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

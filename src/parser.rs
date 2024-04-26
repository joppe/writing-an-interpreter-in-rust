use std::fmt;
use std::result::Result;

use crate::ast::InfixOperator;
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
    ExpectedPrefixParserFunction(Token),
    ExpectedPrefixExpression(Token),
    ExpectedInfixExpression(Token),
    ExpectedPrefixOperator(Token),
    ExpectedInteger(Token),
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
                write!(f, "expected expression, token {}", got)
            }
            ParserError::ExpectedPrefixParserFunction(got) => {
                write!(f, "expected prefix parser function, token {}", got)
            }
            ParserError::ExpectedPrefixExpression(got) => {
                write!(f, "expected prefix expression, token {}", got)
            }
            ParserError::ExpectedInfixExpression(got) => {
                write!(f, "expected infix expression, token {}", got)
            }
            ParserError::ExpectedPrefixOperator(got) => {
                write!(f, "expected prefix operator token, got {}", got)
            }
            ParserError::ExpectedInteger(got) => {
                write!(f, "failed to parse integer, got {}", got)
            }
        }
    }
}

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precedence {
    fn by_token(token: Token) -> Precedence {
        match token {
            Token::Eq => Precedence::Equals,
            Token::NotEq => Precedence::Equals,
            Token::Lt => Precedence::LessGreater,
            Token::Gt => Precedence::LessGreater,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Slash => Precedence::Product,
            Token::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
}

type PrefixParserFn = fn(&mut Parser) -> Result<Expression, ParserError>;
type InfixParserFn = fn(&mut Parser, Expression) -> Result<Expression, ParserError>;

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
            Token::Int(_) => Some(Parser::parse_integer),
            Token::Bang => Some(Parser::parse_prefix_expression),
            Token::Minus => Some(Parser::parse_prefix_expression),
            _ => None,
        }
    }

    fn infix_parser_fn(&self) -> Option<InfixParserFn> {
        match &self.peek_token {
            Token::Plus => Some(Parser::parse_infix_expression),
            Token::Minus => Some(Parser::parse_infix_expression),
            Token::Slash => Some(Parser::parse_infix_expression),
            Token::Asterisk => Some(Parser::parse_infix_expression),
            Token::Eq => Some(Parser::parse_infix_expression),
            Token::NotEq => Some(Parser::parse_infix_expression),
            Token::Lt => Some(Parser::parse_infix_expression),
            Token::Gt => Some(Parser::parse_infix_expression),
            _ => None,
        }
    }

    fn parse_integer(&mut self) -> Result<Expression, ParserError> {
        match &self.current_token {
            Token::Int(value) => match value.parse::<i64>() {
                Ok(num) => Ok(Expression::Integer(num)),
                Err(_) => Err(ParserError::ExpectedInteger(self.current_token.clone())),
            },
            _ => Err(ParserError::ExpectedInteger(self.current_token.clone())),
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParserError> {
        match &self.current_token {
            Token::Identifier(ident) => Ok(Expression::Idententifier(ident.clone())),
            _ => Err(ParserError::ExpectedIdentifier(self.current_token.clone())),
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

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        let infix_operator = match self.current_token {
            Token::Plus => InfixOperator::Plus,
            Token::Minus => InfixOperator::Minus,
            Token::Asterisk => InfixOperator::Multiply,
            Token::Slash => InfixOperator::Divide,
            Token::Eq => InfixOperator::Equal,
            Token::NotEq => InfixOperator::NotEqual,
            Token::Lt => InfixOperator::LessThan,
            Token::Gt => InfixOperator::GreaterThan,
            _ => {
                return Err(ParserError::ExpectedPrefixOperator(
                    self.current_token.clone(),
                ))
            }
        };
        let precedence = Precedence::by_token(self.current_token.clone());

        self.next_token();

        let right = match self.parse_expression(precedence) {
            Ok(expression) => expression,
            Err(error) => return Err(error),
        };

        Ok(Expression::Infix(
            Box::new(left),
            infix_operator.to_string(),
            Box::new(right),
        ))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        let operator = match &self.current_token {
            Token::Bang => "!",
            Token::Minus => "-",
            _ => {
                return Err(ParserError::ExpectedPrefixOperator(
                    self.current_token.clone(),
                ))
            }
        };

        self.next_token();

        let right = match self.parse_expression(Precedence::Prefix) {
            Ok(expression) => expression,
            Err(error) => return Err(error),
        };

        Ok(Expression::Prefix(operator.to_string(), Box::new(right)))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        let statement = Statement::Return(Expression::Idententifier("".to_string()));

        self.next_token();

        while self.current_token != Token::Semicolon {
            self.next_token();
        }

        Ok(statement)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError> {
        let prefix_parser_fn = match self.prefix_parser_fn() {
            Some(prefix_parser_fn) => prefix_parser_fn,
            None => {
                return Err(ParserError::ExpectedPrefixParserFunction(
                    self.current_token.clone(),
                ))
            }
        };

        let mut left = prefix_parser_fn(self)?;

        while self.peek_token != Token::Semicolon
            && precedence < Precedence::by_token(self.peek_token.clone())
        {
            let infix_parser_fn = match self.infix_parser_fn() {
                Some(infix_parser_fn) => infix_parser_fn,
                None => return Ok(left),
            };

            self.next_token();

            left = infix_parser_fn(self, left)?;
        }

        Ok(left)
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let statement;
        if let Ok(expression) = self.parse_expression(Precedence::Lowest) {
            statement = expression;
        } else {
            return Err(ParserError::ExpectedExpression(self.current_token.clone()));
        }

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Expression(statement))
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
        //self.current_token = mem::replace(&mut self.peek_token, self.lexer.next_token());
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
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
    fn parsing_infix_expressions() {
        let tests = [
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
        ];

        for (_i, test) in tests.iter().enumerate() {
            let lexer = Lexer::new(test.0.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);

            let statement = &program.statements[0];

            assert_eq!(
                statement,
                &Statement::Expression(Expression::Infix(
                    Box::new(Expression::Integer(test.1)),
                    test.2.to_string(),
                    Box::new(Expression::Integer(test.3))
                ))
            );
        }
    }

    #[test]
    fn parsing_prefix_expressions() {
        let tests = [("!5;", "!", 5), ("-15;", "-", 15)];

        for (_i, test) in tests.iter().enumerate() {
            let lexer = Lexer::new(test.0.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);

            let statement = &program.statements[0];

            assert_eq!(
                statement,
                &Statement::Expression(Expression::Prefix(
                    test.1.to_string(),
                    Box::new(Expression::Integer(test.2))
                ))
            )
        }
    }

    #[test]
    fn integer_expression() {
        let input = "5;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(&parser);

        let statement = &program.statements[0];

        assert_eq!(statement, &Statement::Expression(Expression::Integer(5i64)));
    }

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

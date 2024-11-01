use std::fmt;
use std::result::Result;

use crate::ast::{Block, InfixOperator, PrefixOperator};
use crate::{
    ast::{Expression, Program, Statement},
    lexer::Lexer,
    token::Token,
};

#[derive(Clone)]
pub enum ParserError {
    ExpectedAssign(Token),
    ExpectedIdentifier(Token),
    ExpectedBoolean(Token),
    ExpectedExpression(Token),
    ExpectedLeftBrace(Token),
    ExpectedRightBrace(Token),
    ExpectedLeftParen(Token),
    ExpectedRightParen(Token),
    ExpectedPrefixParserFunction(Token),
    ExpectedPrefixExpression(Token),
    ExpectedInfixExpression(Token),
    ExpectedPrefixOperator(Token),
    ExpectedInteger(Token),
    ExpectedString(Token),
    ExpectedClosingBracket(Token),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::ExpectedAssign(got) => {
                write!(f, "expected assign, got {}", got)
            }
            ParserError::ExpectedIdentifier(got) => {
                write!(f, "expected identifier, got {}", got)
            }
            ParserError::ExpectedBoolean(got) => {
                write!(f, "expected boolean, got {}", got)
            }
            ParserError::ExpectedExpression(got) => {
                write!(f, "expected expression, got {}", got)
            }
            ParserError::ExpectedLeftBrace(got) => {
                write!(f, "expected left brace, got {}", got)
            }
            ParserError::ExpectedRightBrace(got) => {
                write!(f, "expected right brace, got {}", got)
            }
            ParserError::ExpectedLeftParen(got) => {
                write!(f, "expected left parenthesis, got {}", got)
            }
            ParserError::ExpectedRightParen(got) => {
                write!(f, "expected right parenthesis, got {}", got)
            }
            ParserError::ExpectedPrefixParserFunction(got) => {
                write!(f, "expected prefix parser function, got {}", got)
            }
            ParserError::ExpectedPrefixExpression(got) => {
                write!(f, "expected prefix expression, got {}", got)
            }
            ParserError::ExpectedInfixExpression(got) => {
                write!(f, "expected infix expression, got {}", got)
            }
            ParserError::ExpectedPrefixOperator(got) => {
                write!(f, "expected prefix operator, got {}", got)
            }
            ParserError::ExpectedInteger(got) => {
                write!(f, "failed to parse integer, got {}", got)
            }
            ParserError::ExpectedString(got) => {
                write!(f, "failed to parse string, got {}", got)
            }
            ParserError::ExpectedClosingBracket(got) => {
                write!(f, "failed to parse string, got {}", got)
            }
        }
    }
}

#[derive(PartialEq, PartialOrd, Debug)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
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
            Token::Lparen => Precedence::Call,
            Token::Lbracket => Precedence::Index,
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
            Token::True => Some(Parser::parse_boolean),
            Token::False => Some(Parser::parse_boolean),
            Token::Lparen => Some(Parser::parse_grouped_expression),
            Token::If => Some(Parser::parse_if_expression),
            Token::Function => Some(Parser::parse_function_literal),
            Token::String(_) => Some(Parser::parse_string_literal),
            Token::Lbracket => Some(Parser::parse_array_literal),
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
            Token::Lparen => Some(Parser::parse_call_expression),
            Token::Lbracket => Some(Parser::parse_index_expression),
            _ => None,
        }
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        self.next_token();

        let index = match self.parse_expression(Precedence::Lowest) {
            Ok(expression) => expression,
            Err(error) => return Err(error),
        };

        if !self.expect_peek(Token::Rbracket) {
            return Err(ParserError::ExpectedClosingBracket(self.peek_token.clone()));
        }

        Ok(Expression::Index(Box::new(left), Box::new(index)))
    }

    fn parse_array_literal(&mut self) -> Result<Expression, ParserError> {
        let elements = match self.parse_expression_list(Token::Rbracket) {
            Ok(elements) => elements,
            Err(error) => return Err(error),
        };

        Ok(Expression::Array(elements))
    }

    fn parse_expression_list(&mut self, end: Token) -> Result<Vec<Expression>, ParserError> {
        let mut elements = Vec::new();

        if self.peek_token == end {
            self.next_token();
            return Ok(elements);
        }

        self.next_token();

        match self.parse_expression(Precedence::Lowest) {
            Ok(expression) => elements.push(expression),
            Err(_) => return Err(ParserError::ExpectedExpression(self.current_token.clone())),
        };

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();

            match self.parse_expression(Precedence::Lowest) {
                Ok(expression) => elements.push(expression),
                Err(_) => return Err(ParserError::ExpectedExpression(self.current_token.clone())),
            };
        }

        if !self.expect_peek(end.clone()) {
            return Err(ParserError::ExpectedClosingBracket(end));
        }

        Ok(elements)
    }

    fn parse_boolean(&mut self) -> Result<Expression, ParserError> {
        match &self.current_token {
            Token::True => Ok(Expression::Boolean(true)),
            Token::False => Ok(Expression::Boolean(false)),
            _ => Err(ParserError::ExpectedExpression(self.current_token.clone())),
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

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParserError> {
        self.next_token();

        let expression = match self.parse_expression(Precedence::Lowest) {
            Ok(expression) => expression,
            Err(error) => return Err(error),
        };

        if !self.expect_peek(Token::Rparen) {
            return Err(ParserError::ExpectedRightParen(self.peek_token.clone()));
        }

        Ok(expression)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ParserError> {
        let arguments = match self.parse_expression_list(Token::Rparen) {
            Ok(arguments) => arguments,
            Err(error) => return Err(error),
        };

        Ok(Expression::Call(Box::new(function), arguments))
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
        if !self.expect_peek(Token::Lparen) {
            return Err(ParserError::ExpectedLeftParen(self.peek_token.clone()));
        }

        self.next_token();

        let condition = match self.parse_expression(Precedence::Lowest) {
            Ok(expression) => expression,
            Err(error) => return Err(error),
        };

        if !self.expect_peek(Token::Rparen) {
            return Err(ParserError::ExpectedRightParen(self.peek_token.clone()));
        }

        if !self.expect_peek(Token::Lbrace) {
            return Err(ParserError::ExpectedLeftBrace(self.peek_token.clone()));
        }

        let consequence = match self.parse_block_statement() {
            Ok(block) => block,
            Err(error) => return Err(error),
        };

        let alternative = if self.peek_token == Token::Else {
            self.next_token();

            if !self.expect_peek(Token::Lbrace) {
                return Err(ParserError::ExpectedLeftBrace(self.peek_token.clone()));
            }

            match self.parse_block_statement() {
                Ok(block) => Some(block),
                Err(error) => return Err(error),
            }
        } else {
            None
        };

        Ok(Expression::If(
            Box::new(condition),
            consequence,
            alternative,
        ))
    }

    fn parse_block_statement(&mut self) -> Result<Block, ParserError> {
        let mut statements = Vec::new();

        self.next_token();

        while self.current_token != Token::Rbrace && self.current_token != Token::Eof {
            match self.parse_statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => return Err(error),
            };

            self.next_token();
        }

        Ok(Block { statements })
    }

    fn parse_string_literal(&mut self) -> Result<Expression, ParserError> {
        match &self.current_token {
            Token::String(value) => Ok(Expression::String(value.to_string())),
            _ => Err(ParserError::ExpectedInteger(self.current_token.clone())),
        }
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ParserError> {
        if !self.expect_peek(Token::Lparen) {
            return Err(ParserError::ExpectedLeftParen(self.peek_token.clone()));
        }

        let parameters = match self.parse_function_parameters() {
            Ok(parameters) => parameters,
            Err(error) => return Err(error),
        };

        if !self.expect_peek(Token::Lbrace) {
            return Err(ParserError::ExpectedLeftBrace(self.peek_token.clone()));
        }

        let body = match self.parse_block_statement() {
            Ok(body) => body,
            Err(error) => return Err(error),
        };

        Ok(Expression::Function(parameters, body))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<String>, ParserError> {
        let mut parameters = Vec::new();

        if self.peek_token == Token::Rparen {
            self.next_token();
            return Ok(parameters);
        }

        self.next_token();

        match &self.current_token {
            Token::Identifier(ident) => parameters.push(ident.clone()),
            _ => return Err(ParserError::ExpectedIdentifier(self.current_token.clone())),
        }

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();

            match &self.current_token {
                Token::Identifier(ident) => parameters.push(ident.clone()),
                _ => return Err(ParserError::ExpectedIdentifier(self.current_token.clone())),
            }
        }

        if !self.expect_peek(Token::Rparen) {
            return Err(ParserError::ExpectedRightParen(self.peek_token.clone()));
        }

        Ok(parameters)
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

        self.next_token();

        let expression = match self.parse_expression(Precedence::Lowest) {
            Ok(expression) => expression,
            Err(error) => return Err(error),
        };

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Let(identifier.clone(), expression.clone()))
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
        let prefix_operator = match &self.current_token {
            Token::Bang => PrefixOperator::Bang,
            Token::Minus => PrefixOperator::Minus,
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

        Ok(Expression::Prefix(
            prefix_operator.to_string(),
            Box::new(right),
        ))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.next_token();

        let expression = match self.parse_expression(Precedence::Lowest) {
            Ok(expression) => expression,
            Err(error) => return Err(error),
        };

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Return(expression))
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
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::{
        ast::{Block, Expression, Statement},
        lexer::Lexer,
    };

    #[test]
    fn test_parsing_index_expressions() {
        let input = "myArray[1 + 1]";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let statement = &program.statements[0];

        assert_eq!(
            statement,
            &Statement::Expression(Expression::Index(
                Box::new(Expression::Idententifier("myArray".to_string())),
                Box::new(Expression::Infix(
                    Box::new(Expression::Integer(1)),
                    "+".to_string(),
                    Box::new(Expression::Integer(1))
                ))
            ))
        )
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let statement = &program.statements[0];

        assert_eq!(
            statement,
            &Statement::Expression(Expression::Array(vec![
                Expression::Integer(1),
                Expression::Infix(
                    Box::new(Expression::Integer(2)),
                    "*".to_string(),
                    Box::new(Expression::Integer(2))
                ),
                Expression::Infix(
                    Box::new(Expression::Integer(3)),
                    "+".to_string(),
                    Box::new(Expression::Integer(3))
                )
            ]))
        )
    }

    #[test]
    fn test_string_literal_expression() {
        let input = "\"hello world\";";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let statement = &program.statements[0];

        assert_eq!(
            statement,
            &Statement::Expression(Expression::String("hello world".to_string()))
        );
    }

    #[test]
    fn call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let statement = &program.statements[0];

        assert_eq!(
            statement,
            &Statement::Expression(Expression::Call(
                Box::new(Expression::Idententifier("add".to_string())),
                vec![
                    Expression::Integer(1),
                    Expression::Infix(
                        Box::new(Expression::Integer(2)),
                        "*".to_string(),
                        Box::new(Expression::Integer(3))
                    ),
                    Expression::Infix(
                        Box::new(Expression::Integer(4)),
                        "+".to_string(),
                        Box::new(Expression::Integer(5))
                    )
                ]
            ))
        )
    }

    #[test]
    fn function_parameter_parsing() {
        let tests = [
            (
                "fn() {};",
                &Statement::Expression(Expression::Function(vec![], Block { statements: vec![] })),
            ),
            (
                "fn(x) {};",
                &Statement::Expression(Expression::Function(
                    vec!["x".to_string()],
                    Block { statements: vec![] },
                )),
            ),
            (
                "fn(x, y, z) {};",
                &Statement::Expression(Expression::Function(
                    vec!["x".to_string(), "y".to_string(), "z".to_string()],
                    Block { statements: vec![] },
                )),
            ),
        ];

        for test in tests.iter() {
            let input = test.0;

            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            let statement = &program.statements[0];

            assert_eq!(statement, test.1);
        }
    }

    #[test]
    fn function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let statement = &program.statements[0];

        assert_eq!(
            statement,
            &Statement::Expression(Expression::Function(
                vec!["x".to_string(), "y".to_string()],
                Block {
                    statements: vec![Statement::Expression(Expression::Infix(
                        Box::new(Expression::Idententifier("x".to_string())),
                        "+".to_string(),
                        Box::new(Expression::Idententifier("y".to_string())),
                    ))]
                }
            ))
        )
    }

    #[test]
    fn if_expression() {
        let input = "if (x < y) { x }";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let statement = &program.statements[0];

        assert_eq!(
            statement,
            &Statement::Expression(Expression::If(
                Box::new(Expression::Infix(
                    Box::new(Expression::Idententifier("x".to_string())),
                    "<".to_string(),
                    Box::new(Expression::Idententifier("y".to_string())),
                )),
                Block {
                    statements: vec![Statement::Expression(Expression::Idententifier(
                        "x".to_string()
                    ))]
                },
                None,
            ))
        )
    }

    #[test]
    fn boolean_expression() {
        let tests = [
            ("true;", &Statement::Expression(Expression::Boolean(true))),
            ("false;", &Statement::Expression(Expression::Boolean(false))),
            (
                "let foobar = true;",
                &Statement::Let("foobar".to_string(), Expression::Boolean(true)),
            ),
            (
                "let foobar = false;",
                &Statement::Let("foobar".to_string(), Expression::Boolean(false)),
            ),
        ];

        for test in tests.iter() {
            let input = test.0;

            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            let statement = &program.statements[0];

            assert_eq!(statement, test.1);
        }
    }

    #[test]
    fn operator_precedence_parsing() {
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for test in tests.iter() {
            let lexer = Lexer::new(test.0.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            assert_eq!(program.to_string(), test.1);
        }
    }

    #[test]
    fn parsing_infix_expressions() {
        let tests = [
            (
                "5 + 5;",
                &Statement::Expression(Expression::Infix(
                    Box::new(Expression::Integer(5)),
                    "+".to_string(),
                    Box::new(Expression::Integer(5)),
                )),
            ),
            (
                "5 - 5;",
                &Statement::Expression(Expression::Infix(
                    Box::new(Expression::Integer(5)),
                    "-".to_string(),
                    Box::new(Expression::Integer(5)),
                )),
            ),
            (
                "5 * 5;",
                &Statement::Expression(Expression::Infix(
                    Box::new(Expression::Integer(5)),
                    "*".to_string(),
                    Box::new(Expression::Integer(5)),
                )),
            ),
            (
                "5 / 5;",
                &Statement::Expression(Expression::Infix(
                    Box::new(Expression::Integer(5)),
                    "/".to_string(),
                    Box::new(Expression::Integer(5)),
                )),
            ),
            (
                "true == true;",
                &Statement::Expression(Expression::Infix(
                    Box::new(Expression::Boolean(true)),
                    "==".to_string(),
                    Box::new(Expression::Boolean(true)),
                )),
            ),
            (
                "true != false;",
                &Statement::Expression(Expression::Infix(
                    Box::new(Expression::Boolean(true)),
                    "!=".to_string(),
                    Box::new(Expression::Boolean(false)),
                )),
            ),
            (
                "false == false;",
                &Statement::Expression(Expression::Infix(
                    Box::new(Expression::Boolean(false)),
                    "==".to_string(),
                    Box::new(Expression::Boolean(false)),
                )),
            ),
        ];

        for test in tests.iter() {
            let lexer = Lexer::new(test.0.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);

            let statement = &program.statements[0];

            assert_eq!(statement, test.1,);
        }
    }

    #[test]
    fn parsing_prefix_expressions() {
        let tests = [
            (
                "!5;",
                &Statement::Expression(Expression::Prefix(
                    "!".to_string(),
                    Box::new(Expression::Integer(5)),
                )),
            ),
            (
                "-15;",
                &Statement::Expression(Expression::Prefix(
                    "-".to_string(),
                    Box::new(Expression::Integer(15)),
                )),
            ),
            (
                "!true;",
                &Statement::Expression(Expression::Prefix(
                    "!".to_string(),
                    Box::new(Expression::Boolean(true)),
                )),
            ),
            (
                "!false;",
                &Statement::Expression(Expression::Prefix(
                    "!".to_string(),
                    Box::new(Expression::Boolean(false)),
                )),
            ),
        ];

        for test in tests.iter() {
            let lexer = Lexer::new(test.0.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);

            let statement = &program.statements[0];

            assert_eq!(statement, test.1)
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
        let tests = [
            (
                "let x = 5;",
                &Statement::Let("x".to_string(), Expression::Integer(5)),
            ),
            (
                "let y = 10;",
                &Statement::Let("y".to_string(), Expression::Integer(10)),
            ),
            (
                "let foobar = y;",
                &Statement::Let(
                    "foobar".to_string(),
                    Expression::Idententifier("y".to_string()),
                ),
            ),
        ];

        for test in tests.iter() {
            let lexer = Lexer::new(test.0.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);

            let statement = &program.statements[0];

            assert_eq!(statement, test.1)
        }
    }

    #[test]
    fn return_statements() {
        let tests = [
            ("return 5;", &Statement::Return(Expression::Integer(5))),
            ("return 10;", &Statement::Return(Expression::Integer(10))),
            (
                "return 993322;",
                &Statement::Return(Expression::Integer(993322)),
            ),
        ];

        for test in tests.iter() {
            let lexer = Lexer::new(test.0.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(&parser);

            let statement = &program.statements[0];

            assert_eq!(statement, test.1)
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

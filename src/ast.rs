use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(String, Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(identifier, expression) => {
                write!(f, "let {} = {};", identifier, expression)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Ident(String),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Ident(identifier) => write!(f, "{}", identifier),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?;
        }

        Ok(())
    }
}

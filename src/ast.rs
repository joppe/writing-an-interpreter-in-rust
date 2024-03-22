use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(String, Expression),
    Return(Expression),
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(identifier, expression) => {
                write!(f, "let {} = {};", identifier, expression)
            }
            Statement::Return(expression) => write!(f, "return {};", expression),
            Statement::Expression(expression) => write!(f, "{}", expression),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Idententifier(String),
    Integer(i64),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Idententifier(identifier) => write!(f, "{}", identifier),
            Expression::Integer(value) => write!(f, "{}", value),
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

#[cfg(test)]
mod tests {
    use super::{Expression, Program, Statement};

    #[test]
    fn to_string() {
        let program = Program {
            statements: vec![
                Statement::Let(
                    "myVar".to_string(),
                    Expression::Idententifier("anotherVar".to_string()),
                ),
                Statement::Return(Expression::Idententifier("myVar".to_string())),
            ],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;return myVar;");
    }
}

use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Error(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
            Object::Return(value) => write!(f, "{}", value),
            Object::Error(message) => write!(f, "{}", message),
        }
    }
}

impl Object {
    pub fn type_name(&self) -> &str {
        match self {
            Object::Integer(_) => "Integer",
            Object::Boolean(_) => "Boolean",
            Object::Null => "Null",
            Object::Return(_) => "Return",
            Object::Error(_) => "Error",
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Null => false,
            Object::Boolean(value) => *value,
            _ => true,
        }
    }
}

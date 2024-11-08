use std::{cell::RefCell, collections::HashMap, fmt, hash::Hash, rc::Rc};

use crate::{ast::Block, environment::Environment};

pub type BuiltinFunction = fn(Vec<Object>) -> Object;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    String(String),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Error(String),
    Function(Vec<String>, Block, Rc<RefCell<Environment>>),
    Builtin(String, BuiltinFunction),
    Array(Vec<Object>),
    Hash(HashMap<HashKey, Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::String(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
            Object::Return(value) => write!(f, "{}", value),
            Object::Function(params, body, _) => {
                write!(f, "fn({}) {{\n {} \n}}", params.join(", "), body)
            }
            Object::Error(message) => write!(f, "{}", message),
            Object::Builtin(name, _) => write!(f, "{}", name),
            Object::Array(elements) => {
                let elements: Vec<String> = elements.iter().map(|e| e.to_string()).collect();
                write!(f, "[{}]", elements.join(", "))
            }
            Object::Hash(pairs) => {
                let pairs: Vec<String> = pairs
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect();
                write!(f, "{{{}}}", pairs.join(", "))
            }
        }
    }
}

impl Object {
    pub fn type_name(&self) -> &str {
        match self {
            Object::Integer(_) => "Integer",
            Object::String(_) => "String",
            Object::Boolean(_) => "Boolean",
            Object::Null => "Null",
            Object::Return(_) => "Return",
            Object::Function(..) => "Function",
            Object::Error(_) => "Error",
            Object::Builtin(..) => "Builtin",
            Object::Array(_) => "Array",
            Object::Hash(_) => "Hash",
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

#[derive(Eq, Hash, Debug, PartialEq, Clone)]
pub enum HashKey {
    String(String),
    Integer(i64),
    Boolean(bool),
}

impl fmt::Display for HashKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HashKey::String(value) => write!(f, "{}", value),
            HashKey::Integer(value) => write!(f, "{}", value),
            HashKey::Boolean(value) => write!(f, "{}", value),
        }
    }
}

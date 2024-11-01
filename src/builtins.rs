use crate::object::{BuiltinFunction, Object};

pub struct Builtins {
    builtins: Vec<(String, BuiltinFunction)>,
}

impl Default for Builtins {
    fn default() -> Self {
        Self::new()
    }
}

impl Builtins {
    fn new() -> Builtins {
        Builtins {
            builtins: vec![
                ("len".to_string(), len),
                ("first".to_string(), first),
                ("last".to_string(), last),
                ("rest".to_string(), rest),
                ("push".to_string(), push),
            ],
        }
    }

    pub fn lookup(name: &str) -> Option<BuiltinFunction> {
        let builtins = Builtins::new();

        builtins
            .builtins
            .iter()
            .find(|(builtin_name, _)| builtin_name == name)
            .map(|(_, builtin_fn)| *builtin_fn)
    }
}

fn len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments, got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::String(value) => Object::Integer(value.len() as i64),
        Object::Array(elements) => Object::Integer(elements.len() as i64),
        _ => Object::Error(format!(
            "argument to 'len' not supported, got {}",
            args[0].type_name()
        )),
    }
}

fn first(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments, got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::Array(elements) => {
            if elements.is_empty() {
                return Object::Null;
            }

            elements[0].clone()
        }
        _ => Object::Error(format!(
            "argument to 'first' must be Array, got {}",
            args[0].type_name()
        )),
    }
}

fn last(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments, got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::Array(elements) => {
            if elements.is_empty() {
                return Object::Null;
            }

            elements[elements.len() - 1].clone()
        }
        _ => Object::Error(format!(
            "argument to 'first' must be Array, got {}",
            args[0].type_name()
        )),
    }
}

fn rest(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments, got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::Array(elements) => {
            if elements.is_empty() {
                return Object::Null;
            }

            let new_elements = elements[1..].to_vec();
            Object::Array(new_elements)
        }
        _ => Object::Error(format!(
            "argument to 'first' must be Array, got {}",
            args[0].type_name()
        )),
    }
}

fn push(args: Vec<Object>) -> Object {
    if args.len() != 2 {
        return Object::Error(format!(
            "wrong number of arguments, got={}, want=2",
            args.len()
        ));
    }

    match &args[0] {
        Object::Array(elements) => {
            let mut new_elements = elements.clone();

            new_elements.push(args[1].clone());

            Object::Array(new_elements)
        }
        _ => Object::Error(format!(
            "argument to 'first' must be Array, got {}",
            args[0].type_name()
        )),
    }
}

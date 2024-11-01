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
            builtins: vec![("len".to_string(), len)],
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
        _ => Object::Error(format!(
            "argument to 'len' not supported, got {}",
            args[0].type_name()
        )),
    }
}

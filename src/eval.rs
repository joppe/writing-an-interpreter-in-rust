use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{Block, Expression, Program, Statement},
    builtins::Builtins,
    environment::Environment,
    object::{HashKey, Object},
};

pub fn eval(program: Program, environment: Rc<RefCell<Environment>>) -> Object {
    let mut result = Object::Null;

    for statement in program.statements {
        result = eval_statement(statement, Rc::clone(&environment));

        match result {
            Object::Error(_) => return result,
            Object::Return(value) => return *value,
            _ => (),
        }
    }

    result
}

fn eval_statement(statement: Statement, environment: Rc<RefCell<Environment>>) -> Object {
    match statement {
        Statement::Expression(expression) => eval_expression(expression, Rc::clone(&environment)),
        Statement::Return(expression) => {
            let value = eval_expression(expression, Rc::clone(&environment));

            if let Object::Error(_) = value {
                return value;
            }

            Object::Return(Box::new(value))
        }
        Statement::Let(identifier, expression) => {
            let value = eval_expression(expression, Rc::clone(&environment));

            if let Object::Error(_) = value {
                return value;
            }

            environment.borrow_mut().set(identifier, value.clone());

            value
        }
    }
}

fn eval_expression(expression: Expression, environment: Rc<RefCell<Environment>>) -> Object {
    match expression {
        Expression::Integer(value) => Object::Integer(value),
        Expression::Boolean(value) => Object::Boolean(value),
        Expression::Prefix(operator, right) => {
            let right = eval_expression(*right, Rc::clone(&environment));

            if let Object::Error(_) = right {
                return right;
            }

            eval_prefix_expression(operator, right)
        }
        Expression::Infix(left, operator, right) => {
            let left = eval_expression(*left, Rc::clone(&environment));
            let right = eval_expression(*right, Rc::clone(&environment));

            if let Object::Error(_) = left {
                return left;
            }

            if let Object::Error(_) = right {
                return right;
            }

            eval_infix_expression(operator, &left, &right)
        }
        Expression::If(condition, consequence, alternative) => {
            let condition = eval_expression(*condition, Rc::clone(&environment));

            if let Object::Error(_) = condition {
                return condition;
            }

            if condition.is_truthy() {
                return eval_block_statement(consequence, Rc::clone(&environment));
            } else if let Some(alternative) = alternative {
                return eval_block_statement(alternative, Rc::clone(&environment));
            }

            Object::Null
        }
        Expression::Idententifier(identifier) => {
            eval_identifier(identifier, Rc::clone(&environment))
        }
        Expression::Function(params, body) => {
            Object::Function(params, body, Rc::clone(&environment))
        }
        Expression::Call(function, arguments) => {
            let function = eval_expression(*function, Rc::clone(&environment));

            if let Object::Error(_) = function {
                return function;
            }

            let arguments = arguments
                .into_iter()
                .map(|argument| eval_expression(argument, Rc::clone(&environment)))
                .collect::<Vec<Object>>();

            if arguments
                .iter()
                .any(|argument| matches!(argument, Object::Error(_)))
            {
                new_error("error evaluating arguments".to_string())
            } else {
                apply_function(function, arguments)
            }
        }
        Expression::String(value) => Object::String(value),
        Expression::Array(elements) => {
            let elements = elements
                .into_iter()
                .map(|element| eval_expression(element, Rc::clone(&environment)))
                .collect::<Vec<Object>>();

            if elements
                .iter()
                .any(|element| matches!(element, Object::Error(_)))
            {
                new_error("error evaluating array elements".to_string())
            } else {
                Object::Array(elements)
            }
        }
        Expression::Index(left, right) => {
            let left = eval_expression(*left, Rc::clone(&environment));
            let right = eval_expression(*right, Rc::clone(&environment));

            if let Object::Error(_) = left {
                return left;
            }

            if let Object::Error(_) = right {
                return right;
            }

            eval_index_expression(left, right)
        }
        Expression::Hash(pairs) => {
            let mut hash = HashMap::new();

            for (key_expression, value_expression) in pairs {
                let key = eval_expression(key_expression, Rc::clone(&environment));

                if let Object::Error(_) = key {
                    return key;
                }

                let key = match key {
                    Object::String(value) => HashKey::String(value),
                    Object::Integer(value) => HashKey::Integer(value),
                    Object::Boolean(value) => HashKey::Boolean(value),
                    _ => {
                        return new_error(format!("unusable as hash key: {}", key.type_name()));
                    }
                };

                let value = eval_expression(value_expression, Rc::clone(&environment));

                if let Object::Error(_) = value {
                    return value;
                }

                hash.insert(key, value);
            }

            Object::Hash(hash)
        }
    }
}

fn eval_index_expression(left: Object, index: Object) -> Object {
    match (left.clone(), index) {
        (Object::Array(array), Object::Integer(index)) => {
            if index < 0 || index >= array.len() as i64 {
                Object::Null
            } else {
                array[index as usize].clone()
            }
        }
        (Object::Hash(hash), index) => match index {
            Object::String(value) => match hash.get(&HashKey::String(value)) {
                Some(value) => value.clone(),
                None => Object::Null,
            },
            Object::Integer(value) => match hash.get(&HashKey::Integer(value)) {
                Some(value) => value.clone(),
                None => Object::Null,
            },
            Object::Boolean(value) => match hash.get(&HashKey::Boolean(value)) {
                Some(value) => value.clone(),
                None => Object::Null,
            },
            _ => new_error(format!("unusable as hash key: {}", index.type_name())),
        },
        _ => new_error(format!(
            "index operator not supported: {}",
            left.type_name()
        )),
    }
}

fn eval_identifier(identifier: String, environment: Rc<RefCell<Environment>>) -> Object {
    match environment.borrow().get(&identifier) {
        Some(value) => value.clone(),
        None => {
            if let Some(builtin) = Builtins::lookup(&identifier) {
                Object::Builtin(identifier, builtin)
            } else {
                new_error(format!("identifier not found: {}", identifier))
            }
        }
    }
}

fn apply_function(function: Object, arguments: Vec<Object>) -> Object {
    match function {
        Object::Function(params, body, environment) => {
            if params.len() != arguments.len() {
                return new_error(format!(
                    "wrong number of arguments: expected={}, got={}",
                    params.len(),
                    arguments.len()
                ));
            }

            let extended_environment = Rc::new(RefCell::new(Environment::extend(environment)));

            for (param, argument) in params.iter().zip(arguments) {
                extended_environment
                    .borrow_mut()
                    .set(param.clone(), argument);
            }

            let result = eval_block_statement(body, extended_environment);

            match result {
                Object::Return(value) => *value,
                _ => result,
            }
        }
        Object::Builtin(_, builtin) => builtin(arguments),
        _ => new_error(format!("not a function: {}", function.type_name())),
    }
}

fn eval_block_statement(block: Block, environment: Rc<RefCell<Environment>>) -> Object {
    let mut result = Object::Null;

    for statement in block.statements {
        result = eval_statement(statement, Rc::clone(&environment));

        match result {
            Object::Error(_) => return result,
            Object::Return(_) => return result,
            _ => (),
        }
    }

    result
}

fn eval_infix_expression(operator: String, left: &Object, right: &Object) -> Object {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expression(operator, *left, *right)
        }
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_boolean_infix_expression(operator, *left, *right)
        }
        (Object::String(left), Object::String(right)) => {
            eval_string_infix_expression(operator, left.to_string(), right.to_string())
        }
        _ => new_error(format!(
            "type mismatch: {} {} {}",
            left.type_name(),
            operator,
            right.type_name()
        )),
    }
}

fn eval_string_infix_expression(operator: String, left: String, right: String) -> Object {
    match operator.as_str() {
        "+" => Object::String(format!("{}{}", left, right)),
        _ => new_error(format!(
            "unknown operator: {} {} {}",
            Object::String(left).type_name(),
            operator,
            Object::String(right).type_name()
        )),
    }
}

fn eval_integer_infix_expression(operator: String, left: i64, right: i64) -> Object {
    match operator.as_str() {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => Object::Boolean(left < right),
        ">" => Object::Boolean(left > right),
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => new_error(format!(
            "unknown operator: {} {} {}",
            Object::Integer(left).type_name(),
            operator,
            Object::Integer(right).type_name()
        )),
    }
}

fn eval_boolean_infix_expression(operator: String, left: bool, right: bool) -> Object {
    match operator.as_str() {
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => new_error(format!(
            "unknown operator: {} {} {}",
            Object::Boolean(left).type_name(),
            operator,
            Object::Boolean(right).type_name()
        )),
    }
}

fn eval_prefix_expression(operator: String, right: Object) -> Object {
    match operator.as_str() {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => new_error(format!(
            "unknown operator: {}{}",
            operator,
            right.type_name()
        )),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(value) => Object::Boolean(!value),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(value) => Object::Integer(-value),
        _ => new_error(format!("unknown operator: -{}", right.type_name())),
    }
}

fn new_error(message: String) -> Object {
    Object::Error(message)
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    use crate::{
        environment::Environment,
        lexer::Lexer,
        object::{HashKey, Object},
        parser::Parser,
    };

    use super::eval;

    #[test]
    fn test_hash_index_expressions() {
        let tests = vec![
            ("{\"foo\": 5}[\"foo\"]", Object::Integer(5)),
            ("{\"foo\": 5}[\"bar\"]", Object::Null),
            ("let key = \"foo\"; {\"foo\": 5}[key]", Object::Integer(5)),
            ("{}[\"foo\"]", Object::Null),
            ("{5: 5}[5]", Object::Integer(5)),
            ("{true: 5}[true]", Object::Integer(5)),
            ("{false: 5}[false]", Object::Integer(5)),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);

            assert_eq!(result, expected);
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = "
            let two = \"two\";
            {
                \"one\": 10 - 9,
                two: 1 + 1,
                \"thr\" + \"ee\": 6 / 2,
                4: 4,
                true: 5,
                false: 6
            }
        ";
        let result = test_eval(input);

        let mut expected = HashMap::new();
        expected.insert(HashKey::String("one".to_string()), Object::Integer(1));
        expected.insert(HashKey::String("two".to_string()), Object::Integer(2));
        expected.insert(HashKey::String("three".to_string()), Object::Integer(3));
        expected.insert(HashKey::Integer(4), Object::Integer(4));
        expected.insert(HashKey::Boolean(true), Object::Integer(5));
        expected.insert(HashKey::Boolean(false), Object::Integer(6));

        assert_eq!(result, Object::Hash(expected));
    }

    #[test]
    fn test_array_index_expression() {
        let tests = vec![
            ("[1, 2, 3][0]", Object::Integer(1)),
            ("[1, 2, 3][1]", Object::Integer(2)),
            ("[1, 2, 3][2]", Object::Integer(3)),
            ("let i = 0; [1][i];", Object::Integer(1)),
            ("[1, 2, 3][1 + 1];", Object::Integer(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Object::Integer(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Object::Integer(2),
            ),
            ("[1, 2, 3][3]", Object::Null),
            ("[1, 2, 3][-1]", Object::Null),
        ];

        for (input, expected) in tests {
            let result = test_eval(input);

            assert_eq!(result, expected);
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let result = test_eval(input);

        assert_eq!(
            result,
            Object::Array(vec![
                Object::Integer(1),
                Object::Integer(4),
                Object::Integer(6)
            ])
        );
    }

    #[test]
    fn test_buildin_functions() {
        let tests = vec![
            ("len(\"\")", Object::Integer(0)),
            ("len(\"four\")", Object::Integer(4)),
            ("len(\"hello world\")", Object::Integer(11)),
            (
                "len(1)",
                Object::Error("argument to 'len' not supported, got Integer".to_string()),
            ),
            (
                "len(\"one\", \"two\")",
                Object::Error("wrong number of arguments, got=2, want=1".to_string()),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            assert_eq!(evaluated.to_string(), expected.to_string());
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World!\"";
        let result = test_eval(input);

        assert_eq!(result, Object::String("Hello World!".to_string()));
    }

    #[test]
    fn test_string_literal() {
        let input = "\"Hello World!\"";
        let result = test_eval(input);

        assert_eq!(result, Object::String("Hello World!".to_string()));
    }

    #[test]
    fn test_closures() {
        let input = "
            let newAdder = fn(x) { 
                fn(y) { x + y }
            }

            let addTwo = newAdder(2);
            addTwo(2);
        ";
        let result = test_eval(input);

        assert_eq!(result, Object::Integer(4));
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            ("fn(x) { x + 2; }(2)", Object::Integer(4)),
            (
                "let identity = fn(x) { x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::Integer(20),
            ),
            ("fn(x) { x; }(5)", Object::Integer(5)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            assert_eq!(evaluated.to_string(), expected.to_string());
        }
    }

    #[test]
    fn test_function_object() {
        let tests = vec![("fn(x) { x + 2; }", "fn(x) {\n (x + 2) \n}")];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            assert_eq!(evaluated.to_string(), expected.to_string());
        }
    }

    #[test]
    fn test_let_statemetns() {
        let tests = vec![
            ("let a = 5; a;", Object::Integer(5)),
            ("let a = 5 * 5; a;", Object::Integer(25)),
            ("let a = 5; let b = a; b;", Object::Integer(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::Integer(15),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            assert_eq!(evaluated.to_string(), expected.to_string());
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: Integer + Boolean"),
            ("5 + true; 5;", "type mismatch: Integer + Boolean"),
            ("-true", "unknown operator: -Boolean"),
            ("true + false;", "unknown operator: Boolean + Boolean"),
            ("5; true + false; 5", "unknown operator: Boolean + Boolean"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: Boolean + Boolean",
            ),
            (
                "
                if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }

                    return 1;
                }
                ",
                "unknown operator: Boolean + Boolean",
            ),
            ("foobar", "identifier not found: foobar"),
            ("\"Hello\" - \"World\"", "unknown operator: String - String"),
            (
                "{\"name\": \"Monkey\"}[fn(x) { x }];",
                "unusable as hash key: Function",
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            assert_eq!(evaluated.to_string(), expected);
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", Object::Integer(10)),
            ("return 10; 9;", Object::Integer(10)),
            ("return 2 * 5; 9;", Object::Integer(10)),
            ("9; return 2 * 5; 9;", Object::Integer(10)),
            (
                "
                if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }

                    return 1;
                }
                ",
                Object::Integer(10),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            assert_eq!(evaluated.to_string(), expected.to_string());
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            assert_eq!(evaluated.to_string(), expected.to_string());
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!!true", true),
            ("!!false", false),
            ("!5", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            assert_eq!(evaluated.to_string(), expected.to_string());
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            assert_eq!(evaluated.to_string(), expected.to_string());
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            assert_eq!(evaluated.to_string(), expected.to_string());
        }
    }

    fn test_eval(input: &str) -> Object {
        let environment = Rc::new(RefCell::new(Environment::new()));
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        eval(program, environment)
    }
}

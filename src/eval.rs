use crate::{
    ast::{Block, Expression, Program, Statement},
    object::Object,
};

pub fn eval(program: Program) -> Object {
    let mut result = Object::Null;

    for statement in program.statements {
        result = eval_statement(statement);

        match result {
            Object::Error(_) => return result,
            Object::Return(value) => return *value,
            _ => (),
        }
    }

    result
}

fn eval_statement(statement: Statement) -> Object {
    match statement {
        Statement::Expression(expression) => eval_expression(expression),
        Statement::Return(expression) => {
            let value = eval_expression(expression);

            if let Object::Error(_) = value {
                return value;
            }

            Object::Return(Box::new(value))
        }
        _ => Object::Null,
    }
}

fn eval_expression(expression: Expression) -> Object {
    match expression {
        Expression::Integer(value) => Object::Integer(value),
        Expression::Boolean(value) => Object::Boolean(value),
        Expression::Prefix(operator, right) => {
            let right = eval_expression(*right);

            if let Object::Error(_) = right {
                return right;
            }

            eval_prefix_expression(operator, right)
        }
        Expression::Infix(left, operator, right) => {
            let left = eval_expression(*left);
            let right = eval_expression(*right);

            if let Object::Error(_) = left {
                return left;
            }

            if let Object::Error(_) = right {
                return right;
            }

            eval_infix_expression(operator, &left, &right)
        }
        Expression::If(condition, consequence, alternative) => {
            let condition = eval_expression(*condition);

            if let Object::Error(_) = condition {
                return condition;
            }

            if condition.is_truthy() {
                return eval_block_statement(consequence);
            } else if let Some(alternative) = alternative {
                return eval_block_statement(alternative);
            }

            Object::Null
        }
        _ => Object::Null,
    }
}

fn eval_block_statement(block: Block) -> Object {
    let mut result = Object::Null;

    for statement in block.statements {
        result = eval_statement(statement);

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
        _ => new_error(format!(
            "type mismatch: {} {} {}",
            left.type_name(),
            operator,
            right.type_name()
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
    use crate::{lexer::Lexer, object::Object, parser::Parser};

    use super::eval;

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
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        eval(program)
    }
}

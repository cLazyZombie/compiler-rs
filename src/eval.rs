use snafu::{Backtrace, Snafu};

use crate::{
    ast::{Expr, Node, Statement},
    environment::Environment,
    object::{BoolObject, FnObject, IntObject, Object, ReturnObject, StringObject},
    token::{IdentToken, Token},
};

#[derive(Debug, Snafu)]
#[snafu(visibility(pub(crate)))]
pub enum EvalError {
    FailedToConvertBoolError {
        original_type: String,
        backtrace: Backtrace,
    },
    FailedToConvertIntError {
        original_type: String,
        backtrace: Backtrace,
    },
    InvalidInfixOperator {
        // todo. left, right object도 argument로
        operator_token: String,
        backtrace: Backtrace,
    },
    UseUndeclaredIdentifier {
        identifier: IdentToken,
        backtrace: Backtrace,
    },
    FnArgCountMismatch {
        expected: usize,
        actual: usize,
        backtrace: Backtrace,
    },
    Todo,
}

pub fn eval<'a, N: Into<Node<'a>>>(node: N, envi: &mut Environment) -> Result<Object, EvalError> {
    let node: Node = node.into();

    match node {
        Node::Program(program) => {
            let mut ret = Object::Null;

            for stmt in &program.statements {
                ret = eval(stmt, envi)?;
            }

            Ok(ret)
        }
        Node::Stmt(stmt) => match stmt {
            Statement::ExprStatement(expr_stmt) => eval(&expr_stmt.expr, envi),
            Statement::BlockStatement(block_stmt) => {
                let mut result = Object::Null;

                // statement
                for stmt in &block_stmt.statements {
                    result = eval(stmt, envi)?;

                    if let Object::Return(_) = &result {
                        return Ok(result);
                    }
                }

                Ok(result)
            }
            Statement::ReturnStatement(ret_stmt) => {
                let inner = eval(&ret_stmt.expr, envi)?;
                let ret = ReturnObject::new(inner);
                Ok(ret.into())
            }
            Statement::LetStatement(let_stmt) => {
                let value = eval(&let_stmt.expr, envi)?;
                envi.set(let_stmt.ident.clone(), value);
                Ok(Object::Null)
            }
        },
        Node::Expr(expr) => match expr {
            Expr::Identifier(ident) => {
                let value = envi.get(&ident.ident);
                if let Some(value) = value {
                    Ok(value)
                } else {
                    UseUndeclaredIdentifierSnafu {
                        identifier: ident.ident.clone(),
                    }
                    .fail()
                }
            }
            Expr::Number(num_expr) => Ok(Object::Int(IntObject::new(num_expr.value))),
            Expr::Bool(bool_expr) => Ok(Object::Bool(BoolObject::new(bool_expr.value))),
            Expr::String(string_expr) => {
                Ok(Object::String(StringObject::new(string_expr.value.clone())))
            }
            Expr::Prefix(prefix_expr) => match prefix_expr.op {
                Token::Bang => {
                    let obj = eval(&*prefix_expr.exp, envi)?;
                    let bool_obj: BoolObject = obj.try_into()?;
                    Ok(bool_obj.bang().into())
                }
                Token::Minus => {
                    let obj = eval(&*prefix_expr.exp, envi)?;
                    let int_obj: IntObject = obj.try_into()?;
                    Ok(int_obj.negate().into())
                }
                _ => {
                    todo!()
                }
            },
            Expr::If(if_expr) => {
                let cond = eval(&*if_expr.condition, envi)?;
                let bool_cond: BoolObject = cond.try_into()?;
                if bool_cond.val {
                    eval(&*if_expr.consequence_statement, envi)
                } else {
                    if let Some(alternative) = &if_expr.alternative_statement {
                        eval(&**alternative, envi)
                    } else {
                        Ok(Object::Null)
                    }
                }
            }
            Expr::Infix(infix_expr) => {
                let left = eval(&*infix_expr.left, envi)?;
                let right = eval(&*infix_expr.right, envi)?;
                match infix_expr.op {
                    Token::Plus => {
                        let value = left.plus(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::Minus => {
                        let value = left.minus(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::Asterrisk => {
                        let value = left.asterrisk(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::Slash => {
                        let value = left.slash(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::Eq => {
                        let value = left.eq(&right).ok_or(
                            InvalidInfixOperatorSnafu {
                                operator_token: format!("invalid operator {} == {}", left, right),
                            }
                            .build(),
                        )?;
                        Ok(value)
                    }
                    Token::NotEq => {
                        let value = left.not_eq(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::LT => {
                        let value = left.lt(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::LTEq => {
                        let value = left.lt_eq(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::GT => {
                        let value = left.gt(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    Token::GTEq => {
                        let value = left.gt_eq(&right).ok_or(EvalError::Todo)?;
                        Ok(value)
                    }
                    _ => InvalidInfixOperatorSnafu {
                        operator_token: infix_expr.op.to_string(),
                    }
                    .fail(),
                }
            }
            Expr::Function(fn_expr) => {
                let stmt = <Statement as Clone>::clone(&fn_expr.body);
                let fn_obj = FnObject {
                    args: fn_expr.args.clone(),
                    body: stmt,
                };
                Ok(fn_obj.into())
            }
            Expr::Call(fn_call) => {
                let fn_obj = eval(&*fn_call.func, envi)?;
                if let Object::Fn(fn_obj) = fn_obj {
                    // check argument count match
                    if fn_call.args.len() != fn_obj.args.len() {
                        return FnArgCountMismatchSnafu {
                            expected: fn_obj.args.len(),
                            actual: fn_call.args.len(),
                        }
                        .fail();
                    }

                    // set parameter to local environment
                    let mut local_envi = Environment::new();
                    for (arg_name, arg_expr) in fn_obj.args.iter().zip(fn_call.args.iter()) {
                        let arg_value = eval(arg_expr, envi)?; // parameter를 eval할때는 local_envi를 사용하면 안됨
                        local_envi.set(arg_name.clone(), arg_value);
                    }

                    local_envi.set_parent(envi);
                    let value = eval(&fn_obj.body, &mut local_envi)?;
                    Ok(value)
                } else {
                    todo!()
                }
            }
            Expr::Array(_) => {
                todo!();
            }
            Expr::ArrayIndex(_) => {
                todo!();
            }
        },
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn eval_int() {
        let inputs = [("5", 5), ("10", 10)];

        for input in inputs {
            let object = eval_input(input.0);
            check_int_object(&object, input.1);
        }
    }

    #[test]
    fn eval_bool() {
        let inputs = [("true", true), ("false", false)];

        for input in inputs {
            let object = eval_input(input.0);
            check_bool_object(&object, input.1);
        }
    }

    #[test]
    fn eval_string() {
        let inputs = [("\"ab\"", "ab"), ("\"ab cd\"", "ab cd")];

        for input in inputs {
            let object = eval_input(input.0);
            check_string_object(&object, input.1);
        }
    }

    #[test]
    fn eval_prefix_bang() {
        let inputs = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!5", true),
        ];

        for input in inputs {
            let object = eval_input(input.0);
            check_bool_object(&object, input.1);
        }
    }

    #[test]
    fn eval_infix_num_ops() {
        let inputs = [
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

        for input in inputs {
            let object = eval_input(input.0);
            check_int_object(&object, input.1);
        }
    }

    #[test]
    fn eval_prefix_minus() {
        let inputs = [("5", 5), ("-5", -5), ("10", 10), ("-10", -10)];

        for input in inputs {
            let object = eval_input(input.0);
            check_int_object(&object, input.1);
        }
    }

    #[test]
    fn eval_equal_not_equal() {
        let inputs = [
            ("1 == 2", false),
            ("1 == 1", true),
            ("1 != 2", true),
            ("1 != 1", false),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("\"aa\" == \"aa\"", true),
            ("\"aa\" == \"ab\"", false),
            ("\"aa\" != \"aa\"", false),
        ];

        for input in inputs {
            let object = eval_input(input.0);
            check_bool_object(&object, input.1);
        }
    }

    #[test]
    fn eval_less_greater() {
        let inputs = [
            ("1 < 2", true),
            ("1 < 1", false),
            ("1 <= 1", true),
            ("1 > 2", false),
            ("1 > 1", false),
            ("1 >= 1", true),
        ];

        for input in inputs {
            let object = eval_input(input.0);
            check_bool_object(&object, input.1);
        }
    }

    #[test]
    fn eval_if() {
        let inputs = [
            ("if (1 < 2) { 1 } else { 2 };", 1),
            ("if (1 > 2) { 1 } else { 2 };", 2),
            ("if (1 > 2) { 1 } else { 2; 3; };", 3),
        ];

        for input in inputs {
            let object = eval_input(input.0);
            check_int_object(&object, input.1);
        }
    }

    #[test]
    fn eval_return() {
        let inputs = [
            ("{ 1; return 2; 3; }", 2),
            ("{ 1; { 2 } { return 3; } 4 }", 3),
        ];

        for input in inputs {
            let object = eval_input(input.0);
            check_int_object(&object, input.1);
        }
    }

    #[test]
    fn eval_let() {
        let inputs = [
            ("let a = 10; a;", 10),
            ("let b = 1; if (b == 1) {10} else {20};", 10),
        ];

        for input in inputs {
            let object = eval_input(input.0);
            check_int_object(&object, input.1);
        }
    }

    #[test]
    fn eval_fn() {
        let inputs = [
            ("let a = fn (a, b) { a + b }; a(1, 2);", 3),
            ("let a = fn (i) { i * 2 }; a(2);", 4),
            (
                "let a = fn (a) { a * 2 }; let b = fn (a, b) { a - b }; b(5, a(2));",
                1,
            ),
            ("fn(x) { x }(5);", 5),
            ("fn(x) { return x * 2; 5; }(5);", 10),
            // ("fn(x) { fn (y) { x + y }; }(1)(2);", 3), // todoß
        ];

        for input in inputs {
            let object = eval_input(input.0);
            check_int_object(&object, input.1);
        }
    }

    fn check_int_object(object: &Object, value: i32) {
        match object {
            Object::Int(int_object) => {
                assert_eq!(int_object.val, value);
            }
            Object::Return(ret_object) => check_int_object(&*ret_object.val, value),
            _ => {
                panic!("expected int object, but {:?}", object);
            }
        }
    }

    fn check_string_object(object: &Object, value: &str) {
        match object {
            Object::String(string_object) => {
                assert_eq!(string_object.val, value);
            }
            Object::Return(ret_object) => check_string_object(&*ret_object.val, value),
            _ => {
                panic!("expected string object, but {:?}", object);
            }
        }
    }

    fn check_bool_object(object: &Object, value: bool) {
        match object {
            Object::Bool(bool_object) => {
                assert_eq!(bool_object.val, value);
            }
            Object::Return(ret_object) => check_bool_object(&*ret_object.val, value),
            _ => {
                panic!("expected bool object, but {:?}", object);
            }
        }
    }

    fn eval_input(input: &str) -> Object {
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        let mut envi = Environment::new();

        let obj = eval(&program, &mut envi).unwrap();
        obj
    }
}

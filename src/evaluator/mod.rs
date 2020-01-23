use {
    crate::parser::ast::Node,
    std::fmt::{Display, Formatter, Result as FmtResult},
};
pub mod tree_walker;

pub trait Evaluator {
    fn eval(&self, node: Node) -> Option<Object>;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Error(String),
    Null,
}

const NULL: Object = Object::Null;
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(v) => v.to_string(),
            Object::Boolean(v) => v.to_string(),
            Object::ReturnValue(ret) => ret.inspect(),
            Object::Error(s) => s.to_string(),
            Object::Null => "NULL".into(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::ReturnValue(_) => "RETURN_VALUE",
            Object::Error(_) => "ERROR",
            Object::Null => "NULL",
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        evaluator::{tree_walker::TreeWalker, Evaluator, Object, FALSE, NULL, TRUE},
        lexer::Lexer,
        parser::{ast::Node, Parser},
    };

    fn run_test_cases(test_cases: &[(&str, Option<Object>)]) {
        for test in test_cases.iter() {
            let lexer = Lexer::new(test.0);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            assert!(program.is_some());
            let program = program.unwrap();
            let evaluator = TreeWalker::new();
            let eval = evaluator.eval(Node::Program(program));
            assert_eq!(eval, test.1);
        }
    }

    #[test]
    fn eval_integer_expression() {
        let test_cases = [
            ("5", Some(Object::Integer(5))),
            ("10", Some(Object::Integer(10))),
        ];

        run_test_cases(&test_cases);
    }

    #[test]
    fn eval_boolean_expression() {
        let test_cases = [
            ("true", Some(TRUE)),
            ("false", Some(FALSE)),
            ("3 < 4", Some(TRUE)),
            ("3 > 4", Some(FALSE)),
            ("3 == 4", Some(FALSE)),
            ("3 != 4", Some(TRUE)),
            ("(1 < 2 ) == true", Some(TRUE)),
            ("(1 < 2 ) == false", Some(FALSE)),
            ("(1 > 2 ) == true", Some(FALSE)),
            ("(1 > 2 ) == false", Some(TRUE)),
        ];
        run_test_cases(&test_cases);
    }

    #[test]
    fn eval_bang_operator() {
        let test_cases = [
            ("!5", Some(FALSE)),
            ("!!true", Some(TRUE)),
            ("!!false", Some(FALSE)),
            ("!!5", Some(TRUE)),
            ("!true", Some(FALSE)),
            ("!false", Some(TRUE)),
        ];
        run_test_cases(&test_cases);
    }

    #[test]
    fn eval_integer_literal_expression() {
        let test_cases = [
            ("5", Some(Object::Integer(5))),
            ("10", Some(Object::Integer(10))),
            ("-5", Some(Object::Integer(-5))),
            ("-10", Some(Object::Integer(-10))),
            ("5 + 5 + 5 + 5 - 10", Some(Object::Integer(10))),
            ("2 * 2 * 2 * 2 * 2", Some(Object::Integer(32))),
            ("-50 + 100 + -50", Some(Object::Integer(0))),
            ("5 * 2 + 10 ", Some(Object::Integer(20))),
            ("5 + 2 * 10", Some(Object::Integer(25))),
            ("20 + 2 * -10", Some(Object::Integer(0))),
            ("50  / 2 * 2 + 10", Some(Object::Integer(60))),
            ("2 * (5 + 10)", Some(Object::Integer(30))),
            ("3 * (3 * 3) + 10", Some(Object::Integer(37))),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Some(Object::Integer(50))),
        ];

        run_test_cases(&test_cases);
    }

    #[test]
    fn eval_if_else_expression() {
        let test_cases = [
            // (
            //     "if (x > 10) {
            //         puts(\"everything okay\");
            //     } else {
            //         puts(\"x is too low!\");
            //         potato();
            //     }",
            //     Object::Null,
            // ),
            ("if(true) {10}", Some(Object::Integer(10))),
            ("if(false) {10}", Some(NULL)),
            ("if (1) {10}", Some(Object::Integer(10))),
            ("if(1 < 2) {10}", Some(Object::Integer(10))),
            ("if (1 > 2) {10}", Some(NULL)),
            ("if(1 > 2) {10} else {20}", Some(Object::Integer(20))),
            ("if (1 < 2) {10} else {20}", Some(Object::Integer(10))),
        ];

        run_test_cases(&test_cases);
    }

    #[test]
    fn eval_return_statements() {
        let test_cases = [
            ("return 10; ", Some(Object::Integer(10))),
            ("return 10; 9;", Some(Object::Integer(10))),
            ("return 2 * 5; 9;", Some(Object::Integer(10))),
            ("9; return 2 * 5; 9;", Some(Object::Integer(10))),
            (
                "if(10 > 1) {
                if(10 > 2) {
                    return 10;
                }
                return 1;
            }",
                Some(Object::Integer(10)),
            ),
        ];

        run_test_cases(&test_cases);
    }

    #[test]
    fn error_handling() {
        let test_cases = [
            (
                "-true",
                Some(Object::Error("unknown operator: -BOOLEAN".into())),
            ),
            (
                "true + false;",
                Some(Object::Error("unknown operator: BOOLEAN + BOOLEAN".into())),
            ),
            (
                "5 + true;",
                Some(Object::Error("type mismatch: INTEGER + BOOLEAN".into())),
            ),
            (
                "5 + true; 5",
                Some(Object::Error("type mismatch: INTEGER + BOOLEAN".into())),
            ),
            (
                "5; true + false; 5",
                Some(Object::Error("unknown operator: BOOLEAN + BOOLEAN".into())),
            ),
            (
                "if (10>1){true + false;}",
                Some(Object::Error("unknown operator: BOOLEAN + BOOLEAN".into())),
            ),
            (
                "if(10 > 1) {
                if(10 > 2) {
                    return true + false;
                }
                return 1;
            }",
                Some(Object::Error("unknown operator: BOOLEAN + BOOLEAN".into())),
            ),
        ];

        run_test_cases(&test_cases);
    }
}

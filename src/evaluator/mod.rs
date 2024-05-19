use {
    crate::parser::ast::{BlockStatement, Identifier, Node},
    itertools::Itertools,
    std::{
        cell::RefCell,
        collections::HashMap,
        fmt::{Display, Formatter, Result as FmtResult, Write},
        rc::Rc,
    },
};
pub mod tree_walker;

pub trait Evaluator {
    fn eval(&self, node: Node, env: Rc<RefCell<Environment>>) -> Option<Object>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }
    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(v) => Some(v.clone()),
            None => match &self.outer {
                Some(outer) => {
                    let outer = outer.borrow();
                    outer.get(name)
                }
                None => None,
            },
        }
    }
    pub fn set(&mut self, name: String, val: Object) {
        self.store.insert(name, val);
    }

    pub fn new_enclosed(env: Rc<RefCell<Environment>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(env),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Error(String),
    Function(Function),
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
            Object::Function(s) => {
                let mut out = String::new();

                out.write_fmt(format_args!(
                    "fn({}) {{ {} }}",
                    s.parameters.iter().map(|x| x.to_string()).join(", "),
                    s.body.to_string()
                ))
                .unwrap();

                out
            }
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
            Object::Function(_) => "FUNCTION",
            Object::Null => "NULL",
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    parameters: Vec<Identifier>,
    body: BlockStatement,
    env: Rc<RefCell<Environment>>,
}

#[cfg(test)]
mod tests {
    use std::{assert_matches::assert_matches, cell::RefCell, rc::Rc};

    use crate::{
        evaluator::{tree_walker::TreeWalker, Environment, Evaluator, Object, FALSE, NULL, TRUE},
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
            let env = Rc::new(RefCell::new(Environment::new()));
            let eval = evaluator.eval(Node::Program(program), env);
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
            (
                "foobar",
                Some(Object::Error("identifier not found: foobar".into())),
            ),
        ];

        run_test_cases(&test_cases);
    }

    #[test]
    fn test_let_statements() {
        let test_cases = [
            ("let a = 5; a;", Some(Object::Integer(5))),
            ("let a = 5*5; a;", Some(Object::Integer(25))),
            ("let a = 5; let b = a; b;", Some(Object::Integer(5))),
            (
                "let a = 5; let b = a; let c = a + b +5; c;",
                Some(Object::Integer(15)),
            ),
        ];

        run_test_cases(&test_cases);
    }

    #[test]
    fn test_function_object() {
        let test_case = "fn(x) { x + 2;};";

        let lexer = Lexer::new(&test_case);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert!(program.is_some());
        let program = program.unwrap();
        let evaluator = TreeWalker::new();
        let env = Rc::new(RefCell::new(Environment::new()));
        let eval = evaluator.eval(Node::Program(program), env);
        let node = eval.unwrap();

        assert_matches!(node, Object::Function(_));

        if let Object::Function(ref f) = node {
            assert_eq!(f.parameters.len(), 1);
            assert_eq!(f.parameters.first().unwrap().to_string(), "x");
            assert_eq!(f.body.to_string(), "(x + 2)");
        }
    }

    #[test]
    fn test_function_application() {
        let test_cases = [
            (
                "let identity = fn(x) { x; }; identity(5);",
                Some(Object::Integer(5)),
            ),
            (
                "let identity = fn(x) { return x; }; identity(9);",
                Some(Object::Integer(9)),
            ),
            (
                "let double = fn(x) { return x * 2; }; double(8);",
                Some(Object::Integer(16)),
            ),
            (
                "let add = fn(x,y) { return x +y; }; add(10, 20);",
                Some(Object::Integer(30)),
            ),
            (
                "let add = fn(x,y) { return x +y; }; add(20 + 25, add(3+1,2)));",
                Some(Object::Integer(51)),
            ),
            ("fn(x) { x; }(5)", Some(Object::Integer(5))),
        ];

        run_test_cases(&test_cases);
    }

    #[test]
    fn test_closure() {
        let test_cases = [(
            "let newAdder = fn(x) {
                fn(y) { x + y ; } ;
            };
            let adder5 = newAdder(5);
            adder5(10);
            ",
            Some(Object::Integer(15)),
        )];

        run_test_cases(&test_cases);
    }

    #[test]
    fn memory_test() {
        // This test case allocates memory for `foobar=9999` over and over
        // even though it is never used.
        let test_cases = [(
            "let counter = fn(x) {
                if (x > 100 ) {
                    return true;
                } else {
                    let foobar = 9999;
                    counter(x+1);
                }
            };
            counter(0);
            ",
            Some(Object::Boolean(true)),
        )];

        run_test_cases(&test_cases);
    }
}

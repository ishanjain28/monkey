use {
    crate::parser::ast::{BlockStatement, Identifier, Node},
    itertools::Itertools,
    std::{
        cell::RefCell,
        collections::BTreeMap,
        fmt::{self, Display, Formatter, Result as FmtResult, Write},
        hash::{Hash, Hasher},
        rc::Rc,
    },
};
pub mod builtins;
pub mod tree_walker;

pub trait Evaluator {
    fn eval(&self, node: Node, env: Rc<RefCell<Environment>>) -> Option<Object>;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Environment {
    store: BTreeMap<String, Object>,
    outer: Option<OuterEnvironment>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct OuterEnvironment(Rc<RefCell<Environment>>);

impl Hash for OuterEnvironment {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.borrow().hash(state);
    }
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: BTreeMap::new(),
            outer: None,
        }
    }
    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(v) => Some(v.clone()),
            None => match &self.outer {
                Some(outer) => {
                    let outer = outer.0.borrow();
                    outer.get(name)
                }
                None => None,
            },
        }
    }
    pub fn set(&mut self, name: String, val: Object) {
        self.store.insert(name, val);
    }

    pub fn new_enclosed(env: OuterEnvironment) -> Self {
        Self {
            store: BTreeMap::new(),
            outer: Some(env),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Ord, PartialOrd, Eq)]
pub enum Object {
    Integer(i64),
    String(String),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Error(String),
    Function(Function),
    Builtin(BuiltinFunction),
    Array(Array),
    Hash(HashObject),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(v) => v.to_string(),
            Object::String(s) => s.to_string(),
            Object::Boolean(v) => v.to_string(),
            Object::ReturnValue(ret) => ret.inspect(),
            Object::Error(s) => s.to_string(),
            Object::Null => "NULL".into(),
            Object::Builtin(_) => "builtin function".to_string(),
            Object::Array(v) => {
                format!("[{}]", v.elements.iter().map(|x| x.to_string()).join(", "))
            }
            Object::Function(s) => {
                let mut out = String::new();

                out.write_fmt(format_args!(
                    "fn({}) {{ {} }}",
                    s.parameters.iter().map(|x| x.to_string()).join(", "),
                    s.body
                ))
                .unwrap();

                out
            }
            Object::Hash(h) => {
                let mut pairs = vec![];

                for (k, v) in h.pairs.iter() {
                    pairs.push(format!("{}: {}", k.inspect(), v.inspect()));
                }

                format!("{{ {} }}", pairs.join(", "))
            }
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(match self {
            Object::Integer(_) => "INTEGER",
            Object::String(_) => "STRING",
            Object::Boolean(_) => "BOOLEAN",
            Object::ReturnValue(_) => "RETURN_VALUE",
            Object::Error(_) => "ERROR",
            Object::Function(_) => "FUNCTION",
            Object::Null => "NULL",
            Object::Builtin(_) => "BUILTIN",
            Object::Array(_) => "ARRAY",
            Object::Hash(_) => "HASH",
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct Function {
    parameters: Vec<Identifier>,
    body: BlockStatement,
    env: OuterEnvironment,
}

type Builtin = fn(Vec<Object>) -> Object;

#[derive(Clone, Hash, Ord, PartialOrd, PartialEq, Eq)]
pub struct BuiltinFunction {
    func: Box<Builtin>,
}

impl fmt::Debug for BuiltinFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_struct("BuiltinFunction")
            .field("func", &"builtin")
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct Array {
    elements: Vec<Object>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct HashObject {
    pairs: BTreeMap<Object, Object>,
}

impl HashObject {
    fn new(ip: impl Into<BTreeMap<Object, Object>>) -> Self {
        Self { pairs: ip.into() }
    }
}

#[cfg(test)]
mod tests {
    use std::{assert_matches::assert_matches, cell::RefCell, rc::Rc};

    use crate::{
        evaluator::{tree_walker::TreeWalker, Environment, Evaluator, Object},
        lexer::Lexer,
        parser::{ast::Node, Parser},
    };

    use super::{Array, HashObject};
    const TRUE: Object = Object::Boolean(true);
    const FALSE: Object = Object::Boolean(false);
    const NULL: Object = Object::Null;

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
    fn eval_string_literal_expression() {
        let test_cases = [("\"Hello \"", Some(Object::String("Hello ".to_owned())))];

        run_test_cases(&test_cases);
    }

    #[test]
    fn eval_string_concatenation() {
        let test_cases = [(
            "\"Hello \" + \"World\"",
            Some(Object::String("Hello World".to_owned())),
        )];

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
            (
                "\"Hello\" - \"World\"",
                Some(Object::Error("unknown operator: STRING - STRING".into())),
            ),
            (
                "{\"name\": \"Monkey\"}[fn(x) {x}];",
                Some(Object::Error("unusable as hash key: FUNCTION".into())),
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

        let lexer = Lexer::new(test_case);
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
                if (x > 30 ) {
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

    #[test]
    fn builtin_function() {
        let test_cases = [
            ("len(\"\")", Some(Object::Integer(0))),
            ("len(\"four\")", Some(Object::Integer(4))),
            ("len(\"hello world\")", Some(Object::Integer(11))),
            ("len([1,2,3,4])", Some(Object::Integer(4))),
            (
                "len(1)",
                Some(Object::Error(
                    "argument to `len` not supported, got INTEGER".to_string(),
                )),
            ),
            (
                "len(\"one\", \"two\")",
                Some(Object::Error(
                    "wrong number of arguments. got=2, want=1".to_string(),
                )),
            ),
            ("first([1,2,3,4])", Some(Object::Integer(1))),
            ("last([1,2,3,4])", Some(Object::Integer(4))),
            (
                "rest([1,2,3,4])",
                Some(Object::Array(Array {
                    elements: vec![Object::Integer(2), Object::Integer(3), Object::Integer(4)],
                })),
            ),
            (
                "push([1,2,3,4])",
                Some(Object::Error(
                    "wrong number of arguments. got=1, want=2".to_string(),
                )),
            ),
            (
                "push([1],5)",
                Some(Object::Array(Array {
                    elements: vec![Object::Integer(1), Object::Integer(5)],
                })),
            ),
        ];

        run_test_cases(&test_cases);
    }

    #[test]
    fn array_literals() {
        let test_cases = [(
            "[1, 2* 2, 3+3]",
            Some(Object::Array(Array {
                elements: vec![Object::Integer(1), Object::Integer(4), Object::Integer(6)],
            })),
        )];

        run_test_cases(&test_cases);
    }

    #[test]
    fn array_index_expressions() {
        let test_cases = [
            ("[1, 2, 3][0]", Some(Object::Integer(1))),
            ("[1, 2, 3][1]", Some(Object::Integer(2))),
            ("[1, 2, 3][2]", Some(Object::Integer(3))),
            ("let i = 0; [1][i];", Some(Object::Integer(1))),
            ("[1,2,3][1+1]", Some(Object::Integer(3))),
            ("let array = [1,2,3]; array[2];", Some(Object::Integer(3))),
            (
                "let array = [1,2,3]; array[0]+array[1]+array[2];",
                Some(Object::Integer(6)),
            ),
            (
                "let array = [1,2,3]; let i = array[0]; array[i];",
                Some(Object::Integer(2)),
            ),
            ("[1,2,3][3];", Some(Object::Null)),
            ("[1,2,3][-1];", Some(Object::Null)),
        ];

        run_test_cases(&test_cases);
    }

    #[test]
    fn hash_literals() {
        let test_cases = [(
            "let two = \"two\";
    {
        \"one\": 10 - 9,
        two: 1 + 1,
        \"thr\" + \"ee\": 6 / 2,
        4: 4,
        true: 5,
        false: 6
    }
    ",
            Some(Object::Hash(HashObject::new([
                (Object::String("one".to_string()), Object::Integer(1)),
                (Object::String("two".to_string()), Object::Integer(2)),
                (Object::String("three".to_string()), Object::Integer(3)),
                (Object::Integer(4), Object::Integer(4)),
                (Object::Boolean(true), Object::Integer(5)),
                (Object::Boolean(false), Object::Integer(6)),
            ]))),
        )];

        run_test_cases(&test_cases);
    }

    #[test]
    fn hash_index_expressions() {
        let test_cases = [
            ("{\"foo\": 5}[\"foo\"]", Some(Object::Integer(5))),
            ("{\"foo\": 5}[\"var\"]", Some(Object::Null)),
            (
                "let key = \"foo\"; {\"foo\": 5}[key]",
                Some(Object::Integer(5)),
            ),
            ("{}[\"foo\"]", Some(Object::Null)),
            ("{5: 5}[5]", Some(Object::Integer(5))),
            ("{true: 5}[true]", Some(Object::Integer(5))),
            ("{false: 5}[false]", Some(Object::Integer(5))),
            ("{true: 5}[false]", Some(Object::Null)),
        ];

        run_test_cases(&test_cases);
    }
}

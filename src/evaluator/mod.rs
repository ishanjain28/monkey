use crate::parser::ast::Node;
pub mod tree_walker;

pub trait Evaluator {
    fn eval(&self, node: Node) -> Option<Object>;
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
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
            Object::Null => "NULL".into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        evaluator::{tree_walker::TreeWalker, Evaluator, Object, FALSE, NULL, TRUE},
        lexer::Lexer,
        parser::{ast::Node, Parser},
    };

    #[test]
    fn eval_integer_expression() {
        let test_cases = [
            ("5", Some(Object::Integer(5))),
            ("10", Some(Object::Integer(10))),
        ];

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
    fn eval_bang_operator() {
        let test_cases = [
            ("!5", Some(FALSE)),
            ("!!true", Some(TRUE)),
            ("!!false", Some(FALSE)),
            ("!!5", Some(TRUE)),
            ("!true", Some(FALSE)),
            ("!false", Some(TRUE)),
        ];
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
}

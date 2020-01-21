use crate::{
    evaluator::{Evaluator, Object, FALSE, NULL, TRUE},
    lexer::TokenType,
    parser::ast::{Expression, ExpressionStatement, Node, Statement},
};

pub struct TreeWalker;

impl TreeWalker {
    pub fn new() -> impl Evaluator {
        Self
    }
}

impl Evaluator for TreeWalker {
    fn eval(&self, node: Node) -> Option<Object> {
        match node {
            Node::Program(p) => self.eval_statements(p.statements),
            Node::Statement(stmt) => match stmt {
                Statement::ExpressionStatement(ExpressionStatement { expression, .. }) => {
                    self.eval(Node::Expression(expression))
                }
                _ => None,
            },
            Node::Expression(expr) => match expr {
                Expression::IntegerLiteral(il) => Some(Object::Integer(il.value)),
                Expression::BooleanExpression(b) => Some(Object::Boolean(b.value)),
                Expression::PrefixExpression(p) => {
                    let expr = self.eval(Node::Expression(*p.right))?;
                    self.eval_prefix_expression(p.operator, expr)
                }
                Expression::InfixExpression(ie) => {
                    let left = self.eval(Node::Expression(*ie.left))?;
                    let right = self.eval(Node::Expression(*ie.right))?;

                    self.eval_infix_expression(left, ie.operator, right)
                }
                _ => None,
            },
        }
    }
}

impl TreeWalker {
    fn eval_statements(&self, stmts: Vec<Statement>) -> Option<Object> {
        let mut out: Option<Object> = None;
        for stmt in stmts {
            out = self.eval(Node::Statement(stmt))
        }
        out
    }

    fn eval_prefix_expression(&self, operator: TokenType, expr: Object) -> Option<Object> {
        match operator {
            TokenType::Bang => Some(self.eval_bang_operator_expression(expr)),
            TokenType::Minus => Some(self.eval_minus_prefix_operator_expression(expr)),
            _ => None,
        }
    }

    fn eval_infix_expression(
        &self,
        left: Object,
        operator: TokenType,
        right: Object,
    ) -> Option<Object> {
        Some(match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => match operator {
                TokenType::Plus => Object::Integer(l + r),
                TokenType::Minus => Object::Integer(l - r),
                TokenType::Asterisk => Object::Integer(l * r),
                TokenType::Slash => Object::Integer(l / r),
                TokenType::Equals => Object::Boolean(l == r),
                TokenType::NotEquals => Object::Boolean(l != r),
                TokenType::GreaterThan => Object::Boolean(l > r),
                TokenType::LessThan => Object::Boolean(l < r),
                _ => NULL,
            },
            _ => match operator {
                TokenType::Equals => Object::Boolean(left == right),
                TokenType::NotEquals => Object::Boolean(left != right),
                _ => NULL,
            },
        })
    }

    fn eval_bang_operator_expression(&self, expr: Object) -> Object {
        match expr {
            TRUE => FALSE,
            FALSE => TRUE,
            NULL => TRUE,
            _ => FALSE,
        }
    }

    fn eval_minus_prefix_operator_expression(&self, expr: Object) -> Object {
        match expr {
            Object::Integer(v) => Object::Integer(-v),
            _ => NULL,
        }
    }
}

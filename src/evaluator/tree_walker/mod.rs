// TODO: This is all a mess. Almost certainly because right now, I don't know any better way to do this.
// It's just constantly unwrapping enums from one place and rewrapping it to some other enum(or even the same enum) and returning it
// The error handling story is pretty bad too
use crate::{
    evaluator::{Evaluator, Object, FALSE, NULL, TRUE},
    lexer::TokenType,
    parser::ast::{BlockStatement, Expression, ExpressionStatement, Node, Program, Statement},
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
            Node::Program(p) => self.eval_program(p),
            Node::Statement(stmt) => match stmt {
                Statement::ExpressionStatement(ExpressionStatement { expression, .. }) => {
                    self.eval(Node::Expression(expression))
                }
                Statement::BlockStatement(bs) => self.eval_block_statement(bs),
                Statement::Return(ret) => {
                    let ret_val = self.eval(Node::Expression(ret.value?))?;
                    Some(Object::ReturnValue(Box::new(ret_val)))
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
                Expression::IfExpression(ie) => {
                    let condition = self.eval(Node::Expression(*ie.condition))?;

                    if self.is_truthy(&condition) {
                        self.eval(Node::Statement(Statement::BlockStatement(ie.consequence)))
                    } else if let Some(alternative) = ie.alternative {
                        self.eval(Node::Statement(Statement::BlockStatement(alternative)))
                    } else {
                        Some(NULL)
                    }
                }
                _ => None,
            },
        }
    }
}

impl TreeWalker {
    fn eval_program(&self, prg: Program) -> Option<Object> {
        let mut out: Option<Object> = None;
        for stmt in prg.statements {
            out = self.eval(Node::Statement(stmt));
            // No need to evaluate any more statements from a statements vector once we
            // get a return keyword. nothing after in the block matters.
            if let Some(out) = out.clone() {
                match out {
                    Object::ReturnValue(v) => return Some(*v),
                    Object::Error(_) => return Some(out),
                    _ => {}
                }
            }
        }
        out
    }

    fn eval_block_statement(&self, bs: BlockStatement) -> Option<Object> {
        let mut out: Option<Object> = None;

        for stmt in bs.statements {
            out = self.eval(Node::Statement(stmt));

            // TODO: Find a nicer way to do this. :(
            // The objective here is,
            // If we encounter a node of type ReturnValue, Don't unwrap it. Return it as is
            // So, It can evaluated again by the eval function.
            // This is helpful when we have a nested structure with multiple return statments.
            // something like,
            // if (true) {
            //      if (true) {
            //          return 10;
            //      }
            //      return 1;
            // }
            // This will return 1 if we unwrap return right here and return the value within.
            // But in reality that shouldn't happen. It should be returning 10
            // So, We don't unwrap the ReturnValue node when we encounter 10. Just return it as is and it is later eval-ed
            // and the correct value is returned
            if let Some(out) = out.clone() {
                match out {
                    Object::ReturnValue(v) => {
                        return Some(Object::ReturnValue(v));
                    }
                    Object::Error(_) => return Some(out),
                    _ => {}
                }
            }
        }
        out
    }

    fn eval_prefix_expression(&self, operator: TokenType, expr: Object) -> Option<Object> {
        match operator {
            TokenType::Bang => Some(self.eval_bang_operator_expression(expr)),
            TokenType::Minus => Some(self.eval_minus_prefix_operator_expression(expr)),
            _ => Some(Object::Error(format!(
                "unknown operator: {}{}",
                operator, expr
            ))),
        }
    }

    fn eval_infix_expression(
        &self,
        left: Object,
        operator: TokenType,
        right: Object,
    ) -> Option<Object> {
        Some(match (left.clone(), right.clone()) {
            (Object::Integer(l), Object::Integer(r)) => match operator {
                TokenType::Plus => Object::Integer(l + r),
                TokenType::Minus => Object::Integer(l - r),
                TokenType::Asterisk => Object::Integer(l * r),
                TokenType::Slash => Object::Integer(l / r),
                TokenType::Equals => Object::Boolean(l == r),
                TokenType::NotEquals => Object::Boolean(l != r),
                TokenType::GreaterThan => Object::Boolean(l > r),
                TokenType::LessThan => Object::Boolean(l < r),
                _ => Object::Error(format!("unknown operator: {} {} {}", l, operator, r)),
            },
            (o1 @ _, o2 @ _) if o1.to_string() != o2.to_string() => {
                Object::Error(format!("type mismatch: {} {} {}", o1, operator, o2))
            }

            _ => match operator {
                TokenType::Equals => Object::Boolean(left == right),
                TokenType::NotEquals => Object::Boolean(left != right),
                _ => Object::Error(format!("unknown operator: {} {} {}", left, operator, right)),
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
            v @ _ => Object::Error(format!("unknown operator: -{}", v)),
        }
    }

    fn is_truthy(&self, obj: &Object) -> bool {
        match *obj {
            NULL => false,
            TRUE => true,
            FALSE => false,
            _ => true,
        }
    }
}

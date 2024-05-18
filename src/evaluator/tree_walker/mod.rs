// TODO: This is all a mess. Almost certainly because right now, I don't know any better way to do this.
// It's just constantly unwrapping enums from one place and rewrapping it to some other enum(or even the same enum) and returning it
// The error handling story is pretty bad too
use crate::{
    evaluator::{Evaluator, Object, FALSE, NULL, TRUE},
    lexer::TokenType,
    parser::ast::{
        BlockStatement, Expression, ExpressionStatement, Identifier, LetStatement, Node, Program,
        Statement,
    },
};

use super::{Environment, Function};

pub struct TreeWalker;

impl TreeWalker {
    pub fn new() -> impl Evaluator {
        Self
    }
}

impl Evaluator for TreeWalker {
    fn eval(&self, node: Node, env: &mut Environment) -> Option<Object> {
        match node {
            Node::Program(p) => self.eval_program(p, env),
            Node::Statement(stmt) => match stmt {
                Statement::ExpressionStatement(ExpressionStatement { expression, .. }) => {
                    self.eval(Node::Expression(expression), env)
                }
                Statement::BlockStatement(bs) => self.eval_block_statement(bs, env),
                Statement::Return(ret) => {
                    let ret_val = self.eval(Node::Expression(ret.value?), env)?;
                    Some(Object::ReturnValue(Box::new(ret_val)))
                }
                Statement::Let(LetStatement { name, value }) => {
                    let value = self.eval(Node::Expression(value?), env)?;
                    env.set(name.to_string(), value.clone());
                    Some(value)
                }
            },
            Node::Expression(expr) => match expr {
                Expression::Identifier(v) => self.eval_identifier(v, env),
                Expression::IntegerLiteral(il) => Some(Object::Integer(il.value)),
                Expression::BooleanExpression(b) => Some(Object::Boolean(b.value)),
                Expression::PrefixExpression(p) => {
                    let expr = self.eval(Node::Expression(*p.right), env)?;
                    self.eval_prefix_expression(p.operator, expr)
                }
                Expression::InfixExpression(ie) => {
                    let left = self.eval(Node::Expression(*ie.left), env)?;
                    let right = self.eval(Node::Expression(*ie.right), env)?;
                    self.eval_infix_expression(left, ie.operator, right)
                }
                Expression::FunctionExpression(fnl) => {
                    return Some(Object::Function(Function {
                        body: fnl.body,
                        parameters: fnl.parameters,
                        env: env.clone(),
                    }))
                }
                Expression::CallExpression(v) => {
                    let function = self.eval(Node::Expression(*v.function), env)?;
                    // Resolve function arguments and update the environment
                    // before executing function body
                    let args = match self.eval_expression(v.arguments, env) {
                        Ok(v) => v,
                        Err(e) => return Some(e),
                    };

                    self.apply_function(function, args)
                }
                Expression::IfExpression(ie) => {
                    let condition = self.eval(Node::Expression(*ie.condition), env)?;

                    if self.is_truthy(&condition) {
                        self.eval(
                            Node::Statement(Statement::BlockStatement(ie.consequence)),
                            env,
                        )
                    } else if let Some(alternative) = ie.alternative {
                        self.eval(Node::Statement(Statement::BlockStatement(alternative)), env)
                    } else {
                        Some(NULL)
                    }
                }
            },
        }
    }
}

impl TreeWalker {
    fn eval_program(&self, prg: Program, env: &mut Environment) -> Option<Object> {
        let mut out: Option<Object> = None;
        for stmt in prg.statements {
            out = self.eval(Node::Statement(stmt), env);
            // No need to evaluate any more statements from a statements vector once we
            // get a return keyword. nothing after in the block matters.
            if let Some(out) = out.as_ref() {
                match out {
                    Object::ReturnValue(v) => return Some(*v.clone()),
                    Object::Error(_) => return Some(out.clone()),
                    _ => {}
                }
            }
        }
        out
    }

    fn eval_block_statement(&self, bs: BlockStatement, env: &mut Environment) -> Option<Object> {
        let mut out: Option<Object> = None;

        for stmt in bs.statements {
            out = self.eval(Node::Statement(stmt), env);

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
            if let Some(out) = out.as_ref() {
                match out {
                    Object::ReturnValue(_) => {
                        return Some(out.clone());
                    }
                    Object::Error(_) => return Some(out.clone()),
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
            (o1, o2) if o1.to_string() != o2.to_string() => {
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
            v => Object::Error(format!("unknown operator: -{}", v)),
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

    fn eval_identifier(&self, node: Identifier, env: &mut Environment) -> Option<Object> {
        env.get(&node.to_string()).or(Some(Object::Error(format!(
            "identifier not found: {}",
            node.to_string()
        ))))
    }

    fn eval_expression(
        &self,
        exprs: Vec<Expression>,
        env: &mut Environment,
    ) -> Result<Vec<Object>, Object> {
        let mut out = vec![];

        for expr in exprs {
            match self.eval(Node::Expression(expr), env) {
                Some(v @ Object::Error(_)) => return Err(v),
                Some(v) => out.push(v),
                None => {
                    break;
                }
            }
        }

        Ok(out)
    }

    fn apply_function(&self, function: Object, args: Vec<Object>) -> Option<Object> {
        let function = match function {
            Object::Function(f) => f,
            Object::Error(e) => {
                return Some(Object::Error(format!("not a function: {}", e.to_string())));
            }
            v => return Some(Object::Error(format!("not a function: {}", v.to_string()))),
        };
        println!("{:?}", function.env);

        let mut enclosed_env = Environment::new_enclosed(function.env);
        for (i, parameter) in function.parameters.iter().enumerate() {
            if args.len() <= i {
                return Some(Object::Error(format!("incorrect number of arguments")));
            }

            enclosed_env.set(parameter.value.clone(), args[i].clone());
        }

        let resp = self.eval(
            Node::Statement(Statement::BlockStatement(function.body)),
            &mut enclosed_env,
        );

        // Unwrap return here to prevent it from bubbling up the stack
        // and stopping execution elsewhere.
        if let Some(Object::ReturnValue(v)) = resp.as_ref() {
            return Some(*v.clone());
        }

        resp
    }
}

use crate::{code::Instructions, evaluator::Object, parser::ast};

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Instructions(vec![]),
            constants: vec![],
        }
    }

    pub fn compile(&self, node: ast::Node) -> Result<(), String> {
        Ok(())
    }

    pub fn bytecode(&self) -> ByteCode {
        ByteCode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }
}

struct ByteCode {
    instructions: Instructions,
    constants: Vec<Object>,
}

#[cfg(test)]
mod test {
    use crate::{
        code::{self, Instructions, OpCode},
        compiler::Compiler,
        evaluator::Object,
        lexer::Lexer,
        parser::{ast::Node, Parser},
    };

    fn run_compiler_tests(
        (input, constants, instructions): (&'static str, Vec<Object>, Vec<Instructions>),
    ) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert!(program.is_some());
        let program = program.unwrap();
        let compiler = Compiler::new();
        compiler.compile(Node::Program(program)).unwrap();
        let bytecode = compiler.bytecode();

        assert_eq!(
            bytecode.instructions,
            instructions
                .into_iter()
                .flat_map(|x| x.0)
                .collect::<Vec<u8>>()
                .into()
        );
        assert_eq!(bytecode.constants, constants);
    }

    #[test]
    fn integer_arithmetic() {
        let tests = [(
            "1+2",
            vec![Object::Integer(1), Object::Integer(2)],
            vec![
                code::make(OpCode::Constant, vec![0]),
                code::make(OpCode::Constant, vec![1]),
            ],
        )];

        for test in tests {
            run_compiler_tests(test);
        }
    }
}

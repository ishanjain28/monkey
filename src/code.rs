use std::{
    fmt::{Display, Formatter, Result as FmtResult},
    ops::{Deref, DerefMut},
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Instructions(pub Vec<u8>);

impl Display for Instructions {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let mut i = 0;

        while i < self.0.len() {
            let opcode = OpCode::lookup(self.0[i]);
            let def = opcode.definition();
            let (operands, read) = read_operands(&def, Instructions(self.0[i + 1..].to_vec()));

            f.write_fmt(format_args!("{:04} {}\n", i, def.name))?;
        }

        f.write_str("")
    }
}

impl Deref for Instructions {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Instructions {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<Vec<u8>> for Instructions {
    fn from(value: Vec<u8>) -> Self {
        Self(value)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum OpCode {
    Constant = 1,
}

impl OpCode {
    pub fn lookup(c: u8) -> Self {
        match c {
            1 => Self::Constant,
            _ => unreachable!(),
        }
    }
}

impl Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.write_str(match self {
            OpCode::Constant => "OpConstant",
        })
    }
}

struct Definition {
    name: String,
    operand_widths: Vec<i32>,
}

impl OpCode {
    fn definition(&self) -> Definition {
        match self {
            OpCode::Constant => Definition {
                name: "Constant".to_string(),
                operand_widths: vec![2],
            },
        }
    }
}

pub fn make(op: OpCode, operands: Vec<i32>) -> Instructions {
    let definition = op.definition();

    let mut instruction_len = 1;
    for op in definition.operand_widths.iter() {
        instruction_len += op;
    }

    let mut instruction = Vec::with_capacity(instruction_len as usize);
    instruction.push(op as u8);

    for (i, operand) in operands.into_iter().enumerate() {
        let width = definition.operand_widths[i];

        match width {
            2 => {
                instruction.extend(&(operand as u16).to_be_bytes());
            }
            _ => unreachable!(),
        }
    }

    Instructions(instruction)
}

fn read_operands(def: &Definition, ins: Instructions) -> (Vec<i32>, i32) {
    let mut operands = vec![];

    let mut offset = 0i32;
    for (i, width) in def.operand_widths.iter().enumerate() {
        match width {
            2 => {
                operands[i] =
                    u16::from_be_bytes([ins.0[offset as usize], ins.0[offset as usize + 1]]) as i32
            }
            _ => unreachable!(),
        }
        offset += width;
    }

    (operands, offset)
}

#[cfg(test)]
mod tests {
    use crate::code::{make, read_operands};

    use super::{Instructions, OpCode};

    #[test]
    fn make_test() {
        let tests = vec![(
            OpCode::Constant,
            vec![65534],
            Instructions(vec![OpCode::Constant as u8, 255, 254]),
        )];

        for (op, operands, expected) in tests {
            let instruction = make(op, operands);

            assert_eq!(instruction, expected);
        }
    }

    #[test]
    fn read_operands_test() {
        let tests = [(OpCode::Constant, vec![65535], 2)];

        for test in tests {
            let instruction = make(test.0, test.1.clone());
            let definition = test.0.definition();

            let (operands_read, n) =
                read_operands(&definition, Instructions(instruction[1..].to_vec()));
            assert_eq!(n, test.2);
            assert_eq!(operands_read, test.1);
        }
    }

    #[test]
    fn instructions_string() {
        let instructions: Instructions = vec![
            make(OpCode::Constant, vec![1]),
            make(OpCode::Constant, vec![2]),
            make(OpCode::Constant, vec![65535]),
        ]
        .into_iter()
        .flat_map(|x| x.0)
        .collect::<Vec<u8>>()
        .into();
        let expected = "0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535";

        assert_eq!(instructions.to_string(), expected);
    }
}

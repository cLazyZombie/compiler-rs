use byteorder::{BigEndian, ByteOrder};
use num_enum::TryFromPrimitive;

use crate::{
    code,
    compiler::Bytecode,
    object::{IntObject, Object},
};

pub struct Vm {
    constants: Vec<Object>,
    instructions: code::Instructions,
    stack: Vec<Object>,
    sp: usize,
}

impl Vm {
    const STACK_SIZE: usize = 1024;

    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: vec![Object::Null; Self::STACK_SIZE],
            sp: 0,
        }
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op_raw = self.instructions[ip];
            let op = code::Opcode::try_from_primitive(op_raw);
            if op.is_err() {
                return Err(VmError::GeneralError(format!(
                    "can not convert {} to Opcode",
                    op_raw
                )));
            }
            let op = op.unwrap();
            ip += 1;

            match op {
                code::Opcode::OpConstant => {
                    let const_index = BigEndian::read_u16(&mut self.instructions[ip..]);
                    ip += 2;

                    self.push(self.constants[const_index as usize].clone())?;
                }
                code::Opcode::OpAdd => {
                    let left = self.pop();
                    let right = self.pop();

                    match (left, right) {
                        (Some(Object::Int(left)), Some(Object::Int(right))) => {
                            let add_object = Object::Int(IntObject::new(left.val + right.val));
                            self.push(add_object)?;
                        }
                        (left, right) => panic!("left: {:?}, right: {:?}", left, right),
                    }
                }
            }
        }
        Ok(())
    }

    fn push(&mut self, obj: Object) -> Result<(), VmError> {
        if self.sp >= Self::STACK_SIZE {
            return Err(VmError::GeneralError("stack overflow".to_string()));
        }

        self.stack[self.sp] = obj;
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> Option<Object> {
        if self.sp == 0 {
            None
        } else {
            let obj = std::mem::take(&mut self.stack[self.sp - 1]);
            self.sp -= 1;
            Some(obj)
        }
    }

    pub fn stack_top(&self) -> &Object {
        if self.sp == 0 {
            &Object::Null
        } else {
            &self.stack[self.sp - 1]
        }
    }
}

#[derive(Debug)]
pub enum VmError {
    GeneralError(String),
}

#[cfg(test)]
mod test {
    use crate::{
        compiler,
        object::{IntObject, Object},
    };

    use super::*;

    #[test]
    fn integer_arithmetic() {
        let input = [
            ("1", Object::Int(IntObject::new(1))),
            ("2", Object::Int(IntObject::new(2))),
            ("1 + 2", Object::Int(IntObject::new(3))),
        ];

        for (i, expected) in input {
            vm_test(i, &expected);
        }
    }

    fn vm_test(s: &str, expected: &Object) {
        let bytecode = compiler::compile(s).unwrap();

        let mut vm = Vm::new(bytecode);
        vm.run().unwrap();

        let top = vm.stack_top();

        assert_eq!(top, expected);
    }
}

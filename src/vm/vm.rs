use byteorder::{BigEndian, ByteOrder};
use num_enum::TryFromPrimitive;

use crate::{
    code::{self, Opcode},
    compiler::Bytecode,
    object::{BoolObject, IntObject, Object, StringObject},
};

use super::GlobalVars;

pub struct Vm<'a> {
    constants: Vec<Object>,
    instructions: code::Instructions,
    stack: Vec<Object>,
    globals: &'a mut GlobalVars,
    sp: usize,
    ip: usize,
    pub last_popped_stack_element: Object,
}

impl<'a> Vm<'a> {
    const STACK_SIZE: usize = 1024;

    pub fn new(bytecode: Bytecode, globals: &'a mut GlobalVars) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: vec![Object::Null; Self::STACK_SIZE],
            globals: globals,
            sp: 0,
            ip: 0,
            last_popped_stack_element: Object::default(),
        }
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        while self.ip < self.instructions.len() {
            let op_raw = self.instructions[self.ip];
            let op = code::Opcode::try_from_primitive(op_raw);
            if op.is_err() {
                return Err(VmError::GeneralError(format!(
                    "can not convert {} to Opcode",
                    op_raw
                )));
            }
            let op = op.unwrap();
            self.ip += 1;

            match op {
                code::Opcode::OpConstant => {
                    let const_index = self.read_u16_from_instructions();
                    self.push(self.constants[const_index as usize].clone())?;
                }
                code::Opcode::OpAdd => {
                    let right = self.pop();
                    let left = self.pop();

                    match (left, right) {
                        (Some(Object::Int(left)), Some(Object::Int(right))) => {
                            let add_object = Object::Int(IntObject::new(left.val + right.val));
                            self.push(add_object)?;
                        }
                        (Some(Object::String(left)), Some(Object::String(right))) => {
                            let string_object =
                                Object::String(StringObject::new(left.val + &right.val));
                            self.push(string_object)?;
                        }
                        (left, right) => panic!("left: {:?}, right: {:?}", left, right),
                    }
                }
                code::Opcode::OpSub => {
                    let right = self.pop();
                    let left = self.pop();

                    match (left, right) {
                        (Some(Object::Int(left)), Some(Object::Int(right))) => {
                            let add_object = Object::Int(IntObject::new(left.val - right.val));
                            self.push(add_object)?;
                        }
                        (left, right) => panic!("left: {:?}, right: {:?}", left, right),
                    }
                }
                code::Opcode::OpMul => {
                    let right = self.pop();
                    let left = self.pop();

                    match (left, right) {
                        (Some(Object::Int(left)), Some(Object::Int(right))) => {
                            let add_object = Object::Int(IntObject::new(left.val * right.val));
                            self.push(add_object)?;
                        }
                        (left, right) => panic!("left: {:?}, right: {:?}", left, right),
                    }
                }
                code::Opcode::OpDiv => {
                    let right = self.pop();
                    let left = self.pop();

                    match (left, right) {
                        (Some(Object::Int(left)), Some(Object::Int(right))) => {
                            let add_object = Object::Int(IntObject::new(left.val / right.val));
                            self.push(add_object)?;
                        }
                        (left, right) => panic!("left: {:?}, right: {:?}", left, right),
                    }
                }
                code::Opcode::OpPop => {
                    self.pop().unwrap();
                }
                Opcode::OpTrue => {
                    self.push(Object::Bool(BoolObject::new(true)))?;
                }
                Opcode::OpFalse => {
                    self.push(Object::Bool(BoolObject::new(false)))?;
                }
                Opcode::OpEqual => {
                    let right = self.pop();
                    let left = self.pop();

                    match (left, right) {
                        (Some(left), Some(right)) => {
                            if let Some(Object::Bool(bool_obj)) = left.eq(&right) {
                                self.push(Object::Bool(BoolObject::new(bool_obj.val)))?;
                            } else {
                                self.push(Object::Bool(BoolObject::new(false)))?;
                            }
                        }
                        _ => {
                            self.push(Object::Bool(BoolObject::new(false)))?;
                        }
                    }
                }
                Opcode::OpNotEqual => {
                    let right = self.pop();
                    let left = self.pop();

                    match (left, right) {
                        (Some(left), Some(right)) => {
                            if let Some(Object::Bool(bool_obj)) = left.eq(&right) {
                                self.push(Object::Bool(BoolObject::new(!bool_obj.val)))?;
                            } else {
                                self.push(Object::Bool(BoolObject::new(true)))?;
                            }
                        }
                        _ => {
                            self.push(Object::Bool(BoolObject::new(false)))?;
                        }
                    }
                }
                Opcode::OpGreaterThan => {
                    let right = self.pop();
                    let left = self.pop();

                    match (left, right) {
                        (Some(left), Some(right)) => {
                            if let Some(Object::Bool(bool_obj)) = left.gt(&right) {
                                self.push(Object::Bool(BoolObject::new(bool_obj.val)))?;
                            } else {
                                self.push(Object::Bool(BoolObject::new(false)))?;
                            }
                        }
                        _ => {
                            self.push(Object::Bool(BoolObject::new(false)))?;
                        }
                    }
                }
                Opcode::OpNegate => {
                    if let Some(operand) = self.pop() {
                        if let Some(negated) = operand.negate() {
                            self.push(negated)?;
                        } else {
                            self.push(Object::Bool(BoolObject::new(false)))?;
                        }
                    }
                }
                Opcode::OpBang => {
                    if let Some(operand) = self.pop() {
                        if let Some(banged) = operand.bang() {
                            self.push(banged)?;
                        }
                    }
                }
                Opcode::OpJumpNotTruthy => {
                    let condition = match self.pop() {
                        Some(Object::Bool(bool_obj)) => bool_obj.val,
                        _ => false,
                    };
                    let jump_addr = self.read_u16_from_instructions();

                    if condition == false {
                        self.ip = jump_addr as usize;
                    }
                }
                Opcode::OpJump => {
                    let jump_addr = self.read_u16_from_instructions();
                    self.ip = jump_addr as usize;
                }
                Opcode::OpNull => {
                    self.push(Object::Null)?;
                }
                Opcode::OpGetGlobal => {
                    let idx = self.read_u16_from_instructions();
                    let global = self.globals.get(idx).unwrap();
                    let clonned = global.clone();
                    self.push(clonned)?;
                }
                Opcode::OpSetGlobal => {
                    let top = self.pop().unwrap();

                    let idx = self.read_u16_from_instructions();
                    let global = self.globals.get_mut(idx).unwrap();

                    *global = top;
                }
            }
        }
        Ok(())
    }

    fn read_u16_from_instructions(&mut self) -> u16 {
        let val = BigEndian::read_u16(&mut self.instructions[self.ip..]);
        self.ip += 2;
        val
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
            self.last_popped_stack_element = Object::Null;
            None
        } else {
            let obj = std::mem::take(&mut self.stack[self.sp - 1]);
            self.last_popped_stack_element = obj.clone();
            self.sp -= 1;
            Some(obj)
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
        object::{IntObject, Object, StringObject},
    };

    use super::*;

    #[test]
    fn integer_arithmetic() {
        let input = [
            ("1", Object::Int(IntObject::new(1))),
            ("2", Object::Int(IntObject::new(2))),
            ("1 + 2", Object::Int(IntObject::new(3))),
            ("1 - 2", Object::Int(IntObject::new(-1))),
            ("2 * 3", Object::Int(IntObject::new(6))),
            ("6 / 3", Object::Int(IntObject::new(2))),
            ("1 + 2 * 3", Object::Int(IntObject::new(7))),
            ("1 * 2 + 3", Object::Int(IntObject::new(5))),
            ("(1 + 2) * 3", Object::Int(IntObject::new(9))),
            ("(-1 + 2) * -3", Object::Int(IntObject::new(-3))),
        ];

        for (i, expected) in input {
            vm_test(i, &expected);
        }
    }

    #[test]
    fn boolean_arithmetic() {
        let input = [
            ("true", Object::Bool(BoolObject::new(true))),
            ("false", Object::Bool(BoolObject::new(false))),
            ("2 > 1", Object::Bool(BoolObject::new(true))),
            ("1 < 2", Object::Bool(BoolObject::new(true))),
            ("1 > 2", Object::Bool(BoolObject::new(false))),
            ("1 > 1", Object::Bool(BoolObject::new(false))),
            ("1 == 1", Object::Bool(BoolObject::new(true))),
            ("1 != 1", Object::Bool(BoolObject::new(false))),
            ("true == true", Object::Bool(BoolObject::new(true))),
            ("false == false", Object::Bool(BoolObject::new(true))),
            ("true != true", Object::Bool(BoolObject::new(false))),
            ("true != false", Object::Bool(BoolObject::new(true))),
            ("(1 < 2) == true", Object::Bool(BoolObject::new(true))),
            ("(1 < 2) == false", Object::Bool(BoolObject::new(false))),
            ("!true", Object::Bool(BoolObject::new(false))),
            ("!!true", Object::Bool(BoolObject::new(true))),
            ("!false", Object::Bool(BoolObject::new(true))),
        ];

        for (i, expected) in input {
            vm_test(i, &expected);
        }
    }

    #[test]
    fn conditional() {
        let input = [
            ("if (true) {10}", Object::Int(IntObject::new(10))),
            ("if (true) {10} else {20}", Object::Int(IntObject::new(10))),
            ("if (false) {10} else {20}", Object::Int(IntObject::new(20))),
            ("if (1 < 2) {10} else {20}", Object::Int(IntObject::new(10))),
            ("if (1 > 2) {10} else {20}", Object::Int(IntObject::new(20))),
            ("if (1 > 2) {10};", Object::Null),
        ];

        for (i, expected) in input {
            vm_test(i, &expected);
        }
    }

    #[test]
    fn let_statement() {
        let input = [
            ("let one = 1; one", Object::Int(IntObject::new(1))),
            (
                "let one = 1; let two = 2; one + two",
                Object::Int(IntObject::new(3)),
            ),
            (
                "let one = 1; let two = one + one; one + two",
                Object::Int(IntObject::new(3)),
            ),
        ];

        for (s, expected) in input {
            vm_test(s, &expected);
        }
    }

    #[test]
    fn string_expr() {
        let input = [
            (
                "\"abc\"",
                Object::String(StringObject::new("abc".to_string())),
            ),
            (
                "\"abc\" + \"def\"",
                Object::String(StringObject::new("abcdef".to_string())),
            ),
            (
                "\"a\" + \"b\" + \"c\"",
                Object::String(StringObject::new("abc".to_string())),
            ),
        ];

        for (s, expected) in input {
            vm_test(s, &expected);
        }
    }

    fn vm_test(s: &str, expected: &Object) {
        let bytecode = compiler::compile(s).unwrap();
        let disassembled = compiler::disassemble(&bytecode.instructions).unwrap();

        let mut global_vars = GlobalVars::new(65535);
        let mut vm = Vm::new(bytecode, &mut global_vars);
        vm.run().unwrap();

        assert_eq!(
            &vm.last_popped_stack_element,
            expected,
            "input: {}, asm: {}, expected: {}",
            s,
            disassembled,
            expected.to_string()
        );
    }
}

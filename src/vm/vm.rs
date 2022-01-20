use std::collections::HashMap;

use crate::{
    code::{self, Opcode},
    compiler::Bytecode,
    object::{BoolObject, IntObject, Object, StringObject},
    vm::Frame,
};

use super::{Constants, Frames, GlobalVars, Stack};

pub struct Vm<'a> {
    constants: Constants,
    stack: Stack,
    frames: Frames,
    globals: &'a mut GlobalVars,
}

impl<'a> Vm<'a> {
    pub fn new(bytecode: Bytecode, globals: &'a mut GlobalVars) -> Self {
        Self {
            constants: Constants::new(bytecode.constants),
            frames: Frames::new(bytecode.instructions),
            globals: globals,
            stack: Stack::new(),
        }
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        while !self.frames.cur_frame().no_more_instructions() {
            let frame = self.frames.cur_frame_mut();
            let op = frame.read_op()?;

            match op {
                code::Opcode::OpConstant => {
                    let const_index = frame.read_u16_from_instructions();
                    self.stack.push(self.constants.get(const_index).clone())?;
                }
                code::Opcode::OpAdd => {
                    let right = self.stack.pop();
                    let left = self.stack.pop();

                    match (left, right) {
                        (Some(Object::Int(left)), Some(Object::Int(right))) => {
                            let add_object = Object::Int(IntObject::new(left.val + right.val));
                            self.stack.push(add_object)?;
                        }
                        (Some(Object::String(left)), Some(Object::String(right))) => {
                            let string_object =
                                Object::String(StringObject::new(left.val + &right.val));
                            self.stack.push(string_object)?;
                        }
                        (left, right) => panic!("left: {:?}, right: {:?}", left, right),
                    }
                }
                code::Opcode::OpSub => {
                    let right = self.stack.pop();
                    let left = self.stack.pop();

                    match (left, right) {
                        (Some(Object::Int(left)), Some(Object::Int(right))) => {
                            let add_object = Object::Int(IntObject::new(left.val - right.val));
                            self.stack.push(add_object)?;
                        }
                        (left, right) => panic!("left: {:?}, right: {:?}", left, right),
                    }
                }
                code::Opcode::OpMul => {
                    let right = self.stack.pop();
                    let left = self.stack.pop();

                    match (left, right) {
                        (Some(Object::Int(left)), Some(Object::Int(right))) => {
                            let add_object = Object::Int(IntObject::new(left.val * right.val));
                            self.stack.push(add_object)?;
                        }
                        (left, right) => panic!("left: {:?}, right: {:?}", left, right),
                    }
                }
                code::Opcode::OpDiv => {
                    let right = self.stack.pop();
                    let left = self.stack.pop();

                    match (left, right) {
                        (Some(Object::Int(left)), Some(Object::Int(right))) => {
                            let add_object = Object::Int(IntObject::new(left.val / right.val));
                            self.stack.push(add_object)?;
                        }
                        (left, right) => panic!("left: {:?}, right: {:?}", left, right),
                    }
                }
                code::Opcode::OpPop => {
                    self.stack.pop().unwrap();
                }
                Opcode::OpTrue => {
                    self.stack.push(Object::Bool(BoolObject::new(true)))?;
                }
                Opcode::OpFalse => {
                    self.stack.push(Object::Bool(BoolObject::new(false)))?;
                }
                Opcode::OpEqual => {
                    let right = self.stack.pop();
                    let left = self.stack.pop();

                    match (left, right) {
                        (Some(left), Some(right)) => {
                            if let Some(Object::Bool(bool_obj)) = left.eq(&right) {
                                self.stack
                                    .push(Object::Bool(BoolObject::new(bool_obj.val)))?;
                            } else {
                                self.stack.push(Object::Bool(BoolObject::new(false)))?;
                            }
                        }
                        _ => {
                            self.stack.push(Object::Bool(BoolObject::new(false)))?;
                        }
                    }
                }
                Opcode::OpNotEqual => {
                    let right = self.stack.pop();
                    let left = self.stack.pop();

                    match (left, right) {
                        (Some(left), Some(right)) => {
                            if let Some(Object::Bool(bool_obj)) = left.eq(&right) {
                                self.stack
                                    .push(Object::Bool(BoolObject::new(!bool_obj.val)))?;
                            } else {
                                self.stack.push(Object::Bool(BoolObject::new(true)))?;
                            }
                        }
                        _ => {
                            self.stack.push(Object::Bool(BoolObject::new(false)))?;
                        }
                    }
                }
                Opcode::OpGreaterThan => {
                    let right = self.stack.pop();
                    let left = self.stack.pop();

                    match (left, right) {
                        (Some(left), Some(right)) => {
                            if let Some(Object::Bool(bool_obj)) = left.gt(&right) {
                                self.stack
                                    .push(Object::Bool(BoolObject::new(bool_obj.val)))?;
                            } else {
                                self.stack.push(Object::Bool(BoolObject::new(false)))?;
                            }
                        }
                        _ => {
                            self.stack.push(Object::Bool(BoolObject::new(false)))?;
                        }
                    }
                }
                Opcode::OpNegate => {
                    if let Some(operand) = self.stack.pop() {
                        if let Some(negated) = operand.negate() {
                            self.stack.push(negated)?;
                        } else {
                            self.stack.push(Object::Bool(BoolObject::new(false)))?;
                        }
                    }
                }
                Opcode::OpBang => {
                    if let Some(operand) = self.stack.pop() {
                        if let Some(banged) = operand.bang() {
                            self.stack.push(banged)?;
                        }
                    }
                }
                Opcode::OpJumpNotTruthy => {
                    let condition = match self.stack.pop() {
                        Some(Object::Bool(bool_obj)) => bool_obj.val,
                        _ => false,
                    };
                    let jump_addr = frame.read_u16_from_instructions();

                    if condition == false {
                        frame.ip = jump_addr as usize;
                    }
                }
                Opcode::OpJump => {
                    let jump_addr = frame.read_u16_from_instructions();
                    frame.ip = jump_addr as usize;
                }
                Opcode::OpNull => {
                    self.stack.push(Object::Null)?;
                }
                Opcode::OpGetGlobal => {
                    let idx = frame.read_u16_from_instructions();
                    let global = self.globals.get(idx).unwrap();
                    let clonned = global.clone();
                    self.stack.push(clonned)?;
                }
                Opcode::OpSetGlobal => {
                    let top = self.stack.pop().unwrap();

                    let idx = frame.read_u16_from_instructions();
                    let global = self.globals.get_mut(idx).unwrap();

                    *global = top;
                }
                Opcode::OpArray => {
                    let array_len = frame.read_u16_from_instructions();
                    let mut array: Vec<Object> = vec![Object::Null; array_len as usize];

                    for i in (0..array.len()).rev() {
                        array[i] = self.stack.pop().unwrap();
                    }

                    let array_object = Object::Array(array.into());
                    self.stack.push(array_object)?;
                }
                Opcode::OpHash => {
                    let count = frame.read_u16_from_instructions();
                    let mut hash: HashMap<Object, Object> = HashMap::new();

                    for _ in 0..count {
                        let value = self.stack.pop().unwrap();
                        let key = self.stack.pop().unwrap();
                        hash.insert(key, value);
                    }

                    let hash_object = Object::Hash(hash.into());
                    self.stack.push(hash_object)?;
                }
                Opcode::OpIndex => {
                    let index = self.stack.pop().unwrap();

                    let collection = self.stack.pop().unwrap();
                    match collection {
                        Object::Array(array_object) => {
                            let index = index.into_u16().unwrap();
                            if let Some(obj) = array_object.array.get(index as usize) {
                                self.stack.push(obj.clone())?;
                            } else {
                                self.stack.push(Object::Null)?;
                            }
                        }
                        Object::Hash(hash_object) => {
                            if let Some(obj) = hash_object.hash.get(&index) {
                                self.stack.push(obj.clone())?;
                            } else {
                                self.stack.push(Object::Null)?;
                            }
                        }
                        _ => {
                            panic!("can not apply OpIndex to {:?}", collection);
                        }
                    }
                }
                Opcode::OpCall => {
                    let fn_object = self.stack.pop();
                    match fn_object {
                        Some(Object::CompiledFn(fn_object)) => {
                            let frame = Frame::new(fn_object.instructions);
                            self.frames.push(frame)?;
                        }
                        _ => panic!("can not apply OpCall to {:?}", fn_object),
                    }
                }
                Opcode::OpReturnValue => {
                    let return_value = self.stack.pop().unwrap();
                    self.frames.pop()?;
                    self.stack.push(return_value)?;
                }
                Opcode::OpReturn => {
                    self.frames.pop()?;
                    self.stack.push(Object::Null)?;
                }
            }
        }
        Ok(())
    }

    pub fn last_popped_stack_element(&self) -> Object {
        self.stack.last_popped_stack_element.clone()
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
        object::{ArrayObject, HashObject, IntObject, Object, StringObject},
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

    #[test]
    fn array_expr() {
        let input = [
            ("[]", Object::Array(ArrayObject::new())),
            (
                "[1, 2, 3]",
                Object::Array(
                    [
                        Object::Int(IntObject::new(1)),
                        Object::Int(IntObject::new(2)),
                        Object::Int(IntObject::new(3)),
                    ]
                    .into(),
                ),
            ),
            (
                "[1 + 2, 3 * 4, 5 + 6]",
                Object::Array(
                    [
                        Object::Int(IntObject::new(3)),
                        Object::Int(IntObject::new(12)),
                        Object::Int(IntObject::new(11)),
                    ]
                    .into(),
                ),
            ),
        ];

        for (s, expected) in input {
            vm_test(s, &expected);
        }
    }

    #[test]
    fn hash_expr() {
        let input = [
            ("{}", Object::Hash(HashObject::new())),
            (
                "{1:10, 2:20}",
                Object::Hash(
                    [
                        (Object::Int(1.into()), Object::Int(10.into())),
                        (Object::Int(2.into()), Object::Int(20.into())),
                    ]
                    .into(),
                ),
            ),
            (
                "{1 + 1 : 2 * 2, 3 + 3 : 4 * 4}",
                Object::Hash(
                    [
                        (Object::Int(2.into()), Object::Int(4.into())),
                        (Object::Int(6.into()), Object::Int(16.into())),
                    ]
                    .into(),
                ),
            ),
        ];

        for (s, expected) in input {
            vm_test(s, &expected);
        }
    }

    #[test]
    fn index_expr() {
        let input = [
            (r#"[1][-1]"#, Object::Null),
            (r#"{1:1, 2:2}[1]"#, Object::Int(1.into())),
            (r#"{1:1, 2:2}[2]"#, Object::Int(2.into())),
            (r#"{1:1, 2:2}[0]"#, Object::Null),
            (r#"[][0]"#, Object::Null),
        ];

        for (s, expected) in input {
            vm_test(s, &expected);
        }
    }

    #[test]
    fn calling_function() {
        let input = [
            (
                r#"let five_plus_ten = fn() { 5 + 10 };
            five_plus_ten();"#,
                Object::Int(15.into()),
            ),
            (
                r#"let one = fn() {1};
            let two = fn() {2};
            one() + two()"#,
                Object::Int(3.into()),
            ),
            (
                r#"let a = fn() { 1 };
                let b = fn() { a() + 1 };
                let c = fn() { b() + 1 };
                c();
                "#,
                Object::Int(3.into()),
            ),
            (
                r#"let early_exit = fn() { return 99; 100; }; early_exit();"#,
                Object::Int(99.into()),
            ),
            (
                r#"let return_null = fn() { }; return_null();"#,
                Object::Null,
            ),
            (
                r#"let return_one = fn() {1;};
                let return_one_returner = fn() { return_one };
                return_one_returner()();
                "#,
                Object::Int(1.into()),
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
            &vm.stack.last_popped_stack_element,
            expected,
            "input: {}, asm: {}, expected: {}",
            s,
            disassembled,
            expected.to_string()
        );
    }
}

use crate::object::Object;

use super::VmError;

#[derive(Debug)]
pub struct Stack {
    stack: Vec<Object>, // todo. 이름 수정
    sp: usize,
    pub last_popped_stack_element: Object,
}

impl Stack {
    const STACK_SIZE: usize = 1024;

    pub fn new() -> Self {
        Self {
            stack: vec![Object::Null; Self::STACK_SIZE],
            sp: 0,
            last_popped_stack_element: Object::Null,
        }
    }

    pub fn push(&mut self, obj: Object) -> Result<(), VmError> {
        if self.sp >= Self::STACK_SIZE {
            return Err(VmError::GeneralError("stack overflow".to_string()));
        }

        self.stack[self.sp] = obj;
        self.sp += 1;

        Ok(())
    }

    pub fn pop(&mut self) -> Option<Object> {
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

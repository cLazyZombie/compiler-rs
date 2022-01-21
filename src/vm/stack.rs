use crate::object::Object;

use super::VmError;

#[derive(Debug)]
pub struct Stack {
    stack: Vec<Object>, // todo. 이름 수정
    pub sp: usize,
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

    pub fn add_sp(&mut self, offset: u16) {
        self.sp += offset as usize;
    }

    pub fn set_local(
        &mut self,
        base_pointer: usize,
        local_index: u8,
        obj: Object,
    ) -> Result<(), VmError> {
        let v = self.stack.get_mut(base_pointer + local_index as usize);
        if let Some(v) = v {
            *v = obj;
            Ok(())
        } else {
            Err(VmError::GeneralError("stack overflow".to_string()))
        }
    }

    pub fn get_local(&self, base_pointer: usize, local_index: u8) -> Result<&Object, VmError> {
        if let Some(obj) = self.stack.get(base_pointer + local_index as usize) {
            Ok(obj)
        } else {
            Err(VmError::GeneralError("stack overflow".to_string()))
        }
    }
}

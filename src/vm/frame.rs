use byteorder::{BigEndian, ByteOrder};
use num_enum::TryFromPrimitive;

use crate::code::{self, Opcode};

use super::VmError;

#[derive(Debug)]
pub struct Frames {
    frames: Vec<Frame>,
}

impl Frames {
    const MAX_FRAME_COUNT: usize = 1024;

    pub fn new(instructions: code::Instructions) -> Self {
        let start_frame = Frame::new(instructions, 0);
        let mut frames = Vec::with_capacity(Self::MAX_FRAME_COUNT);
        frames.push(start_frame);

        Self { frames }
    }

    pub fn push(&mut self, frame: Frame) -> Result<(), VmError> {
        if self.frames.len() >= Self::MAX_FRAME_COUNT {
            return Err(VmError::GeneralError(format!("frame overflow to push")));
        }

        self.frames.push(frame);

        Ok(())
    }

    pub fn pop(&mut self) -> Result<Frame, VmError> {
        if let Some(frame) = self.frames.pop() {
            Ok(frame)
        } else {
            Err(VmError::GeneralError(format!("frame empty to pop")))
        }
    }

    pub fn cur_frame(&self) -> &Frame {
        self.frames.last().unwrap()
    }

    pub fn cur_frame_mut(&mut self) -> &mut Frame {
        self.frames.last_mut().unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct Frame {
    pub instructions: code::Instructions,
    pub ip: usize,
    pub base_pointer: usize,
}

impl Frame {
    pub fn new(instructions: code::Instructions, base_pointer: usize) -> Self {
        Self {
            instructions,
            ip: 0,
            base_pointer,
        }
    }

    pub fn read_op(&mut self) -> Result<Opcode, VmError> {
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

        Ok(op)
    }

    pub fn read_u16_from_instructions(&mut self) -> u16 {
        let val = BigEndian::read_u16(&mut self.instructions[self.ip..]);
        self.ip += 2;
        val
    }

    pub fn read_u8_from_instructions(&mut self) -> u8 {
        let val = self.instructions[self.ip];
        self.ip += 1;
        val
    }

    pub fn no_more_instructions(&self) -> bool {
        self.ip >= self.instructions.len()
    }
}

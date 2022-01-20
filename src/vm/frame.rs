use byteorder::{BigEndian, ByteOrder};

use crate::code;

#[derive(Debug, Clone)]
pub struct Frame {
    pub instructions: code::Instructions,
    pub ip: usize,
}

impl Frame {
    pub fn new(instructions: code::Instructions) -> Self {
        Self {
            instructions,
            ip: 0,
        }
    }

    pub fn read_u16_from_instructions(&mut self) -> u16 {
        let val = BigEndian::read_u16(&mut self.instructions[self.ip..]);
        self.ip += 2;
        val
    }
}

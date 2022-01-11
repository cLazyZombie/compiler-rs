use byteorder::{BigEndian, ByteOrder};
use num_enum::TryFromPrimitive;

pub struct Definition {
    pub name: String,
    pub operand_widths: Vec<i32>,
}

pub type Instructions = Vec<u8>;

#[repr(u8)]
#[derive(Clone, Copy, TryFromPrimitive)]
pub enum Opcode {
    OpConstant,
    OpAdd,
}

impl Opcode {
    // todo. 매번 new 하지 말고 const/static 등으로 수정
    pub fn definition(&self) -> Definition {
        match self {
            Opcode::OpConstant => Definition {
                name: "OpConstant".to_string(),
                operand_widths: vec![2],
            },
            Opcode::OpAdd => Definition {
                name: "OpAdd".to_string(),
                operand_widths: Vec::new(),
            },
        }
    }

    pub fn to_byte(&self) -> u8 {
        *self as u8
    }
}

pub fn make(op: Opcode, operands: &[u16]) -> Vec<u8> {
    let def = op.definition();

    let mut instruction_len = 1;
    for w in &def.operand_widths {
        instruction_len += *w;
    }

    let mut instruction = vec![0; instruction_len as usize];
    instruction[0] = op.to_byte();

    let mut offset = 1;
    for (i, operand) in operands.iter().enumerate() {
        let width = def.operand_widths[i];
        match width {
            2 => {
                BigEndian::write_u16(&mut instruction[offset..], *operand as u16);
            }
            _ => todo!(),
        }
        offset += width as usize;
    }

    instruction
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn opcode_to_u8() {
        let opcode = Opcode::OpConstant;
        assert_eq!(opcode as u8, 0);
    }

    #[test]
    fn test_make() {
        // opcode, operands, expected instructions
        #[rustfmt::skip]
        let tests = [
            ( Opcode::OpConstant, vec![65534_u16], vec![Opcode::OpConstant as u8, 255_u8, 254_u8]),
            ( Opcode::OpAdd, vec![], vec![Opcode::OpAdd as u8]),
        ];

        for t in tests {
            let instruction = make(t.0, &t.1);
            assert_eq!(instruction, t.2);
        }
    }
}

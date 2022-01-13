use byteorder::{BigEndian, ByteOrder};
use num_enum::TryFromPrimitive;

pub struct Definition {
    pub name: &'static str,
    pub operand_widths: Vec<i32>,
}

pub type Instructions = Vec<u8>;

#[repr(u8)]
#[derive(Clone, Copy, TryFromPrimitive)]
pub enum Opcode {
    OpConstant,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpPop,
    OpTrue,
    OpFalse,
    OpEqual,
    OpNotEqual,
    OpGreaterThan,
    OpNegate, // - prefix
    OpBang,   // ! prefix
    OpJumpNotTruthy,
    OpJump,
}

impl Opcode {
    // todo. 매번 new 하지 말고 const/static 등으로 수정
    pub fn definition(&self) -> Definition {
        match self {
            Opcode::OpConstant => Definition {
                name: "OpConstant",
                operand_widths: vec![2],
            },
            Opcode::OpAdd => Definition {
                name: "OpAdd",
                operand_widths: Vec::new(),
            },
            Opcode::OpSub => Definition {
                name: "OpSub",
                operand_widths: Vec::new(),
            },
            Opcode::OpMul => Definition {
                name: "OpMul",
                operand_widths: Vec::new(),
            },
            Opcode::OpDiv => Definition {
                name: "OpDiv",
                operand_widths: Vec::new(),
            },
            Opcode::OpPop => Definition {
                name: "OpPop",
                operand_widths: Vec::new(),
            },
            Opcode::OpTrue => Definition {
                name: "OpTrue",
                operand_widths: Vec::new(),
            },
            Opcode::OpFalse => Definition {
                name: "OpFalse",
                operand_widths: Vec::new(),
            },
            Opcode::OpEqual => Definition {
                name: "OpEqual",
                operand_widths: Vec::new(),
            },
            Opcode::OpNotEqual => Definition {
                name: "OpNotEqual",
                operand_widths: Vec::new(),
            },
            Opcode::OpGreaterThan => Definition {
                name: "OpGreaterThan",
                operand_widths: Vec::new(),
            },
            Opcode::OpNegate => Definition {
                name: "OpNegate",
                operand_widths: Vec::new(),
            },
            Opcode::OpBang => Definition {
                name: "OpBang",
                operand_widths: Vec::new(),
            },
            Opcode::OpJumpNotTruthy => Definition {
                name: "OpJumpNotTruthy",
                operand_widths: vec![2],
            },
            Opcode::OpJump => Definition {
                name: "OpJump",
                operand_widths: vec![2],
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
            ( Opcode::OpSub, vec![], vec![Opcode::OpSub as u8]),
            ( Opcode::OpMul, vec![], vec![Opcode::OpMul as u8]),
            ( Opcode::OpDiv, vec![], vec![Opcode::OpDiv as u8]),
            ( Opcode::OpPop, vec![], vec![Opcode::OpPop as u8]),
            ( Opcode::OpTrue, vec![], vec![Opcode::OpTrue as u8]),
            ( Opcode::OpFalse, vec![], vec![Opcode::OpFalse as u8]),
            ( Opcode::OpEqual, vec![], vec![Opcode::OpEqual as u8]),
            ( Opcode::OpNotEqual, vec![], vec![Opcode::OpNotEqual as u8]),
            ( Opcode::OpGreaterThan, vec![], vec![Opcode::OpGreaterThan as u8]),
            ( Opcode::OpNegate, vec![], vec![Opcode::OpNegate as u8]),
            ( Opcode::OpBang, vec![], vec![Opcode::OpBang as u8]),
            ( Opcode::OpJumpNotTruthy, vec![100], vec![Opcode::OpJumpNotTruthy as u8, 0, 100]),
            ( Opcode::OpJump, vec![200], vec![Opcode::OpJump as u8, 0, 200]),
        ];

        for t in tests {
            let instruction = make(t.0, &t.1);
            assert_eq!(instruction, t.2);
        }
    }
}

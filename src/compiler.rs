use byteorder::{BigEndian, ByteOrder};

use crate::{
    ast::{self, Node},
    code,
    lexer::Lexer,
    object::{self, IntObject},
    parser::Parser,
};

pub struct Compiler {
    pub instructions: code::Instructions,
    pub constants: Vec<object::Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: code::Instructions::new(),
            constants: Vec::new(),
        }
    }

    pub fn compile<'a, N: Into<Node<'a>>>(&mut self, node: N) -> Result<(), CompileError> {
        let node: Node = node.into();
        match node {
            ast::Node::Stmt(stmt) => match stmt {
                ast::Statement::ExprStatement(expr_stmt) => self.compile(&expr_stmt.expr)?,
                _ => todo!(),
            },
            ast::Node::Expr(expr) => match expr {
                ast::Expr::Number(num_expr) => {
                    let num_obj = object::Object::Int(IntObject::new(num_expr.value));
                    let const_idx = self.add_constant(num_obj);
                    let _ins_idx = self.emit(code::Opcode::OpConstant, &[const_idx]);
                }
                ast::Expr::Infix(infix_expr) => {
                    self.compile(&*infix_expr.left)?;
                    self.compile(&*infix_expr.right)?;
                }
                _ => {}
            },
        }

        Ok(())
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }

    fn add_constant(&mut self, constant: object::Object) -> u16 {
        let idx = self.constants.len();
        self.constants.push(constant);
        idx as u16
    }

    fn emit(&mut self, op: code::Opcode, operands: &[u16]) -> u16 {
        let idx = self.instructions.len();
        let mut ins = code::make(op, operands);
        self.instructions.append(&mut ins);
        idx as u16
    }
}

pub fn compile(s: &str) -> Result<Bytecode, CompileError> {
    let mut compiler = Compiler::new();

    let lexer = Lexer::new(s);
    let parser = Parser::new(lexer);
    let program = parser.parse_program().unwrap();
    let stmts = program.take_statement();
    for stmt in &stmts {
        let node: Node = stmt.into();
        compiler.compile(node)?;
    }

    let bytecode = compiler.bytecode();
    Ok(bytecode)
}

pub struct Bytecode {
    pub instructions: code::Instructions,
    pub constants: Vec<object::Object>,
}

impl Bytecode {
    pub fn append(&mut self, mut other: Bytecode) {
        self.instructions.append(&mut other.instructions);
        self.constants.append(&mut other.constants);
    }
}

#[derive(Debug)]
pub enum CompileError {
    GeneralError(String),
}

#[derive(Debug)]
pub enum DisassembleError {
    InvalidOpcode(u8),
    GeneralError(String),
}

use std::fmt::Write;

pub fn disassemble(instructions: &code::Instructions) -> Result<String, DisassembleError> {
    let mut result = String::new();
    let mut idx = 0;
    while idx < instructions.len() {
        let op_byte = instructions[idx];
        let op_offset = idx;
        idx += 1;
        let op = code::Opcode::try_from(op_byte);
        match op {
            Err(_err) => return Err(DisassembleError::InvalidOpcode(op_byte)),
            Ok(op) => {
                let def = op.definition();
                let (operands, operands_idx) = read_operand(&def, &instructions[idx..]);

                match operands.len() {
                    1 => {
                        let _ =
                            writeln!(&mut result, "{:04} {} {}", op_offset, def.name, operands[0]);
                    }
                    _ => todo!(),
                }

                idx += operands_idx as usize;
            }
        }
    }

    Ok(result)
}

fn read_operand(def: &code::Definition, ins: &[u8]) -> (Vec<i32>, i32) {
    let mut operands = vec![0; def.operand_widths.len()];
    let mut offset = 0;

    for (idx, w) in def.operand_widths.iter().enumerate() {
        match *w {
            2 => {
                operands[idx] = BigEndian::read_u16(&ins[offset as usize..]) as i32;
            }
            _ => todo!(),
        }
        offset += *w;
    }

    (operands, offset)
}

#[cfg(test)]
mod tests {
    use crate::{
        code::Opcode,
        object::{IntObject, Object},
    };

    use super::*;

    #[test]
    fn disassemble_instructions() {
        let mut instructions = Vec::new();

        instructions.append(&mut code::make(Opcode::OpConstant, &[1]));
        instructions.append(&mut code::make(Opcode::OpConstant, &[2]));
        instructions.append(&mut code::make(Opcode::OpConstant, &[65535]));

        let expected = r#"0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535
"#;

        assert_eq!(disassemble(&instructions).unwrap(), expected);
    }

    #[test]
    fn integer_arithmetic() {
        let cases = [
            // input, constants, expected instructions(
            (
                "1 + 2".to_string(),
                &[
                    Object::Int(IntObject::new(1)),
                    Object::Int(IntObject::new(2)),
                ],
                &[
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpConstant, &[1]),
                ],
            ),
        ];

        for (input, constants, expected) in cases {
            let bytecode = compile(&input).unwrap();
            assert_eq!(&bytecode.constants, constants);
            check_instructions_eq(&bytecode.instructions, expected);
        }
    }

    fn check_instructions_eq(ins: &Vec<u8>, other: &[Vec<u8>]) {
        // let joined = other.iter().flatten().collect::<Vec<_>>();
        let joined = other.iter().flat_map(|v| v.clone()).collect::<Vec<_>>();
        assert_eq!(ins, &joined);
    }
}

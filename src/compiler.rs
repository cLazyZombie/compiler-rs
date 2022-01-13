use byteorder::{BigEndian, ByteOrder};

use crate::{
    ast::{self, Node},
    code::{self, Opcode},
    lexer::Lexer,
    object::{self, IntObject},
    parser::Parser,
    token::Token,
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
            ast::Node::Program(program) => {
                for stmt in &program.statements {
                    self.compile(stmt)?;
                }
            }
            ast::Node::Stmt(stmt) => match stmt {
                ast::Statement::ExprStatement(expr_stmt) => {
                    self.compile(&expr_stmt.expr)?;
                    self.emit(Opcode::OpPop, &[]);
                }
                ast::Statement::BlockStatement(block_stmt) => {
                    for stmt in &block_stmt.statements {
                        self.compile(stmt)?;
                    }
                }
                _ => {
                    panic!("{:?} is not yet implemented", stmt);
                }
            },
            ast::Node::Expr(expr) => match expr {
                ast::Expr::Number(num_expr) => {
                    let num_obj = object::Object::Int(IntObject::new(num_expr.value));
                    let const_idx = self.add_constant(num_obj);
                    let _ins_idx = self.emit(Opcode::OpConstant, &[const_idx]);
                }
                ast::Expr::Bool(bool_expr) => {
                    if bool_expr.value {
                        self.emit(Opcode::OpTrue, &[]);
                    } else {
                        self.emit(Opcode::OpFalse, &[]);
                    }
                }
                ast::Expr::Infix(infix_expr) => match &infix_expr.op {
                    Token::Plus => {
                        self.compile(&*infix_expr.left)?;
                        self.compile(&*infix_expr.right)?;
                        let _ins_idx = self.emit(Opcode::OpAdd, &[]);
                    }
                    Token::Minus => {
                        self.compile(&*infix_expr.left)?;
                        self.compile(&*infix_expr.right)?;
                        let _inx_idx = self.emit(Opcode::OpSub, &[]);
                    }
                    Token::Asterrisk => {
                        self.compile(&*infix_expr.left)?;
                        self.compile(&*infix_expr.right)?;
                        let _inx_idx = self.emit(Opcode::OpMul, &[]);
                    }
                    Token::Slash => {
                        self.compile(&*infix_expr.left)?;
                        self.compile(&*infix_expr.right)?;
                        let _inx_idx = self.emit(Opcode::OpDiv, &[]);
                    }
                    Token::GT => {
                        self.compile(&*infix_expr.left)?;
                        self.compile(&*infix_expr.right)?;
                        self.emit(Opcode::OpGreaterThan, &[]);
                    }
                    Token::LT => {
                        // LT 은 GT의 인자 위치만 바꿈
                        self.compile(&*infix_expr.right)?;
                        self.compile(&*infix_expr.left)?;
                        self.emit(Opcode::OpGreaterThan, &[]);
                    }
                    Token::Eq => {
                        self.compile(&*infix_expr.left)?;
                        self.compile(&*infix_expr.right)?;
                        self.emit(Opcode::OpEqual, &[]);
                    }
                    Token::NotEq => {
                        self.compile(&*infix_expr.left)?;
                        self.compile(&*infix_expr.right)?;
                        self.emit(Opcode::OpNotEqual, &[]);
                    }
                    _ => {
                        panic!("not implemented op {}", infix_expr.op);
                    }
                },
                ast::Expr::Prefix(prefix_expr) => match prefix_expr.op {
                    Token::Minus => {
                        self.compile(&*prefix_expr.exp)?;
                        self.emit(Opcode::OpNegate, &[]);
                    }
                    Token::Bang => {
                        self.compile(&*prefix_expr.exp)?;
                        self.emit(Opcode::OpBang, &[]);
                    }
                    _ => {
                        panic!("not implemented op {}", prefix_expr.op);
                    }
                },
                ast::Expr::If(if_expr) => {
                    self.compile(&*if_expr.condition)?;
                    let condition_addr = self.emit(Opcode::OpJumpNotTruthy, &[9999]);
                    self.compile(&*if_expr.consequence_statement)?;
                    let jump_addr = self.instructions.len() as u16;
                    // todo. change to func
                    let mut buf = [0_u8, 2];
                    BigEndian::write_u16(&mut buf, jump_addr);
                    self.instructions[(condition_addr + 1) as usize] = buf[0];
                    self.instructions[(condition_addr + 2) as usize] = buf[1];

                    if let Some(alternative) = if_expr.alternative_statement.as_ref() {
                        self.compile(&**alternative)?;
                    }
                }
                _ => {
                    panic!("{:?} is not yet implemented", expr);
                }
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
    let program = parser.parse().unwrap();
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
                    0 => {
                        let _ = writeln!(&mut result, "{:04} {}", op_offset, def.name);
                    }
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
        compiler::disassemble,
        object::{IntObject, Object},
    };

    use super::*;

    #[test]
    fn disassemble_instructions() {
        let mut instructions = Vec::new();

        instructions.append(&mut code::make(Opcode::OpConstant, &[1]));
        instructions.append(&mut code::make(Opcode::OpConstant, &[2]));
        instructions.append(&mut code::make(Opcode::OpConstant, &[65535]));
        instructions.append(&mut code::make(Opcode::OpAdd, &[]));
        instructions.append(&mut code::make(Opcode::OpSub, &[]));
        instructions.append(&mut code::make(Opcode::OpMul, &[]));
        instructions.append(&mut code::make(Opcode::OpDiv, &[]));
        instructions.append(&mut code::make(Opcode::OpEqual, &[]));
        instructions.append(&mut code::make(Opcode::OpNotEqual, &[]));
        instructions.append(&mut code::make(Opcode::OpGreaterThan, &[]));
        instructions.append(&mut code::make(Opcode::OpNegate, &[]));
        instructions.append(&mut code::make(Opcode::OpBang, &[]));
        instructions.append(&mut code::make(Opcode::OpJumpNotTruthy, &[100]));
        instructions.append(&mut code::make(Opcode::OpJump, &[200]));

        let expected = r#"0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535
0009 OpAdd
0010 OpSub
0011 OpMul
0012 OpDiv
0013 OpEqual
0014 OpNotEqual
0015 OpGreaterThan
0016 OpNegate
0017 OpBang
0018 OpJumpNotTruthy 100
0021 OpJump 200
"#;

        assert_eq!(disassemble(&instructions).unwrap(), expected);
    }

    #[test]
    fn integer_arithmetic() {
        let cases = [
            // input, constants, expected instructions(
            (
                "1 + 2",
                vec![
                    Object::Int(IntObject::new(1)),
                    Object::Int(IntObject::new(2)),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpAdd, &[]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                "-1",
                vec![Object::Int(IntObject::new(1))],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpNegate, &[]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
        ];

        for (input, constants, expected) in cases {
            let bytecode = compile(input).unwrap();
            assert_eq!(&bytecode.constants, &constants);
            check_instructions_eq(&bytecode.instructions, &expected);
        }
    }

    #[test]
    fn boolean() {
        #[rustfmt::skip]
        let cases = [
            (
                "true;", 
                vec![], 
                vec![code::make(Opcode::OpTrue, &[]), code::make(Opcode::OpPop, &[])]
            ),
            (
                "false;", 
                vec![], 
                vec![code::make(Opcode::OpFalse, &[]), code::make(Opcode::OpPop, &[])]
            ),
            (
                "1 > 2",
                vec![
                    Object::Int(IntObject::new(1)),
                    Object::Int(IntObject::new(2)),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpGreaterThan, &[]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                "1 < 2",
                vec![
                    Object::Int(IntObject::new(2)),
                    Object::Int(IntObject::new(1)),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpGreaterThan, &[]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                "1 == 2",
                vec![
                    Object::Int(IntObject::new(1)),
                    Object::Int(IntObject::new(2)),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpEqual, &[]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                "1 != 2",
                vec![
                    Object::Int(IntObject::new(1)),
                    Object::Int(IntObject::new(2)),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpNotEqual, &[]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                "true == false",
                vec![],
                vec![
                    code::make(Opcode::OpTrue, &[]),
                    code::make(Opcode::OpFalse, &[]),
                    code::make(Opcode::OpEqual, &[]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                "true != false",
                vec![],
                vec![
                    code::make(Opcode::OpTrue, &[]),
                    code::make(Opcode::OpFalse, &[]),
                    code::make(Opcode::OpNotEqual, &[]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                "!true",
                vec![],
                vec![
                    code::make(Opcode::OpTrue, &[]),
                    code::make(Opcode::OpBang, &[]),
                    code::make(Opcode::OpPop, &[]),
                ],
            )
        ];

        for (input, constants, expected) in cases {
            let bytecode = compile(input).unwrap();
            assert_eq!(&bytecode.constants, &constants);
            check_instructions_eq(&bytecode.instructions, &expected);
        }
    }

    #[test]
    fn conditional() {
        let cases = [
            // input, constants, expected instructions(
            (
                "if (true) {10}; 3333;",
                vec![
                    Object::Int(IntObject::new(10)),
                    Object::Int(IntObject::new(3333)),
                ],
                vec![
                    code::make(Opcode::OpTrue, &[]),
                    code::make(Opcode::OpJumpNotTruthy, &[7]),
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpPop, &[]), // 0007
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
        ];

        for (input, constants, expected) in cases {
            let bytecode = compile(input).unwrap();
            assert_eq!(&bytecode.constants, &constants);
            check_instructions_eq(&bytecode.instructions, &expected);
        }
    }

    fn check_instructions_eq(ins: &Vec<u8>, other: &[Vec<u8>]) {
        // let joined = other.iter().flatten().collect::<Vec<_>>();
        let joined = other.iter().flat_map(|v| v.clone()).collect::<Vec<_>>();

        assert_eq!(
            ins,
            &joined,
            "{:?} != {:?}",
            disassemble(ins).unwrap(),
            disassemble(&joined).unwrap()
        )
    }
}

use byteorder::{BigEndian, ByteOrder};

use crate::{
    ast::{self, NodeRef},
    code::{self, Opcode},
    lexer::Lexer,
    object::{CompiledFnObject, IntObject, Object, StringObject},
    parser::Parser,
    token::Token,
};

pub struct Compiler<'a> {
    scopes: Vec<CompilationScope>,
    pub constants: Vec<Object>,
    symbol_table: &'a mut SymbolTable,
}

struct CompilationScope {
    pub instructions: code::Instructions,
    pub last_instruction: Option<code::Opcode>,
    pub prev_instruction: Option<code::Opcode>,
}

impl CompilationScope {
    pub fn new() -> Self {
        Self {
            instructions: code::Instructions::new(),
            last_instruction: None,
            prev_instruction: None,
        }
    }
}

impl<'a> Compiler<'a> {
    pub fn new(symbol_table: &'a mut SymbolTable) -> Self {
        Self {
            scopes: vec![CompilationScope::new()],
            constants: Vec::new(),
            symbol_table: symbol_table,
        }
    }

    pub fn compile<'b, N: Into<NodeRef<'b>>>(&mut self, node: N) -> Result<(), CompileError> {
        let node: NodeRef = node.into();
        match node {
            ast::NodeRef::Program(program) => {
                for stmt in &program.statements {
                    self.compile(stmt)?;
                }
            }
            ast::NodeRef::Stmt(stmt) => match stmt {
                ast::Statement::ExprStatement(expr_stmt) => {
                    self.compile(&expr_stmt.expr)?;
                    self.emit(Opcode::OpPop, &[]);
                }
                ast::Statement::BlockStatement(block_stmt) => {
                    for stmt in &block_stmt.expr.statements {
                        self.compile(stmt)?;
                    }
                }
                ast::Statement::LetStatement(let_stmt) => {
                    self.compile(&let_stmt.expr)?;
                    let symbol = self.symbol_table.define(&let_stmt.ident.to_string());
                    let symbol_index = symbol.index;
                    let opcode = match symbol.scope {
                        SymbolScope::Global => Opcode::OpSetGlobal,
                        SymbolScope::Local => Opcode::OpSetLocal,
                    };
                    self.emit(opcode, &[symbol_index]);
                }
                ast::Statement::ReturnStatement(return_stmt) => {
                    self.compile(&return_stmt.expr)?;
                    self.emit(Opcode::OpReturnValue, &[]);
                }
            },
            ast::NodeRef::Expr(expr) => match expr {
                ast::Expr::Number(num_expr) => {
                    let num_obj = Object::Int(IntObject::new(num_expr.value));
                    let const_idx = self.add_constant(num_obj);
                    self.emit(Opcode::OpConstant, &[const_idx]);
                }
                ast::Expr::Bool(bool_expr) => {
                    if bool_expr.value {
                        self.emit(Opcode::OpTrue, &[]);
                    } else {
                        self.emit(Opcode::OpFalse, &[]);
                    }
                }
                ast::Expr::String(string_expr) => {
                    let string_obj = Object::String(StringObject::new(string_expr.value.clone()));
                    let const_idx = self.add_constant(string_obj);
                    self.emit(Opcode::OpConstant, &[const_idx]);
                }
                ast::Expr::Array(array_expr) => {
                    for elem in &array_expr.array {
                        self.compile(elem)?;
                    }
                    self.emit(Opcode::OpArray, &[array_expr.array.len() as u16]);
                }
                ast::Expr::Hash(hash_expr) => {
                    for (key, value) in &hash_expr.hash {
                        self.compile(key)?;
                        self.compile(value)?;
                    }
                    self.emit(Opcode::OpHash, &[hash_expr.hash.len() as u16]);
                }
                ast::Expr::Index(array_index) => {
                    self.compile(&*array_index.collection_expr)?;
                    self.compile(&*array_index.index_expr)?;
                    self.emit(Opcode::OpIndex, &[]);
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
                        // LT ??? GT??? ?????? ????????? ??????
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
                    let condition_jump_addr = self.emit(Opcode::OpJumpNotTruthy, &[9999]);

                    self.compile(&*if_expr.consequence_statement)?;
                    if self.last_instruction_is_pop() {
                        self.remove_last_pop();
                    }
                    let jump_addr = self.emit(Opcode::OpJump, &[9999]);

                    self.set_jump_addr(condition_jump_addr);

                    if let Some(alternative) = &if_expr.alternative_statement {
                        self.compile(&**alternative)?;
                        if self.last_instruction_is_pop() {
                            self.remove_last_pop();
                        }
                    } else {
                        self.emit(Opcode::OpNull, &[]);
                    }
                    self.set_jump_addr(jump_addr);
                }
                ast::Expr::Identifier(ident_expr) => {
                    let symbol = self.symbol_table.get(&ident_expr.to_string());
                    if let Some(symbol) = symbol {
                        let symbol_index = symbol.index;
                        let op = match symbol.scope {
                            SymbolScope::Global => Opcode::OpGetGlobal,
                            SymbolScope::Local => Opcode::OpGetLocal,
                        };
                        self.emit(op, &[symbol_index]);
                    } else {
                        return Err(CompileError::GeneralError(format!(
                            "not declared identifier {}",
                            ident_expr.to_string()
                        )));
                    }
                }
                ast::Expr::Function(fn_expr) => {
                    self.enter_scope();

                    // argument
                    for arg in &fn_expr.args {
                        let _symbol = self.symbol_table.define(&arg.0);
                    }

                    self.compile(&*fn_expr.body)?;

                    // function ?????? return ?????? ????????? ???????????? ?????? pop??? ?????? return?????? ?????????
                    if self.last_instruction_is_pop() {
                        self.remove_last_pop();
                        self.emit(Opcode::OpReturnValue, &[]);
                    }

                    if !self.last_instruction_is(Opcode::OpReturnValue) {
                        self.emit(Opcode::OpReturn, &[]);
                    }

                    let mut instructions = Vec::new(); //= self.scopes.last_mut().unwrap().instructions;
                    std::mem::swap(
                        &mut instructions,
                        &mut self.scopes.last_mut().unwrap().instructions,
                    );
                    let symbol_count = self.symbol_table.symbol_count();
                    let compiled_fn = CompiledFnObject::new(instructions, symbol_count);
                    let const_idx = self.add_constant(Object::CompiledFn(compiled_fn));
                    self.leave_scope();

                    self.emit(Opcode::OpConstant, &[const_idx]);
                }
                ast::Expr::Call(call_expr) => {
                    for arg in &call_expr.args {
                        self.compile(arg)?;
                    }
                    self.compile(&*call_expr.func)?;
                    self.emit(Opcode::OpCall, &[call_expr.args.len() as u16]);
                }
            },
        }

        Ok(())
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.scopes.last().unwrap().instructions.clone(),
            constants: self.constants.clone(),
        }
    }

    fn add_constant(&mut self, constant: Object) -> u16 {
        let idx = self.constants.len();
        self.constants.push(constant);
        idx as u16
    }

    fn emit(&mut self, op: code::Opcode, operands: &[u16]) -> u16 {
        let cur_scope = self.scopes.last_mut().unwrap();
        let idx = cur_scope.instructions.len();
        let mut ins = code::make(op, operands);
        cur_scope.instructions.append(&mut ins);

        cur_scope.prev_instruction = cur_scope.last_instruction.take();
        cur_scope.last_instruction = Some(op);

        idx as u16
    }

    fn last_instruction_is_pop(&self) -> bool {
        self.last_instruction_is(Opcode::OpPop)
    }

    fn last_instruction_is(&self, op: Opcode) -> bool {
        let cur_scope = self.scopes.last().unwrap();

        if let Some(last_instruction) = &cur_scope.last_instruction {
            last_instruction == &op
        } else {
            false
        }
    }

    fn remove_last_pop(&mut self) {
        let cur_scope = self.scopes.last_mut().unwrap();

        cur_scope.instructions.pop();
        cur_scope.last_instruction = cur_scope.prev_instruction.take();
    }

    fn change_argument(&mut self, instruction_addr: u16, argument_offset: u16, argument: &[u8]) {
        let cur_scope = self.scopes.last_mut().unwrap();

        for i in 0..argument.len() {
            cur_scope.instructions
                [i + (instruction_addr as usize) + 1 + (argument_offset as usize)] = argument[i];
        }
    }

    fn set_jump_addr(&mut self, instruction_addr: u16) {
        let cur_scope = self.scopes.last_mut().unwrap();

        let jump_addr = cur_scope.instructions.len() as u16;
        let mut buf = [0_u8, 2];
        BigEndian::write_u16(&mut buf, jump_addr);
        self.change_argument(instruction_addr, 0, &buf);
    }

    fn enter_scope(&mut self) {
        let new_scope = CompilationScope::new();
        self.scopes.push(new_scope);

        self.symbol_table.enter_scope();
    }
    fn leave_scope(&mut self) {
        self.symbol_table.leave_scope();
        self.scopes.pop().unwrap();
    }
}

pub fn compile(s: &str) -> Result<Bytecode, CompileError> {
    let mut symbol_table = SymbolTable::new();
    let mut compiler = Compiler::new(&mut symbol_table);

    let lexer = Lexer::new(s);
    let parser = Parser::new(lexer);
    let program = parser.parse().unwrap();
    let stmts = program.take_statement();
    for stmt in &stmts {
        let node: NodeRef = stmt.into();
        compiler.compile(node)?;
    }

    let bytecode = compiler.bytecode();
    Ok(bytecode)
}

pub struct Bytecode {
    pub instructions: code::Instructions,
    pub constants: Vec<Object>,
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

use super::{SymbolScope, SymbolTable};

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
            1 => {
                operands[idx] = ins[offset as usize] as i32;
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
        object::{IntObject, Object, StringObject},
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
        instructions.append(&mut code::make(Opcode::OpGetGlobal, &[1]));
        instructions.append(&mut code::make(Opcode::OpSetGlobal, &[2]));
        instructions.append(&mut code::make(Opcode::OpArray, &[10]));
        instructions.append(&mut code::make(Opcode::OpHash, &[10]));
        instructions.append(&mut code::make(Opcode::OpIndex, &[]));
        instructions.append(&mut code::make(Opcode::OpCall, &[1]));
        instructions.append(&mut code::make(Opcode::OpReturnValue, &[]));
        instructions.append(&mut code::make(Opcode::OpReturn, &[]));
        instructions.append(&mut code::make(Opcode::OpGetLocal, &[0]));
        instructions.append(&mut code::make(Opcode::OpSetLocal, &[255]));

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
0024 OpGetGlobal 1
0027 OpSetGlobal 2
0030 OpArray 10
0033 OpHash 10
0036 OpIndex
0037 OpCall 1
0039 OpReturnValue
0040 OpReturn
0041 OpGetLocal 0
0043 OpSetLocal 255
"#;

        assert_eq!(disassemble(&instructions).unwrap(), expected);
    }

    #[test]
    fn compilation_scope() {
        let mut symbol_table = SymbolTable::new();
        let mut compiler = Compiler::new(&mut symbol_table);

        assert_eq!(compiler.scopes.len(), 1);

        compiler.emit(Opcode::OpMul, &[]);

        compiler.enter_scope();
        compiler.emit(Opcode::OpSub, &[]);

        assert_eq!(compiler.scopes.last().unwrap().instructions.len(), 1);

        let last_instruction = &compiler.scopes.last().unwrap().last_instruction;
        assert_eq!(last_instruction, &Some(Opcode::OpSub));

        compiler.leave_scope();
        compiler.emit(Opcode::OpAdd, &[]);

        assert_eq!(compiler.scopes.last().unwrap().instructions.len(), 2);
        let last_instruction = &compiler.scopes.last().unwrap().last_instruction;
        assert_eq!(last_instruction, &Some(Opcode::OpAdd));
        let prev_instruction = &compiler.scopes.last().unwrap().prev_instruction;
        assert_eq!(prev_instruction, &Some(Opcode::OpMul));
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
                    code::make(Opcode::OpJumpNotTruthy, &[10]),
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpJump, &[11]),
                    code::make(Opcode::OpNull, &[]),
                    code::make(Opcode::OpPop, &[]), // 0011
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                "if (true) {10} else {20};3333;",
                vec![
                    Object::Int(IntObject::new(10)),
                    Object::Int(IntObject::new(20)),
                    Object::Int(IntObject::new(3333)),
                ],
                vec![
                    code::make(Opcode::OpTrue, &[]),
                    code::make(Opcode::OpJumpNotTruthy, &[10]),
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpJump, &[13]),
                    code::make(Opcode::OpConstant, &[1]), // 10
                    code::make(Opcode::OpPop, &[]),       // 13
                    code::make(Opcode::OpConstant, &[2]),
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
    fn let_statement() {
        let cases = [
            (
                "let one = 1; let two = 2;",
                vec![
                    Object::Int(IntObject::new(1)),
                    Object::Int(IntObject::new(2)),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpSetGlobal, &[0]),
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpSetGlobal, &[1]),
                ],
            ),
            (
                r#" let one = 1;
                    one;"#,
                vec![Object::Int(IntObject::new(1))],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpSetGlobal, &[0]),
                    code::make(Opcode::OpGetGlobal, &[0]),
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
    fn compile_string() {
        let cases = [
            (
                "\"my string\"",
                vec![Object::String(StringObject::new("my string".to_string()))],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                "\"abc\" + \"def\"",
                vec![
                    Object::String(StringObject::new("abc".to_string())),
                    Object::String(StringObject::new("def".to_string())),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpAdd, &[]),
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
    fn compile_array() {
        let cases = [
            (
                "[]",
                vec![],
                vec![
                    code::make(Opcode::OpArray, &[0]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                "[1, 2, 3]",
                vec![
                    Object::Int(IntObject::new(1)),
                    Object::Int(IntObject::new(2)),
                    Object::Int(IntObject::new(3)),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpConstant, &[2]),
                    code::make(Opcode::OpArray, &[3]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                "[1 + 2, 3 - 4, 5 * 6]",
                vec![
                    Object::Int(IntObject::new(1)),
                    Object::Int(IntObject::new(2)),
                    Object::Int(IntObject::new(3)),
                    Object::Int(IntObject::new(4)),
                    Object::Int(IntObject::new(5)),
                    Object::Int(IntObject::new(6)),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpAdd, &[]),
                    code::make(Opcode::OpConstant, &[2]),
                    code::make(Opcode::OpConstant, &[3]),
                    code::make(Opcode::OpSub, &[]),
                    code::make(Opcode::OpConstant, &[4]),
                    code::make(Opcode::OpConstant, &[5]),
                    code::make(Opcode::OpMul, &[]),
                    code::make(Opcode::OpArray, &[3]),
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
    fn compile_hash() {
        let cases = [
            (
                "{}",
                vec![],
                vec![
                    code::make(Opcode::OpHash, &[0]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                "{1:10, 2:20}",
                vec![
                    Object::Int(IntObject::new(1)),
                    Object::Int(IntObject::new(10)),
                    Object::Int(IntObject::new(2)),
                    Object::Int(IntObject::new(20)),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpConstant, &[2]),
                    code::make(Opcode::OpConstant, &[3]),
                    code::make(Opcode::OpHash, &[2]),
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
    fn compile_array_index() {
        let cases = [
            (
                "[1, 2, 3][1 + 1]",
                vec![
                    Object::Int(1.into()),
                    Object::Int(2.into()),
                    Object::Int(3.into()),
                    Object::Int(1.into()),
                    Object::Int(1.into()),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpConstant, &[2]),
                    code::make(Opcode::OpArray, &[3]),
                    code::make(Opcode::OpConstant, &[3]),
                    code::make(Opcode::OpConstant, &[4]),
                    code::make(Opcode::OpAdd, &[]),
                    code::make(Opcode::OpIndex, &[]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                r#"{1:2}[2-1]"#,
                vec![
                    Object::Int(1.into()),
                    Object::Int(2.into()),
                    Object::Int(2.into()),
                    Object::Int(1.into()),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpHash, &[1]),
                    code::make(Opcode::OpConstant, &[2]),
                    code::make(Opcode::OpConstant, &[3]),
                    code::make(Opcode::OpSub, &[]),
                    code::make(Opcode::OpIndex, &[]),
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
    fn compile_fn() {
        let cases = [
            (
                r#"fn() { return 5 + 10; }"#,
                vec![
                    Object::Int(5.into()),
                    Object::Int(10.into()),
                    Object::CompiledFn(
                        (
                            vec![
                                code::make(Opcode::OpConstant, &[0]),
                                code::make(Opcode::OpConstant, &[1]),
                                code::make(Opcode::OpAdd, &[]),
                                code::make(Opcode::OpReturnValue, &[]),
                            ],
                            0,
                        )
                            .into(),
                    ),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[2]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                r#"fn() { 5 + 10 }"#,
                vec![
                    Object::Int(5.into()),
                    Object::Int(10.into()),
                    Object::CompiledFn(
                        (
                            vec![
                                code::make(Opcode::OpConstant, &[0]),
                                code::make(Opcode::OpConstant, &[1]),
                                code::make(Opcode::OpAdd, &[]),
                                code::make(Opcode::OpReturnValue, &[]),
                            ],
                            0,
                        )
                            .into(),
                    ),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[2]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                r#"fn() { }"#,
                vec![Object::CompiledFn(
                    (vec![code::make(Opcode::OpReturn, &[])], 0).into(),
                )],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
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
    fn compile_fn_call() {
        let cases = [
            (
                r#"fn() { 24 }();"#,
                vec![
                    Object::Int(24.into()),
                    Object::CompiledFn(
                        (
                            vec![
                                code::make(Opcode::OpConstant, &[0]),
                                code::make(Opcode::OpReturnValue, &[]),
                            ],
                            0,
                        )
                            .into(),
                    ),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpCall, &[]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                r#"let no_arg = fn() { 24 }; no_arg()"#,
                vec![
                    Object::Int(24.into()),
                    Object::CompiledFn(
                        (
                            vec![
                                code::make(Opcode::OpConstant, &[0]),
                                code::make(Opcode::OpReturnValue, &[]),
                            ],
                            0,
                        )
                            .into(),
                    ),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpSetGlobal, &[0]),
                    code::make(Opcode::OpGetGlobal, &[0]),
                    code::make(Opcode::OpCall, &[]),
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
    fn let_statement_scope() {
        let cases = [
            (
                r#"let num = 55; fn() {num}"#,
                vec![
                    Object::Int(55.into()),
                    Object::CompiledFn(
                        (
                            vec![
                                code::make(Opcode::OpGetGlobal, &[0]),
                                code::make(Opcode::OpReturnValue, &[]),
                            ],
                            0,
                        )
                            .into(),
                    ),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpSetGlobal, &[0]),
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                r#"fn() { let num = 55; num}"#,
                vec![
                    Object::Int(55.into()),
                    Object::CompiledFn(
                        (
                            vec![
                                code::make(Opcode::OpConstant, &[0]),
                                code::make(Opcode::OpSetLocal, &[0]),
                                code::make(Opcode::OpGetLocal, &[0]),
                                code::make(Opcode::OpReturnValue, &[]),
                            ],
                            1,
                        )
                            .into(),
                    ),
                ],
                vec![
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

    #[test]
    fn fn_argument() {
        let cases = [
            (
                r#"fn(a) { a };"#,
                vec![Object::CompiledFn(
                    (
                        vec![
                            code::make(Opcode::OpGetLocal, &[0]),
                            code::make(Opcode::OpReturnValue, &[]),
                        ],
                        1,
                    )
                        .into(),
                )],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                r#"fn(a) { let b = 20; a + b };"#,
                vec![
                    Object::Int(20.into()),
                    Object::CompiledFn(
                        (
                            vec![
                                code::make(Opcode::OpConstant, &[0]),
                                code::make(Opcode::OpSetLocal, &[1]),
                                code::make(Opcode::OpGetLocal, &[0]),
                                code::make(Opcode::OpGetLocal, &[1]),
                                code::make(Opcode::OpAdd, &[]),
                                code::make(Opcode::OpReturnValue, &[]),
                            ],
                            2,
                        )
                            .into(),
                    ),
                ],
                vec![
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

    #[test]
    fn fn_argument_call() {
        let cases = [
            (
                r#"fn(a) { a }(10);"#,
                vec![
                    Object::Int(10.into()),
                    Object::CompiledFn(
                        (
                            vec![
                                code::make(Opcode::OpGetLocal, &[0]),
                                code::make(Opcode::OpReturnValue, &[]),
                            ],
                            1,
                        )
                            .into(),
                    ),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpConstant, &[1]),
                    code::make(Opcode::OpCall, &[1]),
                    code::make(Opcode::OpPop, &[]),
                ],
            ),
            (
                r#"fn(a) { let b = 20; a + b }(10);"#,
                vec![
                    Object::Int(10.into()),
                    Object::Int(20.into()),
                    Object::CompiledFn(
                        (
                            vec![
                                code::make(Opcode::OpConstant, &[1]),
                                code::make(Opcode::OpSetLocal, &[1]),
                                code::make(Opcode::OpGetLocal, &[0]),
                                code::make(Opcode::OpGetLocal, &[1]),
                                code::make(Opcode::OpAdd, &[]),
                                code::make(Opcode::OpReturnValue, &[]),
                            ],
                            2,
                        )
                            .into(),
                    ),
                ],
                vec![
                    code::make(Opcode::OpConstant, &[0]),
                    code::make(Opcode::OpConstant, &[2]),
                    code::make(Opcode::OpCall, &[1]),
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

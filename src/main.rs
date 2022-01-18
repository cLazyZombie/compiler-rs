use std::io::{self, BufRead, Write};

use compiler_rs::{
    compiler::{Compiler, SymbolTable},
    lexer::Lexer,
    parser::Parser,
    vm::{GlobalVars, Vm},
};

fn main() {
    print!(">> ");
    let _ = io::stdout().lock().flush();
    let mut symbol_table = SymbolTable::new();
    let mut global_vars = GlobalVars::new(65536);

    for line in io::stdin().lock().lines() {
        let line = line.unwrap();

        let lexer = Lexer::new(&line);
        let parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        let mut compiler = Compiler::new(&mut symbol_table);
        compiler.compile(&program).unwrap();
        let bytecode = compiler.bytecode();
        let mut vm = Vm::new(bytecode, &mut global_vars);
        vm.run().unwrap();
        println!("{}", vm.last_popped_stack_element.to_string());

        print!(">> ");
        let _ = io::stdout().lock().flush();
    }
}

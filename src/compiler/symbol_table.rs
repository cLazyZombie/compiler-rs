use std::collections::HashMap;

pub struct SymbolTable {
    stacks: Vec<ScopedSymbols>,
}

struct ScopedSymbols {
    pub symbols: HashMap<String, Symbol>,
    pub num_definitions: usize,
}

impl ScopedSymbols {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: &str, scope: SymbolScope) -> &Symbol {
        let symbol = Symbol {
            name: name.to_string(),
            scope,
            index: self.num_definitions as u16,
        };
        self.num_definitions += 1;
        self.symbols.entry(name.to_string()).or_insert(symbol)
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            stacks: vec![ScopedSymbols::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        let child_symbol = ScopedSymbols::new();
        self.stacks.push(child_symbol);
    }

    pub fn leave_scope(&mut self) {
        self.stacks.pop().unwrap();
        debug_assert!(self.stacks.len() > 0);
    }

    pub fn define(&mut self, name: &str) -> &Symbol {
        let scope = if self.stacks.len() == 1 {
            SymbolScope::Global
        } else {
            SymbolScope::Local
        };
        self.stacks.last_mut().unwrap().define(name, scope)
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        for scope in self.stacks.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }

        None
    }

    pub fn symbol_count(&self) -> u16 {
        self.stacks.last().unwrap().num_definitions as u16
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum SymbolScope {
    Global,
    Local,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: u16,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define() {
        let expected = HashMap::from([
            (
                "a".to_string(),
                Symbol {
                    name: "a".to_string(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
            ),
            (
                "b".to_string(),
                Symbol {
                    name: "b".to_string(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
            ),
        ]);

        let mut global = SymbolTable::new();
        let symbol_a = global.define("a");
        assert_eq!(Some(symbol_a), expected.get("a"));
    }

    #[test]
    fn test_local() {
        let mut symbol_table = SymbolTable::new();

        let sym_a = symbol_table.define("a");
        assert_eq!(
            sym_a,
            &Symbol {
                name: "a".to_string(),
                scope: SymbolScope::Global,
                index: 0
            }
        );

        let sym_b = symbol_table.define("b");
        assert_eq!(
            sym_b,
            &Symbol {
                name: "b".to_string(),
                scope: SymbolScope::Global,
                index: 1
            }
        );

        assert_eq!(symbol_table.symbol_count(), 2);

        symbol_table.enter_scope();

        let sym_c = symbol_table.define("c");
        assert_eq!(
            sym_c,
            &Symbol {
                name: "c".to_string(),
                scope: SymbolScope::Local,
                index: 0
            }
        );

        assert_eq!(symbol_table.symbol_count(), 1);

        symbol_table.leave_scope();

        // scope를 벗어나면 이전 symbol table로 돌아감
        assert_eq!(symbol_table.symbol_count(), 2);
        assert_eq!(symbol_table.get("c"), None);
    }
}

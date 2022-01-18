use std::collections::HashMap;

pub struct SymbolTable {
    pub symbols: HashMap<String, Symbol>,
    pub num_definitions: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: &str) -> &Symbol {
        let symbol = Symbol {
            name: name.to_string(),
            scope: SymbolScope::Global,
            index: self.num_definitions as u16,
        };
        self.num_definitions += 1;
        self.symbols.entry(name.to_string()).or_insert(symbol)
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum SymbolScope {
    Global,
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
}

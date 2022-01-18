use crate::object::Object;

pub struct GlobalVars {
    globals: Vec<Object>,
}

impl GlobalVars {
    pub fn new(count: usize) -> Self {
        Self {
            globals: vec![Object::Null; count],
        }
    }

    pub fn get(&self, idx: u16) -> Option<&Object> {
        self.globals.get(idx as usize)
    }

    pub fn get_mut(&mut self, idx: u16) -> Option<&mut Object> {
        self.globals.get_mut(idx as usize)
    }
}

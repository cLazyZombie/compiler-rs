use crate::object::Object;

pub struct Constants {
    constants: Vec<Object>,
}

impl Constants {
    pub fn new(constants: Vec<Object>) -> Self {
        Self { constants }
    }

    pub fn get(&self, idx: u16) -> &Object {
        self.constants.get(idx as usize).unwrap()
    }
}

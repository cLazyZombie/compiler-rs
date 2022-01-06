use std::collections::HashMap;

use crate::{object::Object, token::IdentToken};

#[derive(Debug)]
pub struct Environment<'a> {
    store: HashMap<IdentToken, Object>,
    parent: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            parent: None,
        }
    }

    pub fn set(&mut self, ident: IdentToken, obj: Object) {
        self.store.insert(ident, obj);
    }

    pub fn set_parent(&mut self, parent: &'a Environment) {
        self.parent = Some(parent);
    }

    pub fn get(&self, ident: &IdentToken) -> Option<Object> {
        if let Some(obj) = self.store.get(ident).map(|obj| obj.clone()) {
            Some(obj)
        } else {
            if let Some(parent) = self.parent {
                parent.get(ident)
            } else {
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::object::IntObject;

    use super::*;

    #[test]
    fn test_enclosed_environment() {
        let mut env_1 = Environment::new();
        env_1.set(IdentToken("a".to_string()), IntObject::new(1).into());

        let obj_a = env_1.get(&IdentToken("a".to_string())).unwrap();
        check_int_object(&obj_a, 1);

        // add sub env
        let mut env_2 = Environment::new();
        env_2.set(IdentToken("b".to_string()), IntObject::new(2).into());
        env_2.set_parent(&env_1);

        let obj_a = env_2.get(&IdentToken("a".to_string())).unwrap();
        check_int_object(&obj_a, 1);

        let obj_b = env_2.get(&IdentToken("b".to_string())).unwrap();
        check_int_object(&obj_b, 2);

        // add sub-sub env
        let mut env_3 = Environment::new();
        env_3.set(IdentToken("c".to_string()), IntObject::new(3).into());
        env_3.set_parent(&env_2);

        let obj_a = env_3.get(&IdentToken("a".to_string())).unwrap();
        check_int_object(&obj_a, 1);

        let obj_b = env_3.get(&IdentToken("b".to_string())).unwrap();
        check_int_object(&obj_b, 2);

        let obj_c = env_3.get(&IdentToken("c".to_string())).unwrap();
        check_int_object(&obj_c, 3);
    }

    fn check_int_object(obj: &Object, val: i32) {
        match obj {
            Object::Int(int_obj) => {
                assert_eq!(int_obj.val, val);
            }
            _ => panic!("expected int object, but {:?}", obj),
        }
    }
}

use std::collections::HashMap;

use crate::object::Object;

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, key: &str) -> Option<&Object> {
        self.store.get(key)
    }

    pub fn set(&mut self, key: String, value: Object) {
        self.store.insert(key, value);
    }
}

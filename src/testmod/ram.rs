use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::Memory;

/// A basic infinitely-large hunk of RAM. Should only be used for tests!
#[derive(Serialize, Deserialize)]
pub struct Ram(HashMap<u32, u8>);

impl Ram {
    pub fn new_with_data(data: &[u8]) -> Ram {
        Ram(data
            .iter()
            .cloned()
            .enumerate()
            .map(|(i, b)| (i as u32, b))
            .collect())
    }
}

impl Memory for Ram {
    fn r8(&mut self, addr: u32) -> u8 {
        // side-effect free
        self.p8(addr)
    }

    fn w8(&mut self, addr: u32, val: u8) {
        self.0.insert(addr, val);
    }

    fn p8(&self, addr: u32) -> u8 {
        *self.0.get(&addr).unwrap_or(&0)
    }
}

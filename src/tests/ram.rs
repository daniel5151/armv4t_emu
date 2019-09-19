use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::Memory;

/// A basic infinitely-large hunk of RAM.
/// Should only be used for tests!
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
        *self.0.get(&addr).unwrap_or(&0)
    }
    fn r16(&mut self, addr: u32) -> u16 {
        self.r8(addr) as u16 | (self.r8(addr + 1) as u16) << 8
    }
    fn r32(&mut self, addr: u32) -> u32 {
        self.r16(addr) as u32 | (self.r16(addr + 2) as u32) << 16
    }

    fn w8(&mut self, addr: u32, val: u8) {
        self.0.insert(addr, val);
    }
    fn w16(&mut self, addr: u32, val: u16) {
        self.w8(addr, val as u8);
        self.w8(addr + 1, (val >> 8) as u8);
    }
    fn w32(&mut self, addr: u32, val: u32) {
        self.w16(addr, val as u16);
        self.w16(addr + 2, (val >> 16) as u16);
    }
}

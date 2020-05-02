use std::collections::BTreeMap;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::Memory;

/// Example memory device backed by a BTreeMap<u32, u8>.
///
/// Uninitialized memory returns 0x00.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Default)]
pub struct ExampleMem(BTreeMap<u32, u8>);

impl ExampleMem {
    /// Constructs a new, empty ExampleMem.
    pub fn new() -> ExampleMem {
        ExampleMem(BTreeMap::new())
    }

    /// Constructs a new ExampleMem from the provided slice. Data is copied
    /// contiguously from the slice into address [0..data.len()]
    pub fn new_with_data(data: &[u8]) -> ExampleMem {
        ExampleMem(
            data.iter()
                .cloned()
                .enumerate()
                .map(|(i, b)| (i as u32, b))
                .collect(),
        )
    }
}

impl Memory for ExampleMem {
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

use std::vec::Vec;

use byteorder::{ByteOrder, LittleEndian};
use serde_derive::{Deserialize, Serialize};

use crate::Memory;

/// Implements a basic memory model with no memory mapping
#[derive(Serialize, Deserialize)]
pub struct Ram {
    mem: Vec<u8>,
}

impl Ram {
    pub fn new(size: usize) -> Ram {
        Ram {
            mem: vec![0u8; size],
        }
    }

    pub fn new_with_data(size: usize, data: &[u8]) -> Ram {
        let mut ram = Ram::new(size);
        ram.mem[..data.len()].clone_from_slice(data);
        ram
    }
}

impl Memory for Ram {
    fn load8(&self, addr: u32) -> u8 {
        let idx = addr as usize;
        if idx < self.mem.len() {
            self.mem[idx]
        } else {
            panic!("load8 from invalid address {:#010x}", addr);
        }
    }

    fn set8(&mut self, addr: u32, val: u8) {
        let idx = addr as usize;
        if idx < self.mem.len() {
            self.mem[idx] = val;
        }
    }

    fn load16(&self, addr: u32) -> u16 {
        debug_assert!(addr % 2 == 0);

        let idx = addr as usize;
        if idx < self.mem.len() - 1 {
            LittleEndian::read_u16(&self.mem[idx..idx + 2])
        } else {
            panic!("load16 from invalid address {:#010x}", addr);
        }
    }

    fn set16(&mut self, addr: u32, val: u16) {
        debug_assert!(addr % 2 == 0);

        let idx = addr as usize;
        if idx < self.mem.len() - 1 {
            LittleEndian::write_u16(&mut self.mem[idx..idx + 2], val);
        }
    }

    fn load32(&self, addr: u32) -> u32 {
        debug_assert!(addr % 4 == 0);

        let idx = addr as usize;
        if idx < self.mem.len() - 3 {
            LittleEndian::read_u32(&self.mem[idx..idx + 4])
        } else {
            panic!("load32 from invalid address {:#010x}", addr);
        }
    }

    fn set32(&mut self, addr: u32, val: u32) {
        debug_assert!(addr % 4 == 0);

        let idx = addr as usize;
        if idx < self.mem.len() - 3 {
            LittleEndian::write_u32(&mut self.mem[idx..idx + 4], val);
        }
    }
}

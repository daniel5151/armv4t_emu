use std::default::Default;
use std::ops::{Index, IndexMut};

use bit_util::extract;

use super::mode::Mode;

pub type Reg = u8;

const NUM_RGSR: Reg = 37;

pub const SP: Reg = 13;
pub const LR: Reg = 14;
pub const PC: Reg = 15;
pub const CPSR: Reg = 16;
pub const SPSR: Reg = 17;

const REG_MAP: [[usize; 18]; 6] = [
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 16],
    [0, 1, 2, 3, 4, 5, 6, 7, 17, 18, 19, 20, 21, 22, 23, 15, 16, 24],
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 25, 26, 15, 16, 27],
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 28, 29, 15, 16, 30],
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 31, 32, 15, 16, 33],
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 34, 35, 15, 16, 36],
];

pub struct RegFile {
    reg: [u32; NUM_RGSR as usize],
    bank: usize,
}

impl RegFile {
    #[inline]
    pub fn mode(&self) -> Mode {
        Mode::from_bits(extract(self.reg[CPSR as usize], 0, 5) as u8)
    }

    #[inline]
    pub fn update_bank(&mut self) {
        self.bank = self.mode().reg_bank();
    }

    #[inline]
    pub fn set(&mut self, bank: usize, reg: Reg, val: u32) {
        self.reg[REG_MAP[bank][reg as usize]] = val;
        if reg == CPSR {
            self.update_bank()
        }
    }

    #[inline]
    pub fn get(&self, bank: usize, reg: Reg) -> u32 {
        self.reg[REG_MAP[bank][reg as usize]]
    }
}

impl Default for RegFile {
    fn default() -> RegFile {
        RegFile {
            reg: [0; NUM_RGSR as usize],
            bank: 0,
        }
    }
}

impl Index<Reg> for RegFile {
    type Output = u32;
    #[inline]
    fn index(&self, idx: Reg) -> &u32 {
        &self.reg[REG_MAP[self.bank][idx as usize]]
    }
}

impl IndexMut<Reg> for RegFile {
    #[inline]
    fn index_mut(&mut self, idx: Reg) -> &mut u32 {
        &mut self.reg[REG_MAP[self.bank][idx as usize]]
    }
}

pub mod cpsr {
    use super::Reg;

    pub const N: Reg = 31;
    pub const Z: Reg = 30;
    pub const C: Reg = 29;
    pub const V: Reg = 28;

    pub const T: Reg = 5;
}

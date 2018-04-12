use std::default::Default;
use std::ops::{Index, IndexMut};

pub type Reg = u8;

const NUM_RGSR: Reg = 37;

pub const SP: Reg = 13;
pub const LR: Reg = 14;
pub const PC: Reg = 15;
pub const CPSR: Reg = 36;

pub struct RegFile {
    reg: [u32; NUM_RGSR as usize],
}

impl Default for RegFile {
    fn default() -> RegFile {
        RegFile { reg: [0; NUM_RGSR as usize] }
    }
}

impl Index<Reg> for RegFile {
    type Output = u32;
    #[inline]
    fn index(&self, idx: Reg) -> &u32 {
        &self.reg[idx as usize]
    }
}

impl IndexMut<Reg> for RegFile {
    #[inline]
    fn index_mut(&mut self, idx: Reg) -> &mut u32 {
        &mut self.reg[idx as usize]
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

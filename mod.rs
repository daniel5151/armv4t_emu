use std;
use std::default::Default;
use std::ops::{Index, IndexMut};

use super::mmu::Mmu;

mod arm;

use self::arm::ArmIsaCpu;

type Reg = u8;

const NUM_RGSR: Reg = 37;
const PC_REG: Reg = 15;

enum IsaMode {
    Arm,
    Thumb,
}

struct RegFile {
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

pub struct Cpu {
    reg: RegFile,
    mmu: Mmu,
    mode: IsaMode,
}

impl Cpu {
    pub fn new(mmu: Mmu) -> Cpu {
        Cpu {
            reg: Default::default(),
            mmu: mmu,
            mode: IsaMode::Arm,
        }
    }
}

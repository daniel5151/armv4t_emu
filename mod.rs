use std::boxed::Box;
use std::default::Default;
use std::iter::IntoIterator;

use super::mmu::Mmu;

mod arm;
mod thumb;
mod util;
pub mod reg;

use self::reg::*;

pub struct Cpu {
    reg: RegFile,
    mmu: Box<Mmu>,
}

impl Cpu {
    pub fn new<'a, I>(mmu: Box<Mmu>, regs: I) -> Cpu
        where I: IntoIterator<Item = &'a (Reg, u32)>
    {
        let mut cpu = Cpu {
            reg: Default::default(),
            mmu: mmu,
        };
        cpu.init(regs);

        cpu
    }

    fn init<'a, I>(&mut self, regs: I)
        where I: IntoIterator<Item = &'a (Reg, u32)>
    {
        for &(reg, val) in regs.into_iter() {
            self.reg[reg] = val;
        }

        // init cpsr
        self.reg[reg::CPSR] = 0x10;
    }

    pub fn run(&mut self) {
        use self::arm::ArmIsaCpu;
        while self.execute() {}
    }

    pub fn memory(&self) -> &Mmu {
        &*self.mmu
    }
}

use std::boxed::Box;
use std::default::Default;

use super::mmu::Mmu;

mod arm;
mod bit_util;
mod reg;

pub struct Cpu {
    reg: reg::RegFile,
    mmu: Box<Mmu>,
}

impl Cpu {
    pub fn new(mmu: Box<Mmu>, start_pc: u32) -> Cpu {
        let mut cpu = Cpu {
            reg: Default::default(),
            mmu: mmu,
        };
        cpu.init(start_pc);

        cpu
    }

    fn init(&mut self, start_pc: u32) {
        self.reg[reg::PC] = start_pc;
    }

    pub fn run(&mut self) {
        use self::arm::ArmIsaCpu;
        while self.execute() {}
    }

    pub fn memory(&self) -> &Mmu {
        &*self.mmu
    }
}

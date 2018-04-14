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
    where
        I: IntoIterator<Item = &'a (Reg, u32)>,
    {
        let mut cpu = Cpu {
            reg: Default::default(),
            mmu: mmu,
        };
        cpu.init(regs);

        cpu
    }

    fn init<'a, I>(&mut self, regs: I)
    where
        I: IntoIterator<Item = &'a (Reg, u32)>,
    {
        // init cpsr mode
        self.reg.set(0, reg::CPSR, 0x10);
        for &(reg, val) in regs.into_iter() {
            self.reg[reg] = val;
        }
    }

    pub fn run(&mut self) {
        use self::arm::ArmIsaCpu;
        use self::thumb::ThumbIsaCpu;

        let mut run = true;
        while run {
            run = if !self.thumb_mode() {
                ArmIsaCpu::execute(self)
            } else {
                ThumbIsaCpu::execute(self)
            }
        }
    }

    pub fn memory(&self) -> &Mmu {
        &*self.mmu
    }

    pub fn set_thumb_mode(&mut self, thumb: bool) {
        let mask = 1u32 << cpsr::T;
        let cpsr = self.reg[reg::CPSR];
        self.reg[reg::CPSR] = (cpsr & !mask) | ((thumb as u32) * mask);
    }

    fn thumb_mode(&self) -> bool {
        (self.reg[reg::CPSR] & (1u32 << cpsr::T)) != 0
    }
}

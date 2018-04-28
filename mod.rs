use std::collections::HashSet;
use std::default::Default;
use std::iter::IntoIterator;

use shared::Shared;

use mmu::Mmu;

pub mod mode;
pub mod exception;
pub mod reg;
mod arm;
mod thumb;
mod util;

use self::reg::*;
use self::exception::Exception;

pub struct Cpu<T: Mmu> {
    reg: RegFile,
    mmu: Shared<T>,
    brk: HashSet<u32>,
}

impl<T: Mmu> Cpu<T> {
    pub fn new<'a, I>(mmu: Shared<T>, regs: I) -> Cpu<T>
    where
        I: IntoIterator<Item = &'a (usize, Reg, u32)>,
    {
        let mut cpu = Cpu {
            reg: Default::default(),
            mmu: mmu,
            brk: Default::default(),
        };
        cpu.init(regs);

        cpu
    }

    fn init<'a, I>(&mut self, regs: I)
    where
        I: IntoIterator<Item = &'a (usize, Reg, u32)>,
    {
        // start in system mode
        self.reg.set(0, reg::CPSR, 0x1F);
        for &(bank, reg, val) in regs.into_iter() {
            self.reg.set(bank, reg, val);
        }
    }

    pub fn set_breaks<'a, I>(&mut self, brks: I)
    where
        I: IntoIterator<Item = &'a u32>,
    {
        for addr in brks.into_iter() {
            self.brk.insert(*addr);
        }
    }

    pub fn run(&mut self) {
        let mut run = true;
        while run {
            run = self.cycle();
        }
    }

    pub fn cycle(&mut self) -> bool {
        if self.brk.contains(&self.reg[reg::PC]) {
            warn!("Breakpoint {:#010x} hit!", self.reg[reg::PC]);
        }
        if !self.thumb_mode() {
            self.execute_arm()
        } else {
            self.execute_thumb()
        }
    }

    pub fn exception(&mut self, exc: &Exception) {
        // this should already be pointing at the next instruction
        let new_mode = exc.mode_on_entry();
        let new_bank = new_mode.reg_bank();

        let cpsr = self.reg.get(0, reg::CPSR);
        // instruction that just executed + (2/4 depending on
        // thumb vs arm)
        let pc = self.reg.get(0, reg::PC);

        let new_lr = match *exc {
            Exception::Interrupt => pc + 4,
            _ => pc,
        };

        self.reg.set(new_bank, reg::LR, new_lr);
        self.reg.set(new_bank, reg::SPSR, cpsr);

        self.reg.set(0, reg::PC, exc.address());
        let new_cpsr =
            (new_mode.bits() as u32) |
            (0 << 5) /* ARM mode */ |
            ((exc.fiq_disable() as u32) << 6) |
            (1 << 7) /* IRQ disable */ |
            (cpsr & (0xf << 28)) /* condition flags */;
        self.reg.set(0, reg::CPSR, new_cpsr);
    }

    pub fn set_thumb_mode(&mut self, thumb: bool) {
        let mask = 1u32 << cpsr::T;
        let cpsr = self.reg[reg::CPSR];
        self.reg[reg::CPSR] = (cpsr & !mask) | ((thumb as u32) * mask);
    }

    pub fn irq_enable(&self) -> bool {
        self.reg.get(0, reg::CPSR) & (1 << 7) == 0
    }

    fn thumb_mode(&self) -> bool {
        (self.reg[reg::CPSR] & (1u32 << cpsr::T)) != 0
    }
}

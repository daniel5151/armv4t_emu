use std::collections::HashSet;
use std::default::Default;
use std::iter::IntoIterator;

use shared::Shared;

use mmu::MemoryUnit;

pub mod mode;
pub mod exception;
pub mod reg;
mod arm;
mod thumb;
mod util;
mod mem;

use self::reg::*;
use self::exception::Exception;

#[derive(Serialize, Deserialize)]
pub struct Cpu<T: MemoryUnit> {
    reg: RegFile,
    #[serde(skip, default="Shared::empty")]
    mmu: Shared<T>,
    #[serde(skip)]
    brk: HashSet<u32>,
}

impl<T: MemoryUnit> Cpu<T> {
    pub fn new<'a, I>(mmu: Shared<T>, regs: I) -> Self
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

    /// Initializes registers according to the ARM documentation
    pub fn init_arm(&mut self) {
        // http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.faqs/ka3761.html
        self.init(&[(0, reg::PC, 0), (0, reg::CPSR, 0xd3)]);
    }

    /// Initializes the registers to emulate booting through BIOS, to directly
    /// start a ROM
    pub fn init_direct(&mut self) {
        self.init(
            &[
                (0, reg::PC, 0x8000000),
                (0, reg::CPSR, 0x1f),
                (0, reg::SP, 0x3007f00),
                (2, reg::SP, 0x3007fa0),
                (3, reg::SP, 0x3007fe0),
            ],
        );
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

    pub fn cycle(&mut self) -> bool {
        if self.brk.contains(&self.reg[reg::PC]) {
            warn!("Breakpoint {:#010x} hit!", self.reg[reg::PC]);
            at_breakpoint();
        }

        at_cycle();
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

    pub fn irq_enable(&self) -> bool {
        self.reg.get(0, reg::CPSR) & (1 << 7) == 0
    }

    pub fn thumb_mode(&self) -> bool {
        (self.reg[reg::CPSR] & (1u32 << cpsr::T)) != 0
    }

    pub fn get_prefetch_addr(&self) -> u32 {
        self.reg[reg::PC] + if self.thumb_mode() { 2 } else { 4 }
    }
}

// These are functions to set breakpoints on for debugging
fn at_breakpoint() {}
fn at_cycle() {}

#[cfg(test)]
pub mod test {
    use super::*;

    impl<T: MemoryUnit> Cpu<T> {
        pub fn run(&mut self) {
            let mut run = true;
            while run {
                run = self.cycle();
            }
        }

        pub fn set_thumb_mode(&mut self, thumb: bool) {
            let mask = 1u32 << cpsr::T;
            let cpsr = self.reg[reg::CPSR];
            self.reg[reg::CPSR] = (cpsr & !mask) | ((thumb as u32) * mask);
        }
    }
}

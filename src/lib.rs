#![allow(
    clippy::cognitive_complexity, // instruction decode methods are large
    clippy::many_single_char_names, // ...it's a CPU, what do you expect?
    clippy::cast_lossless, // Register types _won't_ be changed in the future
    clippy::identity_op, // there are times it makes the code line up better
    clippy::deprecated_cfg_attr,
)]

use std::collections::HashSet;
use std::default::Default;
use std::iter::IntoIterator;

use log::*;
use serde_derive::{Deserialize, Serialize};

pub mod exception;
pub mod mode;
pub mod reg;

mod arm;
mod mem;
mod thumb;
mod util;

#[cfg(test)]
mod testmod;

use self::exception::Exception;
use self::reg::*;

/// Initial registers state according to the ARM documentation.
/// http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.faqs/ka3761.html
pub const ARM_INIT: &[(usize, Reg, u32)] = &[(0, reg::PC, 0), (0, reg::CPSR, 0xd3)];

/// Initial register to emulate booting through BIOS
/// (for booting directly into GBA ROMs)
pub const GBA_INIT: &[(usize, Reg, u32)] = &[
    (0, reg::PC, 0x0800_0000),
    (0, reg::CPSR, 0x1f),
    (0, reg::SP, 0x0300_7f00),
    (2, reg::SP, 0x0300_7fa0),
    (3, reg::SP, 0x0300_7fe0),
];

/// Standard memory access trait.
/// TODO: tweak signature to support access violations / open bus behavior.
pub trait Memory {
    fn set8(&mut self, addr: u32, val: u8);
    fn set16(&mut self, addr: u32, val: u16);
    fn set32(&mut self, addr: u32, val: u32);
    fn load8(&self, addr: u32) -> u8;
    fn load16(&self, addr: u32) -> u16;
    fn load32(&self, addr: u32) -> u32;
}

/// A Emulated ARM7-TDMI CPU
///
/// TODO: add an example
#[derive(Serialize, Deserialize)]
pub struct Cpu<T: Memory> {
    /// Registers
    reg: RegFile,
    /// Memory interface
    #[serde(skip)]
    mmu: T,
    /// Breakpoints
    #[serde(skip)]
    brk: HashSet<u32>,
}

impl<T: Memory> Cpu<T> {
    /// Create a new ARM7TDMI CPU
    pub fn new<'a, I>(mmu: T, regs: I) -> Self
    where
        I: IntoIterator<Item = &'a (usize, Reg, u32)>,
    {
        let mut cpu = Cpu {
            reg: Default::default(),
            mmu,
            brk: Default::default(),
        };

        // load any custom register values
        for &(bank, reg, val) in regs.into_iter() {
            cpu.reg.set(bank, reg, val);
        }

        cpu
    }

    /// Add breakpoints at certain memory addresses
    pub fn set_breaks<'a, I>(&mut self, brks: I)
    where
        I: IntoIterator<Item = &'a u32>,
    {
        for addr in brks.into_iter() {
            self.brk.insert(*addr);
        }
    }

    /// Tick the CPU a single cycle
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

    /// Trigger an exception
    pub fn exception(&mut self, exc: Exception) {
        // this should already be pointing at the next instruction
        let new_mode = exc.mode_on_entry();
        let new_bank = new_mode.reg_bank();

        let cpsr = self.reg.get(0, reg::CPSR);
        // instruction that just executed + (2/4 depending on thumb vs arm)
        let pc = self.reg.get(0, reg::PC);

        let new_lr = match exc {
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

    /// Check if IRQs are enabled
    pub fn irq_enable(&self) -> bool {
        self.reg.get(0, reg::CPSR) & (1 << 7) == 0
    }

    /// Check if CPU is currently in Thumb mode
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

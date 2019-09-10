#![allow(
    clippy::cognitive_complexity, // instruction decode methods are large
    clippy::many_single_char_names, // ...it's a CPU, what do you expect?
    clippy::cast_lossless, // Register types _won't_ be changed in the future
    clippy::identity_op, // there are times it makes the code line up better
    clippy::deprecated_cfg_attr,
)]
#![warn(clippy::bad_bit_mask)] // TODO: remove this once warning is resolved

use std::collections::HashSet;
use std::default::Default;
use std::iter::IntoIterator;

use log::*;
use serde::{Deserialize, Serialize};

pub mod exception;
pub mod mode;
pub mod reg;

mod alignment;
mod arm;
mod thumb;
mod util;

#[cfg(test)]
mod tests;

use self::alignment::AlignmentWrapper;
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

/// Memory access trait.
/// Accesses are all Little Endian.
///
/// Provides default implementations for storing/loading 16bit and 32bit values,
/// though overrides can be provided for more efficient implementations (e.g:
/// reading from a slice / vector)
///
/// TODO: tweak signature to support access violations / open bus behavior?
pub trait Memory {
    /// Write a 8-bit `val` to `addr`
    fn w8(&mut self, addr: u32, val: u8);
    /// Read a 8-bit value from `addr`
    fn r8(&mut self, addr: u32) -> u8;
    /// Peek a 8-bit value from `addr`
    fn p8(&self, addr: u32) -> u8;

    /// Write a 16-bit `val` to `addr`
    fn w16(&mut self, addr: u32, val: u16) {
        self.w8(addr, val as u8);
        self.w8(addr + 1, (val >> 8) as u8);
    }
    /// Write a 32-bit `val` to `addr`
    fn w32(&mut self, addr: u32, val: u32) {
        self.w16(addr, val as u16);
        self.w16(addr + 2, (val >> 16) as u16);
    }

    /// Read a 16-bit value from `addr`
    fn r16(&mut self, addr: u32) -> u16 {
        self.r8(addr) as u16 | (self.r8(addr + 1) as u16) << 8
    }
    /// Read a 32-bit value from `addr`
    fn r32(&mut self, addr: u32) -> u32 {
        self.r16(addr) as u32 | (self.r16(addr + 2) as u32) << 16
    }

    /// Peek a 16-bit value from `addr`
    fn p16(&self, addr: u32) -> u16 {
        self.p8(addr) as u16 | (self.p8(addr + 1) as u16) << 8
    }
    /// Peek a 32-bit value from `addr`
    fn p32(&self, addr: u32) -> u32 {
        self.p16(addr) as u32 | (self.p16(addr + 2) as u32) << 16
    }
}

/// A Emulated ARM7-TDMI CPU
///
/// TODO: add an example
#[derive(Serialize, Deserialize)]
pub struct Cpu {
    /// Registers
    reg: RegFile,
    /// Breakpoints
    #[serde(skip)]
    brk: HashSet<u32>,
}

impl Cpu {
    /// Create a new ARM7TDMI CPU
    pub fn new<'a>(regs: impl IntoIterator<Item = &'a (usize, Reg, u32)>) -> Self {
        let mut cpu = Cpu {
            reg: Default::default(),
            brk: Default::default(),
        };

        // load any custom register values
        for &(bank, reg, val) in regs.into_iter() {
            cpu.reg.set(bank, reg, val);
        }

        cpu
    }

    /// Add breakpoints at certain memory addresses
    pub fn set_breaks<'a>(&mut self, brks: impl IntoIterator<Item = &'a u32>) {
        for addr in brks.into_iter() {
            self.brk.insert(*addr);
        }
    }

    /// Tick the CPU a single cycle
    pub fn cycle(&mut self, mmu: &mut impl Memory) -> bool {
        if self.brk.contains(&self.reg[reg::PC]) {
            warn!("Breakpoint {:#010x} hit!", self.reg[reg::PC]);
            at_breakpoint();
        }

        let mut mmu = AlignmentWrapper::new(mmu);

        at_cycle();
        if !self.thumb_mode() {
            self.execute_arm(&mut mmu)
        } else {
            self.execute_thumb(&mut mmu)
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

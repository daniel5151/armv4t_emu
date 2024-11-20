//! An emulator for the ARMv4T instruction set.
//!
//! ## Example
//!
//! ```
//! use armv4t_emu::{reg, Cpu, ExampleMem, Mode, Memory};
//!
//! let prog = &[
//!     0x06, 0x00, 0xa0, 0xe3, //    mov r0, #6
//!     0x01, 0x10, 0xa0, 0xe3, //    mov r1, #1
//!     0x01, 0x10, 0x81, 0xe0, // l: add r1, r1, r1
//!     0x01, 0x00, 0x50, 0xe2, //    subs r0, #1
//!     0xfc, 0xff, 0xff, 0x1a, //    bne l
//!     0x01, 0x6c, 0xa0, 0xe3, //    mov r6, #0x100
//!     0x00, 0x10, 0x86, 0xe5, //    str r1, [r6]
//!     0xf7, 0xf0, 0xde, 0xad  // ; trigger undefined instr exception
//! ];
//!
//! let mut mem = ExampleMem::new_with_data(prog);
//! let mut cpu = Cpu::new();
//! cpu.reg_set(Mode::User, reg::PC, 0x00);
//! cpu.reg_set(Mode::User, reg::CPSR, 0x10);
//!
//! while cpu.step(&mut mem) {}
//!
//! assert_eq!(64, mem.r32(0x100));
//! ```

#![allow(
    clippy::cognitive_complexity, // instruction decode methods are large
    clippy::many_single_char_names, // ...it's a CPU, what do you expect?
    clippy::cast_lossless, // Register types _won't_ be changed in the future
)]

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(feature = "advanced_disasm")]
use capstone::prelude::*;

pub mod reg;

mod alignment;
mod arm;
mod example_mem;
mod exception;
mod mode;
mod thumb;
mod util;

pub use crate::exception::Exception;
pub use example_mem::ExampleMem;
pub use mode::Mode;

use crate::alignment::AlignmentWrapper;
use crate::reg::*;

/// Encodes how the `Cpu` accesses external memory / memory-mapped devices.
///
/// ### Handling Memory Access Errors
///
/// At the moment, the `Memory` trait assumes that all memory operations are
/// _infallible_, and as such, doesn't support returning any sort of `Result`
/// from reads / writes. This isn't correct, as is a known-blocker for
/// implementing proper Data / Prefetch Abort support (see issue #7)
///
/// Nonetheless, there are plenty of scenarios where a memory access might
/// result in an _application_ error. For example, what if while writing to an
/// emulated UART device, a `std::io::Error` occurs?
///
/// Unfortunately, this library doesn't provide an easy solution to these
/// scenarios (yet?), but here are some possible approaches:
///
/// - Write a application-specific, fallible `Memory` trait + an adapter to
///   converts said trait into this crate's `Memory` trait.
/// - Use an "out-of-band" error signaling mechanism (e.g: a mpsc channel, or a
///   shared queue behind a mutex)
///
/// e.g: an error occurs during a read operation. The failing device signals an
/// error using a mpsc::channel, and returns a dummy value (e.g: 0x00).
/// `Cpu::step` finishes executing the instruction, and returns back to user
/// code. The user code then checks the channel to see if an error had just
/// occurred, and takes an appropriate action.
pub trait Memory {
    /// Read a 8-bit value from `addr`
    fn r8(&mut self, addr: u32) -> u8;
    /// Read a 16-bit value from `addr`
    fn r16(&mut self, addr: u32) -> u16;
    /// Read a 32-bit value from `addr`
    fn r32(&mut self, addr: u32) -> u32;

    /// Write a 8-bit `val` to `addr`
    fn w8(&mut self, addr: u32, val: u8);
    /// Write a 16-bit `val` to `addr`
    fn w16(&mut self, addr: u32, val: u16);
    /// Write a 32-bit `val` to `addr`
    fn w32(&mut self, addr: u32, val: u32);

    /// Read a 16-bit value from `addr`, instruction interface (defaults to data interface)
    fn x16(&mut self, addr: u32) -> u16 {
        self.r16(addr)
    }
    /// Read a 32-bit value from `addr`, instruction interface (defaults to data interface)
    fn x32(&mut self, addr: u32) -> u32 {
        self.r32(addr)
    }
}

/// An emulated CPU which implements the ARMv4T instruction set.
#[derive(Copy, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Cpu {
    /// Registers
    reg: RegFile,
    /// Disassembler
    #[cfg(feature = "advanced_disasm")]
    #[cfg_attr(feature = "serde", serde(skip))]
    cs: Option<Capstone>,
}

impl std::fmt::Debug for Cpu {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct("Cpu").field("reg", &self.reg).finish()
    }
}

impl Default for Cpu {
    fn default() -> Self {
        Self::new()
    }
}

impl Cpu {
    /// Construct a new ARMv4T `Cpu`, with registers set to default "cold-boot"
    /// values.
    ///
    /// Specifically, `PC` is set to `0x00000000`, and `CPSR` is set to `0xd3`
    /// (ARM state, Supervisor mode, FIQ and IRQ mask bits set). Technically,
    /// the ARM spec states that all other registers can have undefined values
    /// on-boot, but in this emulator, all registers are set to 0 on-boot.
    pub fn new() -> Cpu {
        let mut cpu = Cpu {
            reg: RegFile::new_empty(),
            #[cfg(feature = "advanced_disasm")]
            cs: Some(
                Capstone::new()
                    .arm()
                    .mode(arch::arm::ArchMode::Arm)
                    .detail(true)
                    .build()
                    .unwrap(),
            ),
        };

        cpu.reg_set(Mode::User, reg::PC, 0x00);
        cpu.reg_set(Mode::User, reg::CPSR, 0xd3);

        cpu
    }

    /// Step the CPU a single instruction with the given memory object.
    ///
    /// As a testing convenience, this method returns false if a undefined
    /// instruction exception is triggered.
    pub fn step(&mut self, mem: &mut impl Memory) -> bool {
        let mut mem = AlignmentWrapper::new(mem);

        let decode_ok = if !self.thumb_mode() {
            self.execute_arm(&mut mem)
        } else {
            self.execute_thumb(&mut mem)
        };

        if !decode_ok {
            self.exception(Exception::Undefined)
        }

        decode_ok
    }

    /// Trigger a CPU exception.
    pub fn exception(&mut self, exc: Exception) {
        match exc {
            Exception::Interrupt => {
                if !self.irq_enable() {
                    return;
                }
            }
            Exception::FastInterrupt => {
                if !self.fiq_enable() {
                    return;
                }
            }
            _ => (),
        }
        // this should already be pointing at the next instruction
        let new_mode = exc.mode_on_entry();
        let new_bank = new_mode.reg_bank();

        let cpsr = self.reg.get(0, reg::CPSR);
        // instruction that just executed + (2/4 depending on thumb vs arm)
        let pc = self.reg.get(0, reg::PC);

        let new_lr = match exc {
            Exception::Interrupt => pc + 4,
            Exception::FastInterrupt => pc + 4,
            _ => pc,
        };

        self.reg.set(new_bank, reg::LR, new_lr);
        self.reg.set(new_bank, reg::SPSR, cpsr);

        self.reg.set(0, reg::PC, exc.address());
        #[allow(clippy::identity_op)]
        let new_cpsr =
            (new_mode.bits() as u32) |
            (0 << 5) /* ARM mode */ |
            ((exc.fiq_disable() as u32) << 6) |
            (1 << 7) /* IRQ disable */ |
            (cpsr & (0xf << 28)) /* condition flags */;
        self.reg.set(0, reg::CPSR, new_cpsr);
    }

    /// Check if CPU is currently in Thumb mode.
    pub fn thumb_mode(&self) -> bool {
        (self.reg[reg::CPSR] & (1u32 << cpsr::T)) != 0
    }

    /// Manually set a register's value.
    pub fn reg_set(&mut self, mode: Mode, reg: Reg, val: u32) {
        self.reg.set(mode.reg_bank(), reg, val)
    }

    /// Returns a register's value.
    pub fn reg_get(&self, mode: Mode, reg: Reg) -> u32 {
        self.reg.get(mode.reg_bank(), reg)
    }

    /// Returns the current processor mode.
    pub fn mode(&self) -> Mode {
        self.reg.mode()
    }

    /// Check if IRQs are enabled.
    pub fn irq_enable(&self) -> bool {
        self.reg.get(0, reg::CPSR) & (1 << 7) == 0
    }

    /// Check if FIQs are enabled.
    pub fn fiq_enable(&self) -> bool {
        self.reg.get(0, reg::CPSR) & (1 << 6) == 0
    }
}

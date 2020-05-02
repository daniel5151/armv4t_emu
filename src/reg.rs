//! Register identifiers.

use std::ops::{Index, IndexMut};

use log::*;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::mode::Mode;
use crate::util::bit::BitUtilExt;

pub type Reg = u8;

const NUM_RGSR: usize = 37;

/// Stack Pointer (R13)
pub const SP: Reg = 13;
/// Link Register (R14)
pub const LR: Reg = 14;
/// Program Counter (R15)
pub const PC: Reg = 15;
/// Current Program Status Register
pub const CPSR: Reg = 16;
/// Saved Program Status Register
pub const SPSR: Reg = 17;

pub(crate) mod cpsr {
    use super::Reg;

    pub const N: Reg = 31;
    pub const Z: Reg = 30;
    pub const C: Reg = 29;
    pub const V: Reg = 28;

    pub const T: Reg = 5;
}

#[rustfmt::skip]
const REG_MAP: [[usize; 18]; 6] = [
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 16],   // user
    [0, 1, 2, 3, 4, 5, 6, 7, 17, 18, 19, 20, 21, 22, 23, 15, 16, 24], // fiq
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 25, 26, 15, 16, 27],   // irq
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 28, 29, 15, 16, 30],   // supervisor
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 31, 32, 15, 16, 33],   // abort
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 34, 35, 15, 16, 36],   // undefined
];

#[cfg(feature = "serde")]
mod big_array {
    use serde_big_array::big_array;
    big_array! { BigArray; +super::NUM_RGSR }
}

#[derive(Copy, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub(crate) struct RegFile {
    #[cfg_attr(feature = "serde", serde(with = "big_array::BigArray"))]
    reg: [u32; NUM_RGSR],
    bank: usize,
}

impl PartialEq for RegFile {
    fn eq(&self, other: &Self) -> bool {
        self.reg[..] == other.reg[..] && self.bank == other.bank
    }
}

impl Eq for RegFile {}

// This is pretty jank, due to the way registers are stored
// It could use some improvement.
impl std::fmt::Debug for RegFile {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut builder = fmt.debug_struct("RegFile");
        builder.field("cur_bank", &self.bank);
        for (bank, map) in REG_MAP.iter().enumerate() {
            let mut regs = Vec::new();
            for (i, reg) in map.iter().copied().enumerate() {
                match i as u8 {
                    SP => regs.push(("SP".to_string(), self.reg[reg])),
                    LR => regs.push(("LR".to_string(), self.reg[reg])),
                    PC => regs.push(("PC".to_string(), self.reg[reg])),
                    CPSR => regs.push(("CPSR".to_string(), self.reg[reg])),
                    SPSR => regs.push(("SPSR".to_string(), self.reg[reg])),
                    _ => regs.push((format!("r{}", i), self.reg[reg])),
                };
            }
            builder.field(
                match bank {
                    0 => "user       ",
                    1 => "fiq        ",
                    2 => "irq        ",
                    3 => "supervisor ",
                    4 => "abort      ",
                    5 => "undefined  ",
                    _ => unreachable!(),
                },
                &format!("{:08x?}", regs).replace("\"", ""),
            );
        }
        builder.finish()
    }
}

impl RegFile {
    pub fn new_empty() -> RegFile {
        RegFile {
            reg: [0; NUM_RGSR],
            bank: 0,
        }
    }

    #[inline]
    pub fn mode(&self) -> Mode {
        // `expect` should never be fired, as mode bits are checked to be valid when
        // setting the CPSR value.
        Mode::from_bits(self.reg[CPSR as usize].extract(0, 5) as u8)
            .expect("CPSR contained invalid mode bits")
    }

    #[inline]
    pub fn update_bank(&mut self) {
        self.bank = self.mode().reg_bank();
    }

    #[inline]
    pub fn set(&mut self, bank: usize, reg: Reg, mut val: u32) {
        if reg == CPSR {
            let bits = val.extract(0, 5) as u8;
            let mode = Mode::from_bits(bits);
            if mode.is_none() {
                // Switching to an invalid mode leads to unpredictable behavior.
                //
                // Panicking here would be unnecessarily harsh, as the error is
                // originating from emulated code, which the end-user might not
                // have written themselves.
                //
                // Instead, we take a page out of QEMU's book and simply leave
                // the mode bits unchanged, while logging an error.
                error!(
                    "Attempted to write to CPSR with invalid mode bits: {:#x}",
                    bits
                );

                let oldval = self.reg[CPSR as usize];
                val = (val & !0x1f) | (oldval & 0x1f);
            }
        }

        self.reg[REG_MAP[bank][reg as usize]] = val;
        if reg == CPSR {
            self.update_bank()
        }
    }

    #[inline]
    pub fn get(&self, bank: usize, reg: Reg) -> u32 {
        self.reg[REG_MAP[bank][reg as usize]]
    }
}

impl Index<Reg> for RegFile {
    type Output = u32;
    #[inline]
    fn index(&self, idx: Reg) -> &u32 {
        &self.reg[REG_MAP[self.bank][idx as usize]]
    }
}

impl IndexMut<Reg> for RegFile {
    #[inline]
    fn index_mut(&mut self, idx: Reg) -> &mut u32 {
        &mut self.reg[REG_MAP[self.bank][idx as usize]]
    }
}

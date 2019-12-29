use std::default::Default;
use std::ops::{Index, IndexMut};

#[cfg(feature = "serde")]
use serde::{
    de::{self, SeqAccess, Visitor},
    ser::SerializeTuple,
    Deserialize, Deserializer, Serialize, Serializer,
};

use crate::mode::Mode;
use crate::util::bit::BitUtilExt;

pub type Reg = u8;

const NUM_RGSR: usize = 37;

pub const SP: Reg = 13;
pub const LR: Reg = 14;
pub const PC: Reg = 15;
pub const CPSR: Reg = 16;
pub const SPSR: Reg = 17;

#[cfg_attr(rustfmt, rustfmt_skip)]
const REG_MAP: [[usize; 18]; 6] = [
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 16],   // user
    [0, 1, 2, 3, 4, 5, 6, 7, 17, 18, 19, 20, 21, 22, 23, 15, 16, 24], // fiq
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 25, 26, 15, 16, 27],   // irq
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 28, 29, 15, 16, 30],   // supervisor
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 31, 32, 15, 16, 33],   // abort
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 34, 35, 15, 16, 36],   // undefined
];

pub struct RegFile {
    reg: [u32; NUM_RGSR],
    bank: usize,
}

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
    #[inline]
    pub fn mode(&self) -> Mode {
        Mode::from_bits(self.reg[CPSR as usize].extract(0, 5) as u8)
    }

    #[inline]
    pub fn update_bank(&mut self) {
        self.bank = self.mode().reg_bank();
    }

    #[inline]
    pub fn set(&mut self, bank: usize, reg: Reg, val: u32) {
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

impl Default for RegFile {
    fn default() -> RegFile {
        RegFile {
            reg: [0; NUM_RGSR],
            bank: 0,
        }
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

#[cfg(feature = "serde")]
impl Serialize for RegFile {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut seq = serializer.serialize_tuple(NUM_RGSR)?;
        for r in self.reg.iter() {
            seq.serialize_element(r)?;
        }
        seq.end()
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for RegFile {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct RegVisitor;
        impl<'de> Visitor<'de> for RegVisitor {
            type Value = RegFile;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("tuple RegFile")
            }

            fn visit_seq<V: SeqAccess<'de>>(self, mut seq: V) -> Result<RegFile, V::Error> {
                let mut reg = RegFile {
                    reg: [0; NUM_RGSR],
                    bank: 0,
                };
                for i in 0..NUM_RGSR {
                    reg.reg[i] = seq
                        .next_element()?
                        .ok_or_else(|| de::Error::invalid_length(i, &self))?;
                }
                reg.update_bank();

                Ok(reg)
            }
        }

        deserializer.deserialize_tuple(NUM_RGSR, RegVisitor)
    }
}

pub mod cpsr {
    use super::Reg;

    pub const N: Reg = 31;
    pub const Z: Reg = 30;
    pub const C: Reg = 29;
    pub const V: Reg = 28;

    pub const T: Reg = 5;
}

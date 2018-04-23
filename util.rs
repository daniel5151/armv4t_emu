use bit_util::*;

use super::reg::cpsr;

/// Compute the shifted value and shift carry when shift != 0
#[inline]
pub fn arg_shift(val: u32, shift: u32, shift_type: u32) -> (u32, u32) {
    debug_assert!(shift != 0);
    match shift_type {
        0 => shift_lsl(val, shift),
        1 => shift_lsr(val, shift),
        2 => shift_asr(val, shift),
        3 => shift_ror(val, shift),
        _ => unreachable!(),
    }
}

/// Compute the shifted value and shift carry when shift == 0
/// ARM has special logic encoded for when shift is 0, which requires
/// the previous carry in some cases
#[inline]
pub fn arg_shift0(val: u32, shift_type: u32, c: u32) -> (u32, u32) {
    match shift_type {
        0 /* LSL */ => (val, c),
        1 /* LSR */ => shift_lsr(val, 32),
        2 /* ASR */ => shift_asr(val, 32),
        3 /* ROR */ => {
            // in this case its RRX#1
            // so we rotate right by one and shift the
            // carry bit in
            ((val >> 1) | (c << 31),
             bit(val, 0))
        },
        _ => unreachable!(),
    }
}

#[inline]
pub fn build_flags(v: u32, c: u32, z: u32, n: u32) -> u32 {
    (v & 1) << 0 | (c & 1) << 1 | (z & 1) << 2 | (n & 1) << 3
}

pub fn cond_met(cond: u32, cpsr: u32) -> bool {
    let z = bit(cpsr, cpsr::Z);
    let c = bit(cpsr, cpsr::C);
    let v = bit(cpsr, cpsr::V);
    let n = bit(cpsr, cpsr::N);

    match cond {
        0x0 /* EQ */ => z == 1,
        0x1 /* NE */ => z == 0,
        0x2 /* CS */ => c == 1,
        0x3 /* CC */ => c == 0,
        0x4 /* MI */ => n == 1,
        0x5 /* PL */ => n == 0,
        0x6 /* VS */ => v == 1,
        0x7 /* VC */ => v == 0,
        0x8 /* HI */ => c == 1 && z == 0,
        0x9 /* LS */ => c == 0 || z == 1,
        0xA /* GE */ => n == v,
        0xB /* LT */ => n != v,
        0xC /* GT */ => z == 0 && n == v,
        0xD /* LE */ => z == 1 || n != v,
        0xE /* AL */ => true,
        0xF /*    */ => true, /* reserved, default to execute */
        _ => unreachable!(),
    }
}

pub mod bit {
    #[inline]
    pub fn extract(val: u32, off: u8, len: u8) -> u32 {
        debug_assert!(off < 32 && len < 32);
        (val >> off) & ((1u32 << len) - 1)
    }

    #[inline]
    pub fn mask_match(val: u32, mask: u32, test: u32) -> bool {
        ((val ^ test) & mask) == 0
    }

    #[inline]
    pub fn set(base: u32, off: u8, len: u8, val: u32) -> u32 {
        debug_assert!(off < 32 && len < 32);
        let mask = ((1u32 << len) - 1) << off;
        ((std::u32::MAX - mask) & base) | ((val << off) & mask)
    }

    #[inline]
    pub fn bit(val: u32, bit: u8) -> u32 {
        debug_assert!(bit < 32);
        (val >> bit) & 1
    }

    #[inline]
    pub fn sign_extend(val: u32, len: u8) -> u32 {
        debug_assert!(len < 32);
        let off = 32 - len;
        (((val as i32) << off) >> off) as u32
    }

    // These functions return the shifted values as well as the carry bit
    #[inline]
    pub fn shift_lsl(val: u32, rot: u32) -> (u32, u32) {
        match rot {
            _ if rot < 32 => (val << rot, bit(val, 32 - rot as u8)),
            _ if rot == 32 => (0, bit(val, 0)),
            _ => (0, 0),
        }
    }

    #[inline]
    pub fn shift_lsr(val: u32, rot: u32) -> (u32, u32) {
        match rot {
            _ if rot == 0 => (val, 0),
            _ if rot < 32 => (val >> rot, bit(val, rot as u8 - 1)),
            _ if rot == 32 => (0, bit(val, 31)),
            _ => (0, 0),
        }
    }

    #[inline]
    pub fn shift_asr(val: u32, rot: u32) -> (u32, u32) {
        match rot {
            _ if rot == 0 => (val, 0),
            _ if rot < 32 => (((val as i32) >> rot) as u32, bit(val, rot as u8 - 1)),
            _ => (((val as i32) >> 31) as u32, bit(val, 31)),
        }
    }

    #[inline]
    pub fn shift_ror(val: u32, rot: u32) -> (u32, u32) {
        match rot {
            _ if rot == 0 => (val, 0),
            _ => (val.rotate_right(rot), bit(val, (rot as u8 - 1) % 32)),
        }
    }

    #[inline]
    pub fn is_pos(val: u32) -> bool {
        (val as i32) >= 0
    }

    #[inline]
    pub fn is_neg(val: u32) -> bool {
        (val as i32) < 0
    }

    /// Performs addition and returns overflow and carry bits
    #[inline]
    pub fn add_flags(lhs: u32, rhs: u32, carry: u32) -> (u32, u32, u32) {
        // Logic copied from VisualBoyAdvance
        let res = lhs.wrapping_add(rhs).wrapping_add(carry);

        #[cfg_attr(rustfmt, rustfmt_skip)]
        (res,
         ((is_neg(lhs) && is_neg(rhs) && is_pos(res)) ||
          (is_pos(lhs) && is_pos(rhs) && is_neg(res))) as u32,
         ((is_neg(lhs) && is_neg(rhs)) ||
          (is_neg(lhs) && is_pos(res)) ||
          (is_neg(rhs) && is_pos(res))) as u32,
        )
    }

    /// Performs subtraction and returns overflow and carry bits
    #[inline]
    pub fn sub_flags(lhs: u32, rhs: u32, carry: u32) -> (u32, u32, u32) {
        // Logic copied from VisualBoyAdvance
        let res = lhs.wrapping_sub(rhs).wrapping_sub(carry);

        #[cfg_attr(rustfmt, rustfmt_skip)]
        (res,
         ((is_neg(lhs) && is_pos(rhs) && is_pos(res)) ||
          (is_pos(lhs) && is_neg(rhs) && is_neg(res))) as u32,
         ((is_neg(lhs) && is_pos(rhs)) ||
          (is_neg(lhs) && is_pos(res)) ||
          (is_pos(rhs) && is_pos(res))) as u32,
        )
    }

    /// Combines two 32 bit words into a 64 bit word
    #[inline]
    pub fn combine64(hi: u32, lo: u32) -> u64 {
        ((hi as u64) << 32) | (lo as u64)
    }

    /// Splits a 64 bit word into two 32 bit words
    /// The return value is a tuple of (hi, lo)
    #[inline]
    pub fn split64(quad: u64) -> (u32, u32) {
        ((quad >> 32) as u32, quad as u32)
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn signed_conversions() {
            let val = 0xf0000000u32;
            let sval = val as i32;
            assert_eq!(0xff000000u32, (sval >> 4) as u32);
            assert_eq!(val, sval as u32);
            assert_eq!(0xf000u16 as i16 as u32, 0xfffff000u32);
        }

        #[test]
        fn test_overrotate() {
            let val = 0xf000000u32;
            assert_eq!(val.rotate_right(68), 0x0f00000u32);
        }

        #[test]
        fn test_shifts() {
            assert_eq!(shift_lsl(0x11000000, 4), (0x10000000, 1));
            assert_eq!(shift_lsl(0x11000000, 5), (0x20000000, 0));
            assert_eq!(shift_lsr(0x11, 1), (0x8, 1));
            assert_eq!(shift_lsr(0x11, 2), (0x4, 0));
            assert_eq!(shift_ror(0x11, 1), (0x80000008, 1));
        }

        #[test]
        fn test_sign_extend() {
            assert_eq!(0xfffffff0, sign_extend(0xf0, 8));
            assert_eq!(0x00000070, sign_extend(0x70, 8));
        }
    }
}

pub mod arm {
    use super::bit::*;
    use crate::reg::cpsr;

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

    #[inline]
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
}

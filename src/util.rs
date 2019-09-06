pub mod bit {
    /// Collection of useful bit manipulation methods (sepcifically for u32)
    pub trait BitUtilExt {
        fn extract(self, off: u8, len: u8) -> u32;
        fn mask_match(self, mask: u32, test: u32) -> bool;
        fn set_bit(self, off: u8, len: u8, val: u32) -> u32;
        fn get_bit(self, bit: u8) -> u32;
        fn sign_extend(self, len: u8) -> u32;
        /// Returns the shifted value as well as the carry bit
        fn shift_lsl(self, rot: u32) -> (u32, u32);
        /// Returns the shifted value as well as the carry bit
        fn shift_lsr(self, rot: u32) -> (u32, u32);
        /// Returns the shifted value as well as the carry bit
        fn shift_asr(self, rot: u32) -> (u32, u32);
        /// Returns the shifted value as well as the carry bit
        fn shift_ror(self, rot: u32) -> (u32, u32);
        fn is_pos(self) -> bool;
        fn is_neg(self) -> bool;
        /// Performs addition and returns overflow and carry bits
        fn add_flags(self, rhs: u32, carry: u32) -> (u32, u32, u32);
        /// Performs subtraction and returns overflow and carry bits
        fn sub_flags(self, rhs: u32, carry: u32) -> (u32, u32, u32);
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

    /// Collection of useful bit manipulation methods for u32
    impl BitUtilExt for u32 {
        #[inline]
        fn extract(self, off: u8, len: u8) -> u32 {
            debug_assert!(off < 32 && len < 32);
            (self >> off) & ((1u32 << len) - 1)
        }

        #[inline]
        fn mask_match(self, mask: u32, test: u32) -> bool {
            ((self ^ test) & mask) == 0
        }

        #[inline]
        fn set_bit(self, off: u8, len: u8, val: u32) -> u32 {
            debug_assert!(off < 32 && len < 32);
            let mask = ((1u32 << len) - 1) << off;
            ((std::u32::MAX - mask) & self) | ((val << off) & mask)
        }

        #[inline]
        fn get_bit(self, bit: u8) -> u32 {
            debug_assert!(bit < 32);
            (self >> bit) & 1
        }

        #[inline]
        fn sign_extend(self, len: u8) -> u32 {
            debug_assert!(len < 32);
            let off = 32 - len;
            (((self as i32) << off) >> off) as u32
        }

        #[inline]
        fn shift_lsl(self, rot: u32) -> (u32, u32) {
            match rot {
                _ if rot < 32 => (self << rot, u32::get_bit(self, 32 - rot as u8)),
                _ if rot == 32 => (0, u32::get_bit(self, 0)),
                _ => (0, 0),
            }
        }

        #[inline]
        fn shift_lsr(self, rot: u32) -> (u32, u32) {
            match rot {
                _ if rot == 0 => (self, 0),
                _ if rot < 32 => (self >> rot, u32::get_bit(self, rot as u8 - 1)),
                _ if rot == 32 => (0, u32::get_bit(self, 31)),
                _ => (0, 0),
            }
        }

        #[inline]
        fn shift_asr(self, rot: u32) -> (u32, u32) {
            match rot {
                _ if rot == 0 => (self, 0),
                _ if rot < 32 => (
                    ((self as i32) >> rot) as u32,
                    u32::get_bit(self, rot as u8 - 1),
                ),
                _ => (((self as i32) >> 31) as u32, u32::get_bit(self, 31)),
            }
        }

        #[inline]
        fn shift_ror(self, rot: u32) -> (u32, u32) {
            match rot {
                _ if rot == 0 => (self, 0),
                _ => (
                    self.rotate_right(rot),
                    u32::get_bit(self, (rot as u8 - 1) % 32),
                ),
            }
        }

        #[inline]
        fn is_pos(self) -> bool {
            (self as i32) >= 0
        }

        #[inline]
        fn is_neg(self) -> bool {
            (self as i32) < 0
        }

        #[inline]
        fn add_flags(self, rhs: u32, carry: u32) -> (u32, u32, u32) {
            let lhs = self;
            // Logic copied from VisualBoyAdvance
            let res = lhs.wrapping_add(rhs).wrapping_add(carry);

            #[cfg_attr(rustfmt, rustfmt_skip)]
                (res,
                 ((u32::is_neg(lhs) && u32::is_neg(rhs) && u32::is_pos(res)) ||
                  (u32::is_pos(lhs) && u32::is_pos(rhs) && u32::is_neg(res))) as u32,
                 ((u32::is_neg(lhs) && u32::is_neg(rhs)) ||
                  (u32::is_neg(lhs) && u32::is_pos(res)) ||
                  (u32::is_neg(rhs) && u32::is_pos(res))) as u32,
                )
        }

        #[inline]
        fn sub_flags(self, rhs: u32, carry: u32) -> (u32, u32, u32) {
            let lhs = self;
            // Logic copied from VisualBoyAdvance
            let res = lhs.wrapping_sub(rhs).wrapping_sub(carry);

            #[cfg_attr(rustfmt, rustfmt_skip)]
                (res,
                 ((u32::is_neg(lhs) && u32::is_pos(rhs) && u32::is_pos(res)) ||
                  (u32::is_pos(lhs) && u32::is_neg(rhs) && u32::is_neg(res))) as u32,
                 ((u32::is_neg(lhs) && u32::is_pos(rhs)) ||
                  (u32::is_neg(lhs) && u32::is_pos(res)) ||
                  (u32::is_pos(rhs) && u32::is_pos(res))) as u32,
                )
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn signed_conversions() {
            let val = 0xf000_0000u32;
            let sval = val as i32;
            assert_eq!(0xff00_0000u32, (sval >> 4) as u32);
            assert_eq!(val, sval as u32);
            assert_eq!(0xf000u16 as i16 as u32, 0xffff_f000u32);
        }

        #[test]
        fn test_overrotate() {
            let val = 0x0f00_0000u32;
            assert_eq!(val.rotate_right(68), 0x00f0_0000u32);
        }

        #[test]
        fn test_shifts() {
            assert_eq!(u32::shift_lsl(0x1100_0000, 4), (0x1000_0000, 1));
            assert_eq!(u32::shift_lsl(0x1100_0000, 5), (0x2000_0000, 0));
            assert_eq!(u32::shift_lsr(0x11, 1), (0x8, 1));
            assert_eq!(u32::shift_lsr(0x11, 2), (0x4, 0));
            assert_eq!(u32::shift_ror(0x11, 1), (0x8000_0008, 1));
        }

        #[test]
        fn test_sign_extend() {
            assert_eq!(0xffff_fff0, u32::sign_extend(0xf0, 8));
            assert_eq!(0x0000_0070, u32::sign_extend(0x70, 8));
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
            0 => val.shift_lsl(shift),
            1 => val.shift_lsr(shift),
            2 => val.shift_asr(shift),
            3 => val.shift_ror(shift),
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
        1 /* LSR */ => val.shift_lsr(32),
        2 /* ASR */ => val.shift_asr(32),
        3 /* ROR */ => {
            // in this case its RRX#1
            // so we rotate right by one and shift the
            // carry bit in
            ((val >> 1) | (c << 31), val.get_bit(0))
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
        let z = cpsr.get_bit(cpsr::Z);
        let c = cpsr.get_bit(cpsr::C);
        let v = cpsr.get_bit(cpsr::V);
        let n = cpsr.get_bit(cpsr::N);

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

use std;

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
     ((is_neg(lhs) && is_neg(rhs)) ||
      (is_neg(lhs) && is_pos(res)) ||
      (is_neg(rhs) && is_pos(res))) as u32,
     ((is_neg(lhs) && is_neg(rhs) && is_pos(res)) ||
      (is_pos(lhs) && is_pos(rhs) && is_neg(res))) as u32)
}

/// Performs subtraction and returns overflow and carry bits
#[inline]
pub fn sub_flags(lhs: u32, rhs: u32, carry: u32) -> (u32, u32, u32) {
    // Logic copied from VisualBoyAdvance
    let res = lhs.wrapping_sub(rhs).wrapping_sub(carry);

    #[cfg_attr(rustfmt, rustfmt_skip)]
    (res,
     ((is_neg(lhs) && is_pos(rhs)) ||
      (is_neg(lhs) && is_pos(res)) ||
      (is_pos(rhs) && is_pos(res))) as u32,
     ((is_neg(lhs) && is_pos(rhs) && is_pos(res)) ||
      (is_pos(lhs) && is_neg(rhs) && is_neg(res))) as u32)
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
    }

    #[test]
    fn overrotate() {
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
}

use crate::{Cpu, Memory};

impl<T: Memory> Cpu<T> {
    // FIXME: other sizes should likely also have
    // centralized behaviour for unaligned access
    pub(super) fn r32(&mut self, addr: u32) -> u32 {
        let a = addr & !3;

        let val = self.mmu.r32(a);
        if a == addr {
            val
        } else {
            let shift = (addr & 3) * 8;
            val.rotate_right(shift)
        }
    }

    pub(super) fn w32(&mut self, addr: u32, val: u32) {
        self.mmu.w32(addr & !3, val);
    }
}

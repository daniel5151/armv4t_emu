use crate::Memory;

/// Thin wrapper around a memory interface that implements correct ARM
/// misaligned access behavior
pub struct AlignmentWrapper<'a, T: Memory>(&'a mut T);

impl<T: Memory> AlignmentWrapper<'_, T> {
    pub fn new(mmu: &mut T) -> AlignmentWrapper<'_, T> {
        AlignmentWrapper(mmu)
    }
}

impl<T: Memory> Memory for AlignmentWrapper<'_, T> {
    fn r8(&mut self, addr: u32) -> u8 {
        // TODO: fixme
        self.0.r8(addr)
    }

    fn r16(&mut self, addr: u32) -> u16 {
        // TODO: fixme
        self.0.r16(addr)
    }

    fn r32(&mut self, addr: u32) -> u32 {
        let a = addr & !3;

        let val = self.0.r32(a);
        if a == addr {
            val
        } else {
            let shift = (addr & 3) * 8;
            val.rotate_right(shift)
        }
    }

    fn w8(&mut self, addr: u32, val: u8) {
        // TODO: fixme
        self.0.w8(addr, val)
    }

    fn w16(&mut self, addr: u32, val: u16) {
        // TODO: fixme
        self.0.w16(addr, val)
    }

    fn w32(&mut self, addr: u32, val: u32) {
        self.0.w32(addr & !3, val);
    }

    fn x16(&mut self, addr: u32) -> u16 {
        self.0.x16(addr)
    }

    fn x32(&mut self, addr: u32) -> u32 {
        let a = addr & !3;

        let val = self.0.x32(a);
        if a == addr {
            val
        } else {
            let shift = (addr & 3) * 8;
            val.rotate_right(shift)
        }
    }
}

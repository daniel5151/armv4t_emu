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
        self.0.r8(addr)
    }
    fn p8(&self, addr: u32) -> u8 {
        self.0.p8(addr)
    }
    fn w8(&mut self, addr: u32, val: u8) {
        self.0.w8(addr, val)
    }

    // TODO: implement correct halfword behavior

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

    fn w32(&mut self, addr: u32, val: u32) {
        self.0.w32(addr & !3, val);
    }
}

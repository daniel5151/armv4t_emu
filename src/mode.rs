/// An ARMv4T processor mode.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Mode {
    User,
    Fiq,
    Irq,
    Supervisor,
    Abort,
    Undefined,
    System,
}

impl Mode {
    #[inline]
    pub(crate) fn from_bits(mode: u8) -> Option<Self> {
        use self::Mode::*;
        Some(match mode & 0x1f {
            0x10 => User,
            0x11 => Fiq,
            0x12 => Irq,
            0x13 => Supervisor,
            0x17 => Abort,
            0x1b => Undefined,
            0x1f => System,
            _ => return None,
        })
    }

    #[inline]
    pub(crate) fn bits(self) -> u8 {
        use self::Mode::*;
        match self {
            User => 0x10,
            Fiq => 0x11,
            Irq => 0x12,
            Supervisor => 0x13,
            Abort => 0x17,
            Undefined => 0x1b,
            System => 0x1f,
        }
    }

    #[inline]
    pub(crate) fn reg_bank(self) -> usize {
        use self::Mode::*;
        match self {
            User => 0,
            Fiq => 1,
            Irq => 2,
            Supervisor => 3,
            Abort => 4,
            Undefined => 5,
            System => 0,
        }
    }
}

use log::*;

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
    pub fn from_bits(mode: u8) -> Self {
        use self::Mode::*;
        match mode & 0x1f {
            0x10 => User,
            0x11 => Fiq,
            0x12 => Irq,
            0x13 => Supervisor,
            0x17 => Abort,
            0x1b => Undefined,
            0x1f => System,
            _ => {
                warn!("Invalid mode bits: {:#x}", mode);
                User
            }
        }
    }

    #[inline]
    pub fn bits(self) -> u8 {
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
    pub fn reg_bank(self) -> usize {
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

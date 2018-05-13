use super::mode::Mode;

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Exception {
    Reset,
    Undefined,
    Software,
    PreAbort,
    DataAbort,
    Addr26Bit,
    Interrupt,
    FastInterrupt,
}

impl Exception {
    #[inline]
    pub fn mode_on_entry(self) -> Mode {
        use self::Exception::*;
        match self {
            Reset => Mode::Supervisor,
            Undefined => Mode::Undefined,
            Software => Mode::Supervisor,
            PreAbort => Mode::Abort,
            DataAbort => Mode::Abort,
            Addr26Bit => Mode::Supervisor,
            Interrupt => Mode::Irq,
            FastInterrupt => Mode::Fiq,
        }
    }

    #[inline]
    pub fn address(self) -> u32 {
        use self::Exception::*;
        match self {
            Reset => 0x00,
            Undefined => 0x04,
            Software => 0x08,
            PreAbort => 0x0c,
            DataAbort => 0x10,
            Addr26Bit => 0x14,
            Interrupt => 0x18,
            FastInterrupt => 0x1c,
        }
    }

    #[inline]
    pub fn fiq_disable(self) -> bool {
        use self::Exception::*;
        match self {
            FastInterrupt | Reset => true,
            _ => false,
        }
    }
}

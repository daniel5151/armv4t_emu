use crate::mode::Mode;

/// An ARMv4T processor exception.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Exception {
    Reset,
    Undefined,
    Software,
    PreAbort,
    DataAbort,
    Interrupt,
    FastInterrupt,
}

impl Exception {
    /// Returns the ARM processor mode that's switched to upon handling the
    /// exception.
    #[inline]
    pub fn mode_on_entry(self) -> Mode {
        use self::Exception::*;
        match self {
            Reset => Mode::Supervisor,
            Undefined => Mode::Undefined,
            Software => Mode::Supervisor,
            PreAbort => Mode::Abort,
            DataAbort => Mode::Abort,
            Interrupt => Mode::Irq,
            FastInterrupt => Mode::Fiq,
        }
    }

    /// Returns the address of the exception's Vector Table entry.
    #[inline]
    pub fn address(self) -> u32 {
        use self::Exception::*;
        match self {
            Reset => 0x00,
            Undefined => 0x04,
            Software => 0x08,
            PreAbort => 0x0c,
            DataAbort => 0x10,
            Interrupt => 0x18,
            FastInterrupt => 0x1c,
        }
    }

    #[inline]
    pub(crate) fn fiq_disable(self) -> bool {
        use self::Exception::*;
        match self {
            FastInterrupt | Reset => true,
            _ => false,
        }
    }
}

use enumflags2_derive::EnumFlags;

#[derive(Copy, Clone, Debug, EnumFlags, Eq, PartialEq)]
#[repr(u8)]
pub enum Interrupt {
    VBlank = 1 << 0,
    Lcd    = 1 << 1,
    Timer  = 1 << 2,
    Serial = 1 << 3,
    Joypad = 1 << 4,
}

impl Interrupt {
    /// Get the memory address of the code which handles this interrupt.
    pub fn handler_addr(self) -> u16 {
        match self {
            Interrupt::VBlank => 0x0040,
            Interrupt::Lcd    => 0x0048,
            Interrupt::Timer  => 0x0050,
            Interrupt::Serial => 0x0058,
            Interrupt::Joypad => 0x0060,
        }
    }
}